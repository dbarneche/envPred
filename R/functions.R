#' Environmental predictability components
#'
#' @title Calculates environmental predictability components
#' @param rawTimeSeries A \code{\link[base]{numeric}} vector containing 
#' a raw environmental time series.
#' @param datesVector An object of class \code{\link[base]{Date}} of format YYYY-MM-DD.
#' @param delta Time interval (any unit) of \code{rawTimeSeries}.
#' @param isUneven Is \code{rawTimeSeries} even or unevenly distributed in time? Default is FALSE.
#' @param interpolate Should a linear interpolation be applied to missing values in 
#' \code{rawTimeSeries}? Irrelevant method if \code{isUneven} is FALSE. Default is FALSE.
#' @param checkPlots Should inspection plots be triggered? Default is FALSE. Not recommended 
#' if applying this task to multiple time series at once.
#' @param seasonalityMethod A method for estimating seasonality. Takes 3 possible 
#' values: \code{'absolute'}, \code{'unbounded'}, or \code{'bounded'} (default).
#' @param noiseMethod A method for estimating the slope beta. Takes 2 possible 
#' values: \code{'spectrum'} for evenly distributed time series or 
#' \code{'LombScargle'} for unevenly distributed ones.
#' @details This function currently allows for monthly seasonality calculation only. 
#' This algorithm adapts the steps described in \href{http://onlinelibrary.wiley.com/doi/10.1111/ele.12402/abstract}{Marshall and Burgess 2015} Ecology Letters 18: 1461–0248, doi: \href{http://dx.doi.org/10.1111/ele.12402}{10.1111/ele.12402}.
#' @return A \code{\link[base]{data.frame}} with environmental predictability components.
#' @author Diego Barneche and Scott Burgess.
#' @seealso \code{\link[envPred]{envSeasonality}}, \code{\link[envPred]{envNoise}}.
#' @examples
#' library(envPred)
#' data(sst)
#' envPredictability(sst$rawTimeSeries, sst$datesVector, delta = 1, isUneven = FALSE, interpolate = FALSE, checkPlots = FALSE, seasonalityMethod = 'absolute', noiseMethod = 'spectrum')
#' envPredictability(sst$rawTimeSeries, sst$datesVector, delta = 1, isUneven = FALSE, interpolate = FALSE, checkPlots = FALSE, seasonalityMethod = 'unbounded', noiseMethod = 'spectrum')
#' envPredictability(sst$rawTimeSeries, sst$datesVector, delta = 1, isUneven = FALSE, interpolate = FALSE, checkPlots = FALSE, seasonalityMethod = 'bounded', noiseMethod = 'spectrum')
#' data(npp)
#' envPredictability(npp$rawTimeSeries, npp$datesVector, delta = 8, isUneven = TRUE, interpolate = FALSE, checkPlots = TRUE, seasonalityMethod = 'unbounded', noiseMethod = 'LombScargle')
#' envPredictability(npp$rawTimeSeries, npp$datesVector, delta = 8, isUneven = TRUE, interpolate = TRUE, checkPlots = TRUE, seasonalityMethod = 'unbounded', noiseMethod = 'LombScargle')
#' @export
envPredictability  <-  function (rawTimeSeries, datesVector, delta, isUneven = FALSE, interpolate = FALSE, checkPlots = FALSE, seasonalityMethod = c('absolute', 'unbounded', 'bounded'), noiseMethod = c('spectrum', 'LombScargle')) {
    if (missing(seasonalityMethod)) {
        seasonalityMethod  <-  'bounded'
    }
    
    if (isUneven && noiseMethod == 'spectrum') {
        stop('Time series is uneven, please use noise method LombScargle')
    }
    # Calculate predicted mean linear trend and residuals (interpolate NAs linearly)
    if (any(is.na(rawTimeSeries)) && interpolate) {
        rawTimeSeries  <-  imputeTS::na.interpolation(rawTimeSeries, option = 'linear')
    }
    
    detrended      <-  linearDetrending(rawTimeSeries, datesVector)
    detrendedVecs  <-  monthlyBinning(detrended$resids, datesVector)

    seasonalityList  <-  envSeasonality(detrendedVecs$interpolatedSeasons, detrendedVecs$residualTimeSeries, seasonalityMethod)
    noiseList        <-  envNoise(detrendedVecs$residualTimeSeries, detrended$predictor, checkPlots, noiseMethod)
    
    if (checkPlots) {
        dev.new()
        plot(datesVector, detrended$resids, type = 'l', xlab = 'Date', ylab = 'Response', col = 'dodgerblue2', lwd = 3, las = 1)
        lines(datesVector, detrendedVecs$interpolatedSeasons, col = 'tomato', lwd = 1)
    }

    data.frame(frequency            =  2 / (length(datesVector) * delta),
               nyquistFrequency     =  1 / (2 * delta),
               predictedVariance    =  seasonalityList$predictedVariance,
               unpredictedVariance  =  seasonalityList$unpredictedVariance,
               seasonality          =  seasonalityList$seasonality,
               environmentalColor   =  noiseList
    )
}

#' Linear detrending of time series
#'
#' @title Linear detrending of time series
#' @param rawTimeSeries A \code{\link[base]{numeric}} vector containing 
#' a raw environmental time series.
#' @param datesVector An object of class \code{\link[base]{Date}} of format YYYY-MM-DD.
#' @return A \code{\link[base]{list}} containing a \code{\link[base]{numeric}} vector 
#' of the time (in days) for each observation in \code{rawTimeSeries} starting at day 0, 
#' and a \code{\link[base]{numeric}} vector containing the residual variation of \code{rawTimeSeries}
#' after removing the linear trend.
#' @author Diego Barneche and Scott Burgess.
#' @seealso \code{\link[envPred]{envPredictability}}.
linearDetrending  <-  function (rawTimeSeries, datesVector) {
    predictor      <-  cumsum(c(0, difftime(datesVector[2:length(datesVector)], datesVector[1:(length(datesVector) - 1)], unit = 'days')))
    linearModel    <-  lm(rawTimeSeries ~ predictor)
    predictedMean  <-  matrix(c(rep(1, length(predictor)), predictor), ncol = 2) %*% coef(linearModel)

    list('predictor'  =  predictor,
         'resids'     =  rawTimeSeries - predictedMean[, 1]
        )
}

#' Adapt Lomb-Scargle Periodogram
#'
#' @title Calculate the 'color' of environmental noise assuming 1/f noise family
#' @param spectrumObject A \code{\link[base]{data.frame}} containing a vector 
#' of frequencies/periods scanned, and a vector containing the normalised power
#' corresponding to scanned frequencies/periods.
#' @param plot Should inspection plots be triggered?
#' @return A \code{\link[base]{numeric}} vector of length 1 containing the beta slope.
#' @details Calculates slope of ln(normalised power)~ln(frequencies).
#' @author Diego Barneche.
#' @seealso \code{\link[envPred]{extractSpectrumSlope}}, \code{\link[envPred]{envPredictability}}.
extractSpectrumSlope  <-  function (spectrumObject, plot = FALSE) {
    model  <-  stats::lm(log(spectrumObject$spec) ~ log(spectrumObject$freq))
    if (plot) {
        dev.new()
        plot(spec ~ freq, data = spectrumObject, type = 'l', xlab = 'Frequency', ylab = 'Spectral power density', col = 'tomato', las = 1)
        dev.new()
        plot(log(spec) ~ log(freq), data = spectrumObject, type = 'l', xlab = 'ln Frequency', ylab = 'ln Spectral power density', col = 'tomato', las = 1)
    }
    as.numeric(abs(coef(model)[2])) # Extract the linear slope coefficient, make it positive
}

#' Adapt Lomb-Scargle Periodogram
#'
#' @title Calculates and renames output of Lomb-Scargle Periodogram 
#' @param residualTimeSeries A \code{\link[base]{numeric}} vector containing unpredicted 
#' detrended residuals as obtained by function \code{\link[envPred]{monthlyBinning}}.
#' @param ... Additional arguments to \code{\link[lomb]{lsp}}.
#' @return A \code{\link[base]{data.frame}} containing a vector 
#' of frequencies/periods scanned, and a vector containing the normalised power
#' corresponding to scanned frequencies/periods.
#' @author Diego Barneche.
#' @seealso \code{\link[envPred]{extractSpectrumSlope}}, \code{\link[envPred]{envPredictability}}.
prepareLombScargleOutput  <-  function (residualTimeSeries, ...) {
    LombScargleObj  <-  lomb::lsp(residualTimeSeries, plot = FALSE, ...)
    data.frame(freq = LombScargleObj$scanned, spec = LombScargleObj$power, stringsAsFactors = FALSE)
}

#' Calculate environmental seasonality
#'
#' @title Calculate the 'color' of unpredicted detrended residuals 
#' @param interpolatedSeasons A \code{\link[base]{numeric}} vector containing predicted (interpolated) 
#' detrended residuals as obtained by function \code{\link[envPred]{monthlyBinning}}.
#' @param residualTimeSeries A \code{\link[base]{numeric}} vector containing unpredicted 
#' detrended residuals as obtained by function \code{\link[envPred]{monthlyBinning}}.
#' @param seasonalityMethod A method for estimating seasonality. Takes 3 possible 
#' values: \code{'absolute'}, \code{'unbounded'}, or \code{'bounded'}, with no default.
#' @return A \code{\link[base]{list}} containing the sample variance of \code{interpolatedSeasons},
#' \code{residualTimeSeries}, and the resulting seasonality. See details.
#' @details If \code{seasonalityMethod} is \code{'absolute'}, then seasonality corresponds to the
#' sample variance of \code{interpolatedSeasons}; if \code{'unbounded'}, it corresponds to to the 
#' ratio between sample variances of \code{interpolatedSeasons} and \code{residualTimeSeries};
#' if \code{'bounded'}, it corresponds to the sample variance of \code{interpolatedSeasons}
#' relative to the total summed variances of both \code{interpolatedSeasons} and \code{residualTimeSeries}.
#' @author Diego Barneche.
#' @seealso \code{\link[envPred]{monthlyBinning}}, \code{\link[envPred]{envPredictability}}.
envSeasonality  <-  function (interpolatedSeasons, residualTimeSeries, seasonalityMethod = c('absolute', 'unbounded', 'bounded')) {
    varPredict    <-  var(interpolatedSeasons, na.rm = TRUE)
    varUnpredict  <-  var(residualTimeSeries, na.rm = TRUE)
    switch (match.arg(seasonalityMethod),
            'absolute' = {
                seasonality   <-  varPredict
            },
            'unbounded' = {
                seasonality   <-  varPredict / varUnpredict
            },
            'bounded' = {
                seasonality   <-  varPredict / (varPredict + varUnpredict)
            }
    )
    list(predictedVariance    =  varPredict,
         unpredictedVariance  =  varUnpredict,
         seasonality          =  seasonality
        )
}

#' Calculate environmental noise
#'
#' @title Calculate the 'color' of the noise of unpredicted detrended residuals 
#' @param residualTimeSeries A \code{\link[base]{numeric}} vector containing unpredicted 
#' detrended residuals as obtained by function \code{\link[envPred]{monthlyBinning}}.
#' @param predictor A \code{\link[base]{numeric}} vector containing the time (in days)
#' for each observation in \code{residualTimeSeries}. Needs to start at day 0.
#' @param checkPlots Should inspection plots be triggered? No default value.
#' @param noiseMethod A method for estimating the slope beta. Takes 2 possible 
#' values: \code{'spectrum'} for evenly distributed time series or 
#' \code{'LombScargle'} for unevenly distributed ones.
#' @return A \code{\link[base]{numeric}} vector of length 1 containing the beta slope.
#' @details This function calculates beta slope (assuming 1/f noise family) on the
#' residual time series using function \code{\link[envPred]{extractSpectrumSlope}}.
#' @author Diego Barneche.
#' @seealso \code{\link[envPred]{monthlyBinning}}, \code{\link[envPred]{envPredictability}}, \code{\link[envPred]{extractSpectrumSlope}}.
envNoise  <-  function (residualTimeSeries, predictor, checkPlots, noiseMethod = c('spectrum', 'LombScargle')) {
    switch (match.arg(noiseMethod),
            'spectrum' = {
                betaLS1  <-  extractSpectrumSlope(stats::spectrum(stats::as.ts(residualTimeSeries), plot = FALSE), plot = checkPlots)
            },
            'LombScargle' = {
                LombScargleObj  <-  prepareLombScargleOutput(residualTimeSeries, times = predictor)
                betaLS1         <-  extractSpectrumSlope(LombScargleObj, plot = checkPlots)
            }
    )
    betaLS1
}

#' Compute the seasonal (monthly) components of time series
#'
#' @title Compute the seasonal (monthly) components of time series 
#' @param resids A \code{\link[base]{numeric}} vector containing the residual 
#' variation of a raw time series after removing the linear trend.
#' @param datesVec An object of class \code{\link[base]{Date}} of format YYYY-MM-DD.
#' @return A \code{\link[base]{list}} containing the a \code{\link[base]{numeric}} 
#' vector containing predicted (interpolated) detrended residuals (\code{'interpolatedSeasons'}), and
#' a \code{\link[base]{numeric}} vector containing unpredicted detrended residuals 
#' \code{'residualTimeSeries'}. See details.
#' @details This algorithm follows the steps described in \href{http://onlinelibrary.wiley.com/doi/10.1111/ele.12402/abstract}{Marshall and Burgess 2015} Ecology Letters 18: 1461–0248, doi: \href{http://dx.doi.org/10.1111/ele.12402}{10.1111/ele.12402}.
#' @author Diego Barneche and Scott Burgess.
#' @seealso \code{\link[envPred]{envPredictability}}.
monthlyBinning  <-  function (resids, datesVec) {
    monthAvResidAcrossYears      <-  aggregate(resids, by = list(format(datesVec, format = '%B')), mean, na.rm = TRUE)
    dailyDummyTimeSeries         <-  seq.Date(from = as.Date(format(datesVec[1], format = '1 %B %Y'), format = '%d %B %Y'), to = as.Date(noaaErddap::findLastDayOfTheMonth(datesVec[length(datesVec)])), by = 'day')
    dummyTimeSeriesMonthYear     <-  format(dailyDummyTimeSeries, format = '%B %Y')
    medianMonthDatesAcrossYears  <-  aggregate(dailyDummyTimeSeries, by = list(dummyTimeSeriesMonthYear), median)
    medianMonthDatesAcrossYears  <-  medianMonthDatesAcrossYears$x[match(unique(dummyTimeSeriesMonthYear), medianMonthDatesAcrossYears$Group.1)]
    monthData  <-  monthAvResidAcrossYears[match(format(medianMonthDatesAcrossYears, format = '%B'), monthAvResidAcrossYears[, 1]), ]
    
    # Do the interpolation to get seasonal trend (linear, do not allow for NAs at the extremes)
    seasonalInterpolation  <-  approxfun(x = medianMonthDatesAcrossYears, y = monthData[, 2], method = 'linear', rule = 2)
    interpolatedSeasons    <-  seasonalInterpolation(datesVec)
    # Calculate residual time series with seasonal trend removed
    residualTimeSeries     <-  resids - interpolatedSeasons
    
    list(residualTimeSeries   =  residualTimeSeries,
         interpolatedSeasons  =  interpolatedSeasons
        )
}
