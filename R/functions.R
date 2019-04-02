#' Environmental predictability components
#'
#' @title Calculates environmental predictability components
#' @param rawTimeSeries A \code{\link[base]{numeric}} vector containing 
#' a raw environmental time series.
#' @param datesVector An vector of class \code{\link[base]{Date}} of format YYYY-MM-DD 
#' (must be in progressive chronological order).
#' @param delta Time interval (any unit) of \code{rawTimeSeries}.
#' @param isUneven Is \code{rawTimeSeries} even or unevenly distributed in time? Default is \code{\link[base]{FALSE}}.
#' @param interpolate Should a linear interpolation be applied to missing values in 
#' \code{rawTimeSeries}? Irrelevant method if \code{isUneven} is \code{\link[base]{FALSE}}. Default is \code{\link[base]{FALSE}}.
#' @param checkPlots Should inspection plots be triggered? Default is \code{\link[base]{FALSE}}. Not recommended 
#' if applying this task to multiple time series at once.
#' @param showWarnings Should cautionary warning messages be displayed? Default is \code{\link[base]{TRUE}}. 
#' Strongly recommended for first time users.
#' @param noiseMethod A method for estimating the slope beta. Takes 2 possible 
#' values: \code{'spectrum'} for evenly distributed time series or 
#' \code{'LombScargle'} for unevenly distributed ones.
#' @details This function currently allows for monthly-based seasonality calculation only. 
#' This algorithm adapts the steps described in \href{http://onlinelibrary.wiley.com/doi/10.1111/ele.12402/abstract}{Marshall and Burgess (2015)} Ecology Letters 18: 1461–0248, doi: \href{http://dx.doi.org/10.1111/ele.12402}{10.1111/ele.12402}.
#' 
#' We advise caution for datasets in which \code{rawTimeSeries} contains NAs. Particularly, 
#' we strongly advise setting \code{interpolate = FALSE} for \code{rawTimeSeries} containing large 
#' continuous chunks of NAs because the linear interpolation might introduce substantial bias.
#'
#' \href{http://onlinelibrary.wiley.com/doi/10.1890/02-3122/abstract}{Vasseur and Yodzis (2004)} Ecology 85: 1146-1152, doi: \href{http://dx.doi.org/10.1890/02-3122}{10.1890/02-3122} recommend time series encompassing at least 128 months or approximately 10 years.
#' 
#' Also note that if \code{datesVector} starts and ends in different month of the year, 
#' some monthly means are estimated with relatively less precision.
#' @return A \code{\link[base]{data.frame}} with environmental predictability components.
#' @author Diego Barneche and Scott Burgess.
#' @seealso \code{\link[envPred]{envSeasonality}}, \code{\link[envPred]{envNoise}}, \code{\link[envPred]{colwell74}}.
#' @examples
#' library(envPred)
#' data(sst)
#' # all results return important warning messages of which the user should be aware.
#' # after double-checking that the data falls within the recommended specs, set showWarnings = FALSE
#' envPredictability(sst$rawTimeSeries, sst$datesVector, delta = 1, isUneven = FALSE, interpolate = FALSE, checkPlots = FALSE, showWarnings = TRUE, noiseMethod = 'spectrum')
#' envPredictability(sst$rawTimeSeries, sst$datesVector, delta = 1, isUneven = FALSE, interpolate = FALSE, checkPlots = FALSE, showWarnings = TRUE, noiseMethod = 'spectrum')
#' envPredictability(sst$rawTimeSeries, sst$datesVector, delta = 1, isUneven = FALSE, interpolate = FALSE, checkPlots = FALSE, showWarnings = TRUE, noiseMethod = 'spectrum')
#' data(npp)
#' envPredictability(npp$rawTimeSeries, npp$datesVector, delta = 8, isUneven = TRUE, interpolate = FALSE, checkPlots = TRUE, showWarnings = TRUE, noiseMethod = 'LombScargle')
#' envPredictability(npp$rawTimeSeries, npp$datesVector, delta = 8, isUneven = TRUE, interpolate = TRUE, checkPlots = TRUE, showWarnings = TRUE, noiseMethod = 'LombScargle')
#' @export
envPredictability  <-  function (rawTimeSeries, datesVector, delta, isUneven = FALSE, interpolate = FALSE, checkPlots = FALSE, showWarnings = TRUE, noiseMethod) {
    seriesLength   <-  length(rawTimeSeries)
    nOfNAs         <-  sum(is.na(rawTimeSeries))

    if (isUneven && noiseMethod == 'spectrum') {
        stop('Time series is uneven, please use noise method LombScargle')
    }
    if (any(is.na(rawTimeSeries)) && interpolate) {
        rawTimeSeries  <-  imputeTS::na.interpolation(rawTimeSeries, option = 'linear')
    }
    if (showWarnings) {
        if (any(is.na(rawTimeSeries))) {
            warning('Data contains NAs, we strongly advise setting "interpolate = FALSE" for rawTimeSeries containing large continuous chunks of NAs')
        }
        nOfMonths  <-  length(unique(format(datesVector, format = '%B%Y')))
        if (nOfMonths < 120) {
            warning(sprintf('Time series is shorter than recommended (contains %s months); see ?envPredictability', nOfMonths))
        }
        months              <-  format(datesVector, format = '%B')
        startAndEndNotSame  <-  months[1] != months[length(months)]
        if (startAndEndNotSame) {
            warning(sprintf('Time series starts and ends at different times of the year. Starting month is %s and ending month is %s', months[1], months[length(months)]))
        }
    }
    
    detrended      <-  linearDetrending(rawTimeSeries, datesVector)
    detrendedVecs  <-  monthlyBinning(detrended$resids, datesVector)

    seasonalityList  <-  envSeasonality(detrendedVecs$interpolatedSeasons, detrendedVecs$residualTimeSeries)
    noiseList        <-  envNoise(detrendedVecs$residualTimeSeries, detrended$predictor, checkPlots, noiseMethod)
    
    if (checkPlots) {
        dev.new()
        plot(datesVector, detrended$resids, type = 'l', xlab = 'Date', ylab = 'Response', col = 'dodgerblue2', lwd = 3, las = 1)
        lines(datesVector, detrendedVecs$interpolatedSeasons, col = 'tomato', lwd = 1)
    }

    data.frame(seriesLength         =  seriesLength,
               nOfNAs               =  nOfNAs,
               proportionNAs        =  nOfNAs / seriesLength,
               nOfYears             =  length(unique(format(datesVector, format = '%Y'))),
               nOfMonths            =  length(unique(format(datesVector, format = '%B'))),
               nOfDays              =  length(unique(datesVector)),
               frequency            =  2 / (length(datesVector) * delta),
               nyquistFrequency     =  1 / (2 * delta),
               rawMean              =  mean(rawTimeSeries, na.rm = TRUE),
               rawVariance          =  var(rawTimeSeries, na.rm  = TRUE),
               rawCV                =  sd(rawTimeSeries, na.rm  = TRUE) / mean(rawTimeSeries, na.rm  = TRUE),
               predictedVariance    =  seasonalityList$predictedVariance,
               unpredictedVariance  =  seasonalityList$unpredictedVariance,
               unBoundedSeasonality =  seasonalityList$unBoundedSeasonality,
               boundedSeasonality   =  seasonalityList$boundedSeasonality,
               environmentalColor   =  noiseList)
}

#' Environmental predictability components
#'
#' @title Calculates environmental predictability components and Colwell (1974) metric
#' @param rawTimeSeries A \code{\link[base]{numeric}} vector containing 
#' a raw environmental time series.
#' @param datesVector An vector of class \code{\link[base]{Date}} of format YYYY-MM-DD 
#' (must be in progressive chronological order).
#' @param nStates is a \code{\link[base]{numeric}} vector of length 1 containing 
#' a somewhat arbitrary number, as Colwell's method divides a continuous variable
#' up into discrete states. Default (arbitrary) is 11. See \code{\link[envPred]{colwell74}}
#' for Details.
#' @param ... Additional arguments to \code{\link[envPred]{envPredictability}}.
#' @details Wrapper function.
#' @return A \code{\link[base]{data.frame}} with environmental predictability components and 
#' Colwell (1974) metric.
#' @author Diego Barneche and Scott Burgess.
#' @seealso \code{\link[envPred]{envPredictability}},\code{\link[envPred]{colwell74}}.
#' @export
envPredictabilityAndColwell  <-  function (rawTimeSeries, datesVector, nStates, ...) {    
    predicTab      <-  envPredictability(rawTimeSeries, datesVector, ...)
    colwellStats   <-  colwell74(rawTimeSeries, datesVector, nStates)
    cbind(predicTab, colwellStats)
}

#' Predictability from Colwell (1974)
#'
#' @title Calculates Constancy (C), Contingency (M), and Predictability (P) metrics from Colwell (1974)
#' for seasonally varying environmental data. Can be applied to any phenomenon known or suspected to be
#' periodic or cyclic in time which can be scored for at least two states. Scores from each month in the
#' cycle are then collected for as many complete cycles as possible, and the data cast as a frequency
#' matrix with months as columns, and states as rows. See Colwell (1974) for more information.
#' @param rawTimeSeries A \code{\link[base]{numeric}} vector containing 
#' a raw environmental time series.
#' @param datesVector An vector of class \code{\link[base]{Date}} of format YYYY-MM-DD 
#' (must be in progressive chronological order).
#' @param nStates is a \code{\link[base]{numeric}} vector of length 1 containing 
#' a somewhat arbitrary number, as Colwell's method divides a continuous variable
#' up into discrete states. Default (arbitrary) is 11. See Details.
#' @details This algorithm implements the methods described in \href{http://onlinelibrary.wiley.com/doi/10.2307/1940366/abstract}{Colwell (1974)} Ecology 55: 1148–1153, doi: \href{http://dx.doi.org/10.2307/1940366}{10.2307/1940366}.
#' @return A \code{\link[base]{data.frame}} with three environmental predictability components:
#' constancy (\code{Colwell_C}), contingency (\code{Colwell_M}) and predictability (\code{Colwell_P}).
#' Constancy (C) measures the extent to which the environment is the same for all months in all years. 
#' Contingency (M) measures the extent to which the environmental differences between months are the same 
#' in all years. Predictability (P) is the sum of Constancy (C) and Contingency (M). Maximum predictability 
#' can be attained as a consequence of either complete constancy, complete contingency, or a combination 
#' of constancy and contingency.
#' @author Diego Barneche and Scott Burgess.
#' @seealso \code{\link[envPred]{envPredictability}}.
#' @examples
#' library(envPred)
#' data(sst)
#' data(npp)
#' nStates  <-  11
#' colwell74(sst$rawTimeSeries, sst$datesVector, nStates)
#' colwell74(npp$rawTimeSeries, npp$datesVector, nStates)
#' @export
colwell74  <-  function (rawTimeSeries, datesVector, nStates = 11) {
    dat        <-  data.frame(datesVector = datesVector)
    dat$month  <-  factor(strftime(dat$datesVector, format = '%m'))
    dat$year   <-  factor(strftime(dat$datesVector, format = '%Y'))
    dat$yRaw   <-  rawTimeSeries
    monthAverageAcrossYears         <-  aggregate(yRaw ~ month + year, dat, mean, na.rm = TRUE)
    monthAverageAcrossYears$breaks  <-  cut(monthAverageAcrossYears$yRaw, nStates, right = FALSE, include.lowest = TRUE)
    colwellMat                      <-  with(monthAverageAcrossYears, table(breaks, month))
    
    X    <-  colSums(colwellMat, na.rm = TRUE)
    Y    <-  rowSums(colwellMat, na.rm = TRUE)
    Z    <-  sum(colwellMat, na.rm = TRUE)
    HX   <-  -sum((X / Z) * log2(X / Z), na.rm = TRUE)
    HY   <-  -sum((Y / Z) * log2(Y / Z), na.rm = TRUE)
    HXY  <-  -sum((colwellMat / Z) * log2(colwellMat / Z), na.rm = TRUE)
    Colwell_C  <-  1 - (HY / log2(nStates))
    Colwell_M  <-  (HX + HY - HXY) / log2(nStates)
    Colwell_P  <-  Colwell_C + Colwell_M
    data.frame(Colwell_C  =  Colwell_C,
               Colwell_M  =  Colwell_M,
               Colwell_P  =  Colwell_P
               )
}

#' Linear detrending of time series
#'
#' @title Linear detrending of time series
#' @param rawTimeSeries A \code{\link[base]{numeric}} vector containing 
#' a raw environmental time series.
#' @param datesVector An object of class \code{\link[base]{Date}} of format YYYY-MM-DD
#' (must be in progressive chronological order).
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
#' @param plot Should inspection plots be triggered? Default is \code{\link[base]{FALSE}}.
#' @return A \code{\link[base]{numeric}} vector of length 1 containing the beta slope.
#' @details Calculates slope of log10(normalised power)~log10(frequencies).
#' @author Diego Barneche and Scott Burgess.
#' @seealso \code{\link[envPred]{extractSpectrumSlope}}, \code{\link[envPred]{envPredictability}}.
extractSpectrumSlope  <-  function (spectrumObject, plot = FALSE) {
    model  <-  stats::lm(log10(spec) ~ log10(freq), data = spectrumObject)
    if (plot) {
        dev.new()
        plot(spec ~ freq, data = spectrumObject, type = 'l', xlab = 'Frequency', ylab = 'Spectral power density', col = 'tomato', las = 1)
        dev.new()
        plot(log10(spec) ~ log10(freq), data = spectrumObject, type = 'l', xlab = 'log10 Frequency', ylab = 'log10 Spectral power density', col = 'tomato', las = 1)
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
#' @author Diego Barneche and Scott Burgess.
#' @seealso \code{\link[envPred]{extractSpectrumSlope}}, \code{\link[envPred]{envPredictability}}.
prepareLombScargleOutput  <-  function (residualTimeSeries, ...) {
    LombScargleObj  <-  lomb::lsp(residualTimeSeries, plot = FALSE, ...)
    data.frame(freq = LombScargleObj$scanned, spec = LombScargleObj$power, stringsAsFactors = FALSE)
}

#' Adapt Spectrum Periodogram
#'
#' @title Calculates and renames output of Spectrum Periodogram 
#' @param residualTimeSeries A \code{\link[base]{numeric}} vector containing unpredicted 
#' detrended residuals as obtained by function \code{\link[envPred]{monthlyBinning}}.
#' @param ... Additional arguments to \code{\link[stats]{spectrum}}.
#' @return A \code{\link[base]{data.frame}} containing a vector 
#' of frequencies/periods scanned, and a vector containing the normalised power
#' corresponding to scanned frequencies/periods.
#' @author Diego Barneche and Scott Burgess.
#' @seealso \code{\link[envPred]{extractSpectrumSlope}}, \code{\link[envPred]{envPredictability}}.
prepareSpectrumOutput  <-  function (residualTimeSeries, ...) {
    spectrumObj  <-  stats::spectrum(stats::as.ts(residualTimeSeries), plot = FALSE, ...)
    data         <-  data.frame(freq = spectrumObj$freq, spec = spectrumObj$spec, stringsAsFactors = FALSE)
}

#' Calculate environmental seasonality
#'
#' @title Calculate the 'color' of unpredicted detrended residuals 
#' @param interpolatedSeasons A \code{\link[base]{numeric}} vector containing predicted (interpolated) 
#' detrended residuals as obtained by function \code{\link[envPred]{monthlyBinning}}.
#' @param residualTimeSeries A \code{\link[base]{numeric}} vector containing unpredicted 
#' detrended residuals as obtained by function \code{\link[envPred]{monthlyBinning}}.
#' @return A \code{\link[base]{list}} containing the sample variance of \code{interpolatedSeasons},
#' \code{residualTimeSeries}, and the resulting seasonality. See details.
#' @details Three types of seasonality are returned currently: an 'absolute' seasonality which corresponds to the sample variance of
#' \code{interpolatedSeasons}; an unbounded seasonality which corresponds to the 
#' ratio between sample variances of \code{interpolatedSeasons} and \code{residualTimeSeries}; and a 'bounded' seasonality which corresponds to the sample variance of \code{interpolatedSeasons}
#' relative to the total summed variances of both \code{interpolatedSeasons} and \code{residualTimeSeries}.
#' @author Diego Barneche and Scott Burgess.
#' @seealso \code{\link[envPred]{monthlyBinning}}, \code{\link[envPred]{envPredictability}}.
envSeasonality  <-  function (interpolatedSeasons, residualTimeSeries) {
    varPredict    <-  var(interpolatedSeasons, na.rm = TRUE)
    varUnpredict  <-  var(residualTimeSeries, na.rm = TRUE)
    list(predictedVariance     =  varPredict,
         unpredictedVariance   =  varUnpredict,
         unBoundedSeasonality  =  varPredict / varUnpredict,
         boundedSeasonality    =  varPredict / (varPredict + varUnpredict)
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
#' @author Diego Barneche and Scott Burgess.
#' @seealso \code{\link[envPred]{monthlyBinning}}, \code{\link[envPred]{envPredictability}}, \code{\link[envPred]{extractSpectrumSlope}}.
envNoise  <-  function (residualTimeSeries, predictor, checkPlots, noiseMethod = c('spectrum', 'LombScargle')) {
    switch (match.arg(noiseMethod),
            'spectrum' = {
                spectrumObj  <-  prepareSpectrumOutput(residualTimeSeries)
            },
            'LombScargle' = {
                spectrumObj  <-  prepareLombScargleOutput(residualTimeSeries, times = predictor)
            }
    )
    extractSpectrumSlope(spectrumObj, plot = checkPlots)
}

#' Compute the seasonal (monthly) components of time series
#'
#' @title Compute the seasonal (monthly) components of time series 
#' @param resids A \code{\link[base]{numeric}} vector containing the residual 
#' variation of a raw time series after removing the linear trend.
#' @param datesVec An object of class \code{\link[base]{Date}} of format YYYY-MM-DD
#' (must be in progressive chronological order).
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
