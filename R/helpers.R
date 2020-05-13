#' Wrapper
#'
#' @title Binds output from \code{\link{linear_detrend}} and
#' \code{\link{monthly_bins}}
#' @param ... Arguments passed to \code{\link{linear_detrend}}
#' @return  A \code{\link[base]{data.frame}}.
#' @author Diego Barneche and Scott Burgess.
detrend_and_bin <- function(...) {
  df <- linear_detrend(...)
  cbind(df, monthly_bins(df$resids, df$dates))
}

#' Linear detrending of time series
#'
#' @title Linear detrending of time series
#' @param time_series A \code{\link[base]{numeric}} vector containing 
#' a raw environmental time series.
#' @param dates An object of class \code{\link[base]{Date}} of format YYYY-MM-DD
#' (must be in progressive chronological order).
#' @return A \code{\link[base]{data.frame}} containing a \code{\link[base]{numeric}} vector 
#' of the time (in days) for each observation in \code{time_series} starting at day 0, 
#' and a \code{\link[base]{numeric}} vector containing the residual variation of \code{time_series}
#' after removing the linear trend.
#' @author Diego Barneche and Scott Burgess.
#' @importFrom stats coef lm
#' @seealso \code{\link{predictability}}.
linear_detrend <- function(time_series, dates) {
  predictor <- cumsum(c(0, difftime(dates[2:length(dates)],
                                    dates[1:(length(dates) - 1)],
                                    units = "days")))
  lm_mod <- lm(time_series ~ predictor)
  predicted_av <- matrix(c(rep(1, length(predictor)), predictor), ncol = 2)
  predicted_av <- predicted_av %*% coef(lm_mod)
  data.frame(predictor = predictor,
             resids = time_series - predicted_av[, 1],
             dates = dates,
             stringsAsFactors = FALSE)
}

#' Adapt Lomb-Scargle Periodogram
#'
#' @title Calculate the 'colour' of environmental noise assuming 1/f noise family
#' @param spec_obj A \code{\link[base]{data.frame}} containing a vector 
#' of frequencies/periods scanned, and a vector containing the normalised power
#' corresponding to scanned frequencies/periods.
#' @return A \code{\link[base]{numeric}} vector of length 1 containing the beta slope.
#' @details Calculates slope of log10(normalised power)~log10(frequencies).
#' @author Diego Barneche and Scott Burgess.
#' @importFrom stats coef lm
#' @seealso \code{\link{predictability}}.
spec_slope_get <- function(spec_obj) {
  model <- lm(log10(spec) ~ log10(freq), data = spec_obj)
  list(slope = as.numeric(abs(coef(model)[2])),
       spec_obj = spec_obj)
}

#' Adapt Lomb-Scargle Periodogram
#'
#' @title Calculates and renames output of Lomb-Scargle Periodogram 
#' @param resid_time_series A \code{\link[base]{numeric}} vector containing unpredicted 
#' de-trended residuals as obtained by function \code{\link{monthly_bins}}.
#' @param ... Additional arguments to \code{\link[lomb]{lsp}}.
#' @return A \code{\link[base]{data.frame}} containing a vector 
#' of frequencies/periods scanned, and a vector containing the normalised power
#' corresponding to scanned frequencies/periods.
#' @author Diego Barneche and Scott Burgess.
#' @seealso \code{\link{spec_slope_get}}, \code{\link{predictability}}.
#' @importFrom lomb lsp
lmb_output_prep <- function(resid_time_series, ...) {
  lmb_obj <- lsp(resid_time_series, plot = FALSE, ...)
  data.frame(freq = lmb_obj$scanned,
             spec = lmb_obj$power,
             stringsAsFactors = FALSE)
}

#' Adapt Spectrum Periodogram
#'
#' @title Calculates and renames output of Spectrum Periodogram 
#' @param resid_time_series A \code{\link[base]{numeric}} vector containing unpredicted 
#' de-trended residuals as obtained by function \code{\link{monthly_bins}}.
#' @param ... Additional arguments to \code{\link[stats]{spectrum}}.
#' @return A \code{\link[base]{data.frame}} containing a vector 
#' of frequencies/periods scanned, and a vector containing the normalised power
#' corresponding to scanned frequencies/periods.
#' @author Diego Barneche and Scott Burgess.
#' @importFrom stats spectrum as.ts
#' @seealso \code{\link{spec_slope_get}}, \code{\link{predictability}}.
spec_output_prep <- function(resid_time_series, ...) {
  spec_obj <- spectrum(as.ts(resid_time_series),
                       plot = FALSE,
                       ...)
  data.frame(freq = spec_obj$freq,
             spec = spec_obj$spec,
             stringsAsFactors = FALSE)
}

#' Calculate environmental seasonality
#'
#' @title Calculate the 'colour' of unpredicted de-trended residuals 
#' @param interpolated_seasons A \code{\link[base]{numeric}} vector containing predicted (interpolated) 
#' de-trended residuals as obtained by function \code{\link{monthly_bins}}.
#' @param resid_time_series A \code{\link[base]{numeric}} vector containing unpredicted 
#' de-trended residuals as obtained by function \code{\link{monthly_bins}}.
#' @return A \code{\link[base]{list}} containing the sample variance of \code{interpolated_seasons},
#' \code{resid_time_series}, and the resulting seasonality. See details.
#' @details Three types of seasonality are returned currently: an 'absolute' seasonality which corresponds to the sample variance of
#' \code{interpolated_seasons}; an unbounded seasonality which corresponds to the 
#' ratio between sample variances of \code{interpolated_seasons} and \code{resid_time_series}; and a 'bounded' seasonality which corresponds to the sample variance of \code{interpolated_seasons}
#' relative to the total summed variances of both \code{interpolated_seasons} and \code{resid_time_series}.
#' @author Diego Barneche and Scott Burgess.
#' @importFrom stats var
#' @seealso \code{\link{monthly_bins}}, \code{\link{predictability}}.
seasonality_calc <- function(interpolated_seasons, resid_time_series) {
  var_predict <- var(interpolated_seasons, na.rm = TRUE)
  var_unpredict <- var(resid_time_series, na.rm = TRUE)
  list(predicted_var = var_predict,
       unpredicted_var = var_unpredict,
       unbounded_seasonality = var_predict / var_unpredict,
       bounded_seasonality = var_predict / (var_predict + var_unpredict)
  )
}

#' Calculate environmental noise
#'
#' @title Calculate the 'colour' of the noise of unpredicted de-trended residuals 
#' @param resid_time_series A \code{\link[base]{numeric}} vector containing unpredicted 
#' de-trended residuals as obtained by function \code{\link{monthly_bins}}.
#' @param predictor A \code{\link[base]{numeric}} vector containing the time (in days)
#' for each observation in \code{resid_time_series}. Needs to start at day 0.
#' @param noise_method A method for estimating the slope beta. Takes 2 possible 
#' values: \code{'spectrum'} for evenly distributed time series or 
#' \code{'lomb_scargle'} for unevenly distributed ones.
#' @return A \code{\link[base]{numeric}} vector of length 1 containing the beta slope.
#' @details This function calculates beta slope (assuming 1/f noise family) on the
#' residual time series using function \code{\link{spec_slope_get}}.
#' @author Diego Barneche and Scott Burgess.
#' @seealso \code{\link{monthly_bins}}, \code{\link{predictability}}, \code{\link{spec_slope_get}}.
noise_calc <- function(resid_time_series, predictor,
                       noise_method = c("spectrum", "lomb_scargle")) {
  switch(match.arg(noise_method),
         "spectrum" = {
           spec_obj <- spec_output_prep(resid_time_series)
         },
         "lomb_scargle" = {
           spec_obj <- lmb_output_prep(resid_time_series, times = predictor)
         }
  )
  spec_slope_get(spec_obj)
}

#' Compute the seasonal (monthly) components of time series
#'
#' @title Compute the seasonal (monthly) components of time series 
#' @param resids A \code{\link[base]{numeric}} vector containing the residual 
#' variation of a raw time series after removing the linear trend.
#' @param dates An object of class \code{\link[base]{Date}} of format YYYY-MM-DD
#' (must be in progressive chronological order).
#' @return A \code{\link[base]{data.frame}} containing the a \code{\link[base]{numeric}} 
#' vector containing predicted (interpolated) de-trended residuals (\code{'interpolated_seasons'}), and
#' a \code{\link[base]{numeric}} vector containing unpredicted de-trended residuals 
#' \code{'resid_time_series'}. See details.
#' @details This algorithm follows the steps described in \href{http://onlinelibrary.wiley.com/doi/10.1111/ele.12402/abstract}{Marshall and Burgess 2015} Ecology Letters 18: 1461–0248, doi: \href{http://dx.doi.org/10.1111/ele.12402}{10.1111/ele.12402}.
#' @author Diego Barneche and Scott Burgess.
#' @seealso \code{\link{predictability}}.
#' @importFrom noaaErddap findLastDayOfTheMonth
#' @importFrom stats aggregate approxfun median
monthly_bins <- function(resids, dates) {
  fct <- findLastDayOfTheMonth
  month_av_res_yrs <- aggregate(resids, by = list(format(dates, format = "%B")),
                                mean, na.rm = TRUE)
  daily_dummy_ts <- seq.Date(from = as.Date(format(dates[1],
                                                   format = "1 %B %Y"),
                                            format = "%d %B %Y"),
                             to = as.Date(fct(dates[length(dates)])),
                             by = "day")
  dummy_ts_m_y <- format(daily_dummy_ts, format = "%B %Y")
  med_month_yrs <- aggregate(daily_dummy_ts, by = list(dummy_ts_m_y), median)
  med_month_yrs <- med_month_yrs$x[match(unique(dummy_ts_m_y),
                                         med_month_yrs$Group.1)]
  month_data <- month_av_res_yrs[match(format(med_month_yrs, format = "%B"),
                                       month_av_res_yrs[, 1]), ]
  # Do the interpolation to get seasonal trend
  # (linear, do not allow for NAs at the extremes)
  season_interpolate <- approxfun(x = med_month_yrs,
                                  y = month_data[, 2],
                                  method = "linear",
                                  rule = 2)
  interpolated_seasons <- season_interpolate(dates)
  # Calculate residual time series with seasonal trend removed
  resid_time_series <- resids - interpolated_seasons
  data.frame(resid_time_series = resid_time_series,
             interpolated_seasons = interpolated_seasons,
             stringsAsFactors = FALSE)
}
