#' Environmental predictability components
#'
#' @title Calculates environmental predictability components
#' @param time_series A \code{\link[base]{numeric}} vector containing 
#' a raw environmental time series.
#' @param dates An vector of class \code{\link[base]{Date}}
#' of format YYYY-MM-DD (must be in progressive chronological order).
#' @param delta Time interval (any unit) of \code{time_series}.
#' @param is_uneven Is \code{time_series} even or unevenly distributed in time? Default is \code{\link[base]{FALSE}}.
#' @param interpolate Should a linear interpolation be applied to missing values in 
#' \code{time_series}? Irrelevant method if \code{is_uneven} is \code{\link[base]{FALSE}}. Default is \code{\link[base]{FALSE}}.
#' @param show_warns Should cautionary warning messages be displayed? Default is \code{\link[base]{TRUE}}. 
#' Strongly recommended for first time users.
#' @param noise_method A method for estimating the slope beta. Takes 2 possible 
#' values: \code{'spectrum'} for evenly distributed time series or 
#' \code{'lomb_scargle'} for unevenly distributed ones.
#' @details This function currently allows for monthly-based seasonality calculation only. 
#' This algorithm adapts the steps described in \href{http://onlinelibrary.wiley.com/doi/10.1111/ele.12402/abstract}{Marshall and Burgess (2015)} Ecology Letters 18: 1461–0248, doi: \href{http://dx.doi.org/10.1111/ele.12402}{10.1111/ele.12402}.
#' 
#' We advise caution for datasets in which \code{time_series} contains NAs. Particularly, 
#' we strongly advise setting \code{interpolate = FALSE} for \code{time_series} containing large 
#' continuous chunks of NAs because the linear interpolation might introduce substantial bias.
#'
#' \href{http://onlinelibrary.wiley.com/doi/10.1890/02-3122/abstract}{Vasseur and Yodzis (2004)} Ecology 85: 1146-1152, doi: \href{http://dx.doi.org/10.1890/02-3122}{10.1890/02-3122} recommend time series encompassing at least 128 months or approximately 10 years.
#' 
#' Also note that if \code{dates} starts and ends in different month of the year, 
#' some monthly means are estimated with relatively less precision.
#' @return A \code{\link[base]{data.frame}} with environmental predictability components.
#' @author Diego Barneche and Scott Burgess.
#' @seealso \code{\link{seasonality_calc}}, \code{\link{noise_calc}}, \code{\link{colwell74}}.
#' @examples
#' library(envPred)
#' data(sst)
#' # all results return important
#' # warning messages of which the user should be aware.
#' # after double-checking that the data falls
#' # within the recommended specs, set show_warns = FALSE
#' predictability(sst$time_series,
#'                sst$dates,
#'                delta = 1,
#'                is_uneven = FALSE,
#'                interpolate = FALSE,
#'                show_warns = TRUE,
#'                noise_method = 'spectrum')
#' predictability(sst$time_series,
#'                sst$dates,
#'                delta = 1,
#'                is_uneven = FALSE,
#'                interpolate = FALSE,
#'                show_warns = TRUE,
#'                noise_method = 'spectrum')
#' predictability(sst$time_series,
#'                sst$dates,
#'                delta = 1,
#'                is_uneven = FALSE,
#'                interpolate = FALSE,
#'                show_warns = TRUE,
#'                noise_method = 'spectrum')
#' data(npp)
#' predictability(npp$time_series,
#'                npp$dates,
#'                delta = 8,
#'                is_uneven = TRUE,
#'                interpolate = FALSE,
#'                show_warns = TRUE,
#'                noise_method = 'lomb_scargle')
#' predictability(npp$time_series,
#'                npp$dates,
#'                delta = 8,
#'                is_uneven = TRUE,
#'                interpolate = TRUE,
#'                show_warns = TRUE,
#'                noise_method = 'lomb_scargle')
#' @importFrom imputeTS na_interpolation
#' @importFrom stats var sd
#' @export
predictability <- function(time_series, dates, delta, is_uneven = FALSE,
                           interpolate = FALSE, show_warns = TRUE,
                           noise_method) {
  series_n  <- length(time_series)
  n_na <- sum(is.na(time_series))
  
  if (is_uneven && noise_method == "spectrum") {
    stop("Time series is uneven, please use noise method lomb_scargle")
  }
  if (any(is.na(time_series)) && interpolate) {
    time_series <- na_interpolation(time_series, option = "linear")
  }
  if (show_warns) {
    if (any(is.na(time_series))) {
      warning(paste("Data contains NAs, we strongly advise setting",
              "\"interpolate = FALSE\"",
              "for time_series containing large continuous chunks of NAs"))
    }
    n_months <- length(unique(format(dates, format = "%B%Y")))
    if (n_months < 120) {
      warning(sprintf(paste("Time series is shorter than recommended",
                            "(contains %s months); see ?predictability"),
                      n_months))
    }
    months <- format(dates, format = "%B")
    start_diffs_end <- months[1] != months[length(months)]
    if (start_diffs_end) {
      warning(sprintf(paste("Time series starts and ends at different",
                            "times of the year. Starting month is %s",
                            "and ending month is %s"),
                      months[1], months[length(months)]))
    }
  }

  detrended <- detrend_and_bin(time_series, dates)
  seasonality_list <- seasonality_calc(detrended$interpolated_seasons,
                                       detrended$resid_time_series)
  noise_list <- noise_calc(detrended$resid_time_series,
                           detrended$predictor,
                           noise_method)

  out <- data.frame(series_n = series_n,
                    n_na = n_na,
                    prop_na = n_na / series_n,
                    n_yrs = length(unique(format(dates, format = "%Y"))),
                    n_months = length(unique(format(dates, format = "%B"))),
                    n_days = length(unique(dates)),
                    frequency = 2 / (length(dates) * delta),
                    nyquist_freq = 1 / (2 * delta),
                    raw_mean = mean(time_series, na.rm = TRUE),
                    raw_var = var(time_series, na.rm  = TRUE),
                    raw_cv = sd(time_series, na.rm  = TRUE) / mean(time_series, na.rm  = TRUE),
                    predicted_var = seasonality_list$predicted_var,
                    unpredicted_var = seasonality_list$unpredicted_var,
                    unbounded_seasonality = seasonality_list$unbounded_seasonality,
                    bounded_seasonality = seasonality_list$bounded_seasonality,
                    env_col = noise_list$slope,
                    stringsAsFactors = FALSE)
  attr(out, "detrended_data") <- detrended
  attr(out, "noise_data") <- noise_list$spec_obj
  attr(out, "noise_model") <- noise_list$model
  class(out) <- append(class(out), "predictability")
  out
}
