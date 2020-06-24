#' Seasonality and colour of environmental noise
#'
#' Calculates seasonality and colour of environmental noise.
#' 
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
#' 
#' @return A \code{\link[base]{data.frame}}.
#' 
#' @author Diego Barneche and Scott Burgess.
#' 
#' @seealso \code{\link{env_stats}}, \code{\link{seasonality_calc}}, \code{\link{noise_calc}}.
#' 
#' @examples
#' library(envPred)
#' data(sst)
#' # all results return important
#' # warning messages of which the user should be aware.
#' # after double-checking that the data falls
#' # within the recommended specs, set show_warns = FALSE
#' fct <- envPred:::seasonality_and_colour
#' fct(sst$time_series,
#'     sst$dates,
#'     delta = 1,
#'     is_uneven = FALSE,
#'     interpolate = FALSE,
#'     show_warns = TRUE,
#'     noise_method = 'spectrum')
#' fct(sst$time_series,
#'     sst$dates,
#'     delta = 1,
#'     is_uneven = FALSE,
#'     interpolate = FALSE,
#'     show_warns = TRUE,
#'     noise_method = 'spectrum')
#' fct(sst$time_series,
#'     sst$dates,
#'     delta = 1,
#'     is_uneven = FALSE,
#'     interpolate = FALSE,
#'     show_warns = TRUE,
#'     noise_method = 'spectrum')
#' data(npp)
#' fct(npp$time_series,
#'     npp$dates,
#'     delta = 8,
#'     is_uneven = TRUE,
#'     interpolate = FALSE,
#'     show_warns = TRUE,
#'     noise_method = 'lomb_scargle')
#' fct(npp$time_series,
#'     npp$dates,
#'     delta = 8,
#'     is_uneven = TRUE,
#'     interpolate = TRUE,
#'     show_warns = TRUE,
#'     noise_method = 'lomb_scargle')
#' @importFrom imputeTS na_interpolation
#' @importFrom stats var sd
seasonality_and_colour <- function(time_series, dates, delta, is_uneven = FALSE,
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
                            "(contains %s months); see ?seasonality_and_colour"),
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
  list(data = out,
       detrended_data = detrended,
       noise_list = noise_list)
}
