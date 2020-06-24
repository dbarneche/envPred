#' Class \code{envpreddata} of environmental time series statistics calculated with the \pkg{envPred} package
#' 
#' Statistics calculated with the \code{\link[envPred:envPred]{envPred}} package are 
#' represented as a \code{envpreddata} object, which contains a data.frame with
#' descriptive summary of the time series vector (length, number of NAs, etc), the
#' mean, variance, and coefficient of variation of the environmental time series, and
#' seasonality, colour of environmental noise, constancy, contingency and predictability.
#' 
#' @name envpreddata-class
#' @aliases envpreddata
#' @docType class
#' 
#' @details See function \code{\link{env_stats}}.
#' 
#' @slot series_n Length of the time series.
#' @slot n_na Number of NAs in the time series.
#' @slot prop_na Proportion of NAs in the time series.
#' @slot n_yrs Number of years in the time series.
#' @slot n_months Number of months in the time series.
#' @slot n_days Number of days in the time series.
#' @slot frequency Frequency (\eqn{2 / (n \Delta t)}) of the time series, where \eqn{\Delta t} is the time gap between consecutive points in the time series, and \eqn{n} is the number of observations in the time series.
#' @slot nyquist_freq Nyquist frequency (\eqn{1 / (2 \Delta t)}) of the time series.
#' @slot raw_mean Arithmetic mean of the time series.
#' @slot raw_var Variance of the time series.
#' @slot raw_cv Coefficient of variation (standard deviation / mean) of the time series.
#' @slot predicted_var Variance of the seasonal trend. See \code{\link{env_stats}} for further explanations.
#' @slot unpredicted_var Variance of the residual time series (i.e. the time series after the seasonal trend was removed). See \code{\link{env_stats}} for further explanations.
#' @slot unbounded_seasonality Seasonality as var_predict / var_unpredict. See \code{\link{env_stats}} for further explanations.
#' @slot bounded_seasonality Seasonality as var_predict / (var_predict + var_unpredict). See \code{\link{env_stats}} for further explanations.
#' @slot env_col Colour of environmental noise. See \code{\link{env_stats}} for further explanations.
#' @slot colwell_c Colwell's constancy. See \code{\link{env_stats}} for further explanations.
#' @slot colwell_m Colwell's contingency. See \code{\link{env_stats}} for further explanations.
#' @slot colwell_p Colwell's predictability. See \code{\link{env_stats}} for further explanations.
#' 
#' @seealso 
#'   \code{\link{env_stats}}, 
#'   \code{\link{gg_envpred}}
#' 
NULL
