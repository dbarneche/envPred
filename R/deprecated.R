#' Seasonality and colour of environmental noise (deprecated)
#' 
#' Calculates seasonality and colour of environmental noise (deprecated).
#' This function will be removed in future versions of the package.
#' 
#' @param rawTimeSeries A \code{\link[base]{numeric}} vector containing 
#' a raw environmental time series.
#' @param datesVector An vector of class \code{\link[base]{Date}} of format YYYY-MM-DD 
#' (must be in progressive chronological order).
#' @param delta Time interval (any unit) of \code{rawTimeSeries}.
#' @param isUneven Is \code{rawTimeSeries} even or unevenly distributed in time? Default is \code{\link[base]{FALSE}}.
#' @param interpolate Should a linear interpolation be applied to missing values in 
#' \code{rawTimeSeries}? Irrelevant method if \code{isUneven} is \code{\link[base]{FALSE}}. Default is \code{\link[base]{FALSE}}.
#' @param checkPlots Deprecated. Use newer \code{\link{gg_envpred}} instead.
#' @param showWarnings Should cautionary warning messages be displayed? Default is \code{\link[base]{TRUE}}. 
#' Strongly recommended for first time users.
#' @param noiseMethod A method for estimating the slope beta. Takes 2 possible 
#' values: \code{'spectrum'} for evenly distributed time series or 
#' \code{'LombScargle'} for unevenly distributed ones.
#' 
#' @return A \code{\link[base]{data.frame}}.
#' 
#' @author Diego Barneche and Scott Burgess.
#' 
#' @seealso \code{\link{env_stats}}.
#' 
#' @export
envPredictability  <-  function(rawTimeSeries, datesVector, delta,
                                isUneven = FALSE, interpolate = FALSE,
                                checkPlots = FALSE, showWarnings = TRUE,
                                noiseMethod) {
  message("\"envPredictability\" is deprecated;",
          " use \"env_stats\" instead")
  noiseMethod  <-  ifelse(noiseMethod == "LombScargle", "lomb_scargle", noiseMethod)
  seasonality_and_colour(rawTimeSeries,
                         datesVector,
                         delta,
                         isUneven,
                         interpolate,
                         showWarnings,
                         noiseMethod)
}

#' Environmental time series statistics (deprecated)
#' 
#' Calculates seasonality, colour of environmental noise, constancy, contingency and predictability (deprecated).
#' This function will be removed in future versions of the package.
#' Use \code{\link{env_stats}} instead.
#'
#' @inherit env_stats details return
#' 
#' @inheritParams envPredictability
#' 
#' @param nStates is a \code{\link[base]{numeric}} vector of length 1 containing 
#' a somewhat arbitrary number, as Colwell's method divides a continuous variable
#' up into discrete states. Default (arbitrary) is 11. See \code{\link{colwell_stats}}
#' for Details.
#' 
#' @param ... Additional arguments to \code{\link{envPredictability}}.
#' 
#' @author Diego Barneche and Scott Burgess.
#' 
#' @export
envPredictabilityAndColwell  <-  function (rawTimeSeries, datesVector, nStates, ...) {
  message("\"envPredictabilityAndColwell\" is deprecated;",
          " use \"env_stats\" instead")
  env_stats(rawTimeSeries, datesVector, nStates, ...)
}
