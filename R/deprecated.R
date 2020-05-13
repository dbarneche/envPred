#' @title Calculates environmental predictability components 
#' 
#' Environmental predictability components (deprecated).
#' Use \code{\link{predictability}} instead.
#'
#' @param rawTimeSeries A \code{\link[base]{numeric}} vector containing 
#' a raw environmental time series.
#' @param datesVector An vector of class \code{\link[base]{Date}} of format YYYY-MM-DD 
#' (must be in progressive chronological order).
#' @param delta Time interval (any unit) of \code{rawTimeSeries}.
#' @param isUneven Is \code{rawTimeSeries} even or unevenly distributed in time? Default is \code{\link[base]{FALSE}}.
#' @param interpolate Should a linear interpolation be applied to missing values in 
#' \code{rawTimeSeries}? Irrelevant method if \code{isUneven} is \code{\link[base]{FALSE}}. Default is \code{\link[base]{FALSE}}.
#' @param checkPlots Deprecated. Use newer \code{\link{gg_pred}} instead.
#' @param showWarnings Should cautionary warning messages be displayed? Default is \code{\link[base]{TRUE}}. 
#' Strongly recommended for first time users.
#' @param noiseMethod A method for estimating the slope beta. Takes 2 possible 
#' values: \code{'spectrum'} for evenly distributed time series or 
#' \code{'LombScargle'} for unevenly distributed ones.
#' @return A \code{\link[base]{data.frame}} with environmental predictability components.
#' @author Diego Barneche and Scott Burgess.
#' @seealso \code{\link{predictability}}.
#' @export
envPredictability  <-  function(rawTimeSeries, datesVector, delta,
                                isUneven = FALSE, interpolate = FALSE,
                                checkPlots = FALSE, showWarnings = TRUE,
                                noiseMethod) {
  message("\"envPredictability\" is deprecated;",
          " use \"predictability\" instead")
  noiseMethod  <-  ifelse(noiseMethod == "LombScargle", "lomb_scargle", noiseMethod)
  predictability(rawTimeSeries,
                 datesVector,
                 delta,
                 isUneven,
                 interpolate,
                 showWarnings,
                 noiseMethod)
}

#' @title Calculates environmental predictability components and Colwell (1974) metric
#' 
#' Environmental predictability components (deprecated).
#' Use \code{\link{predictability_and_colwell}} instead.
#'
#' @param rawTimeSeries A \code{\link[base]{numeric}} vector containing 
#' a raw environmental time series.
#' @param datesVector An vector of class \code{\link[base]{Date}} of format YYYY-MM-DD 
#' (must be in progressive chronological order).
#' @param nStates is a \code{\link[base]{numeric}} vector of length 1 containing 
#' a somewhat arbitrary number, as Colwell's method divides a continuous variable
#' up into discrete states. Default (arbitrary) is 11. See \code{\link{colwell74}}
#' for Details.
#' @param ... Additional arguments to \code{\link{envPredictability}}.
#' @details Wrapper function.
#' @return A \code{\link[base]{data.frame}} with environmental predictability components and 
#' Colwell (1974) metric.
#' @author Diego Barneche and Scott Burgess.
#' @seealso \code{\link{envPredictability}},\code{\link{colwell74}}.
#' @export
envPredictabilityAndColwell  <-  function (rawTimeSeries, datesVector, nStates, ...) {
  message("\"envPredictabilityAndColwell\" is deprecated;",
          " use \"predictability_and_colwell\" instead")
  predictability_and_colwell(rawTimeSeries, datesVector, nStates, ...)
}
