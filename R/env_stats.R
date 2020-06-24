#' Environmental time series statistics
#'
#' Calculates seasonality, colour of environmental noise, constancy, contingency and predictability.
#' 
#' @inheritDotParams seasonality_and_colour
#' 
#' @inherit seasonality_and_colour params
#' 
#' @param n_states is a \code{\link[base]{numeric}} vector of length 1 containing 
#' a somewhat arbitrary number, as Colwell's method divides a continuous variable
#' up into discrete states. Default (arbitrary) is 11. See Details.
#' 
#' @param ... Additional arguments to (internal) function \code{\link{seasonality_and_colour}}.
#' 
#' @details
#' To calculate seasonality, we first remove linear trends by extracting the residuals from a linear regression model fitted to the raw time series. Seasonality is estimated in two forms: 1) as the "unbounded" fraction of the total variance that is due to predictable seasonal periodicities, \eqn{\alpha / \beta}, where \eqn{\alpha} is the variance of the seasonal trend, and \eqn{\beta} is the variance of the residual time series (i.e. the time series after the seasonal trend was removed); or 2) as the "bounded" fraction of the total variance that is due to predictable seasonal periodicities, \eqn{\alpha / (\alpha + \beta)}. The seasonal trend is estimated by binning the time-series data into monthly intervals, averaging each month across the duration of the time series, then re-creating a seasonal time-series dataset on the same time-scale as the original data using a linear interpolation between the monthly midpoints.
#' 
#' To calculate colour, we calculate a residual time series by subtracting the corresponding seasonal value from each data point in the time series. The spectral density (i.e. variance in the residual time series) was assumed to scale with frequency, \eqn{f}, according to an inverse power law, \eqn{1/f^{\theta}} (\href{https://www.sciencedirect.com/science/article/pii/S0040580999914247}{Halley & Kunin, 1999}; \href{https://esajournals.onlinelibrary.wiley.com/doi/10.1890/02-3122}{Vasseur & Yodzis, 2004}). The spectral exponent \eqn{\theta} is then estimated as the negative slope of the linear regression of the natural log of spectral density as a function of the natural log of frequency. White noise occurs when there is no correlation between one measurement and the next (i.e. \eqn{\theta = 0}), while for reddened noise, there is some correlation between measurements separated by a finite time-scale (i.e. \eqn{\theta > 0}). Spectral density is estimated using the \code{\link[stats]{spectrum}} function from the \pkg{stats} R package if the time series is evenly distributed, and the Lomb–Scargle function \code{\link[lomb]{lsp}} from the \pkg{lomb} R package if the time series is unevenly distributed (\href{https://academic.oup.com/bioinformatics/article/22/3/310/220284}{Glynn et al. 2006}). Spectral densities and therefore \eqn{\theta} are calculated between the frequencies of \eqn{2 / (n \Delta t)} and \eqn{1 / (2 \Delta t)} (i.e. Nyquist frequency), where \eqn{\Delta t} is the time gap between consecutive points in the time series, and \eqn{n} is the number of observations in the time series.
#' 
#' For predictability, constancy, and contingency, this algorithm implements the methods described in \href{https://onlinelibrary.wiley.com/doi/10.2307/1940366/abstract}{Colwell (1974)} Ecology 55: 1148–1153, doi: \href{http://dx.doi.org/10.2307/1940366}{10.2307/1940366}. These three metrics can be calculated from any phenomenon known or suspected to be periodic or cyclic in time which can be scored for at least two states. Scores from each month in the cycle are then collected for as many complete cycles as possible, and the data cast as a frequency matrix with months as columns, and states as rows. Constancy (C) measures the extent to which the environment is the same for all months in all years.  Contingency (M) measures the extent to which the environmental differences between months are the same in all years. Predictability (P) is the sum of Constancy (C) and Contingency (M). Maximum predictability can be attained as a consequence of either complete constancy, complete contingency, or a combination of constancy and contingency.
#' 
#' General important notes:
#' \itemize{
#'    \item This function currently allows for monthly-based seasonality calculation only. This algorithm adapts the steps described in \href{http://onlinelibrary.wiley.com/doi/10.1111/ele.12402/abstract}{Marshall and Burgess (2015)} Ecology Letters 18: 1461–0248, doi: \href{http://dx.doi.org/10.1111/ele.12402}{10.1111/ele.12402}.
#'    \item We advise caution for datasets in which \code{time_series} contains NAs. Particularly, we strongly advise setting \code{interpolate = FALSE} for \code{time_series} containing large continuous chunks of NAs because the linear interpolation might introduce substantial bias.
#'    \item \href{http://onlinelibrary.wiley.com/doi/10.1890/02-3122/abstract}{Vasseur and Yodzis (2004)} Ecology 85: 1146-1152, doi: \href{http://dx.doi.org/10.1890/02-3122}{10.1890/02-3122} recommend time series encompassing at least 128 months or approximately 10 years.
#'    \item Also note that if \code{dates} starts and ends in different month of the year, some monthly means are estimated with relatively less precision.
#' }
#' 
#' @return An \code{\link[base]{data.frame}} of class \code{\link{envpreddata}}. See \code{?envpreddata} for output details.
#' 
#' @author Diego Barneche and Scott Burgess.
#' 
#' @examples
#' library(envPred)
#' data(sst)
#' data(npp)
#' # all results return important
#' # warning messages of which the user should be aware.
#' # after double-checking that the data falls
#' # within the recommended specs, set show_warns = FALSE
#' env_stats(sst$time_series, sst$dates, n_states = 11, delta = 1,
#'           is_uneven = FALSE, interpolate = FALSE, show_warns = TRUE,
#'           noise_method = 'spectrum')
#' 
#' env_stats(sst$time_series, sst$dates, n_states = 11, delta = 1,
#'           is_uneven = FALSE, interpolate = FALSE, show_warns = TRUE,
#'           noise_method = 'spectrum')
#' 
#' env_stats(sst$time_series, sst$dates, n_states = 11, delta = 1,
#'           is_uneven = FALSE, interpolate = FALSE, show_warns = TRUE,
#'           noise_method = 'spectrum')
#' 
#' env_stats(npp$time_series, npp$dates, n_states = 11, delta = 8,
#'           is_uneven = TRUE, interpolate = FALSE, show_warns = TRUE,
#'           noise_method = 'lomb_scargle')
#' 
#' env_stats(npp$time_series, npp$dates, n_states = 11, delta = 8,
#'           is_uneven = TRUE, interpolate = TRUE, show_warns = TRUE,
#'           noise_method = 'lomb_scargle')
#' 
#' @export
env_stats <- function(time_series, dates, n_states, ...) {
  sc_tab <- seasonality_and_colour(time_series, dates, ...)
  colwell_tab <- colwell_stats(time_series, dates, n_states)
  out <- cbind(sc_tab$data, colwell_tab)
  attr(out, "detrended_data") <- sc_tab$detrended_data
  attr(out, "noise_data") <- sc_tab$noise_list$spec_obj
  attr(out, "noise_model") <- sc_tab$noise_list$model
  class(out) <- append(class(out), "envpreddata")
  out
}
