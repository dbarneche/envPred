#' Environmental predictability components
#'
#' @title Calculates environmental predictability components and Colwell (1974) metric
#' @param time_series A \code{\link[base]{numeric}} vector containing 
#' a raw environmental time series.
#' @param dates An vector of class \code{\link[base]{Date}} of format YYYY-MM-DD 
#' (must be in progressive chronological order).
#' @param n_states is a \code{\link[base]{numeric}} vector of length 1 containing 
#' a somewhat arbitrary number, as Colwell's method divides a continuous variable
#' up into discrete states. Default (arbitrary) is 11. See \code{\link{colwell74}}
#' for Details.
#' @param ... Additional arguments to \code{\link{predictability}}.
#' @details Wrapper function.
#' @return A \code{\link[base]{data.frame}} with environmental predictability components and 
#' Colwell (1974) metric.
#' @author Diego Barneche and Scott Burgess.
#' @seealso \code{\link{predictability}},\code{\link{colwell74}}.
#' @export
predictability_and_colwell <- function(time_series, dates, n_states, ...) {
  predic_tab <- predictability(time_series, dates, ...)
  colwell_stats <- colwell74(time_series, dates, n_states)
  cbind(predic_tab, colwell_stats)
}

#' Predictability from Colwell (1974)
#'
#' @title Calculates Constancy (C), Contingency (M), and Predictability (P) metrics from Colwell (1974)
#' for seasonally varying environmental data. Can be applied to any phenomenon known or suspected to be
#' periodic or cyclic in time which can be scored for at least two states. Scores from each month in the
#' cycle are then collected for as many complete cycles as possible, and the data cast as a frequency
#' matrix with months as columns, and states as rows. See Colwell (1974) for more information.
#' @param time_series A \code{\link[base]{numeric}} vector containing 
#' a raw environmental time series.
#' @param dates An vector of class \code{\link[base]{Date}} of format YYYY-MM-DD 
#' (must be in progressive chronological order).
#' @param n_states is a \code{\link[base]{numeric}} vector of length 1 containing 
#' a somewhat arbitrary number, as Colwell's method divides a continuous variable
#' up into discrete states. Default (arbitrary) is 11. See Details.
#' @details This algorithm implements the methods described in \href{http://onlinelibrary.wiley.com/doi/10.2307/1940366/abstract}{Colwell (1974)} Ecology 55: 1148–1153, doi: \href{http://dx.doi.org/10.2307/1940366}{10.2307/1940366}.
#' @return A \code{\link[base]{data.frame}} with three environmental predictability components:
#' constancy (\code{colwell_c}), contingency (\code{colwell_m}) and predictability (\code{colwell_p}).
#' Constancy (C) measures the extent to which the environment is the same for all months in all years. 
#' Contingency (M) measures the extent to which the environmental differences between months are the same 
#' in all years. Predictability (P) is the sum of Constancy (C) and Contingency (M). Maximum predictability 
#' can be attained as a consequence of either complete constancy, complete contingency, or a combination 
#' of constancy and contingency.
#' @author Diego Barneche and Scott Burgess.
#' @seealso \code{\link{predictability}}.
#' @examples
#' library(envPred)
#' data(sst)
#' data(npp)
#' n_states <- 11
#' colwell74(sst$time_series, sst$dates, n_states)
#' colwell74(npp$time_series, npp$dates, n_states)
#' @export
colwell74 <- function(time_series, dates, n_states = 11) {
  dat <- data.frame(dates = dates)
  dat$month <- factor(strftime(dat$dates, format = "%m"))
  dat$year <- factor(strftime(dat$dates, format = "%Y"))
  dat$y_raw  <- time_series
  month_av_yrs <- aggregate(y_raw ~ month + year, dat, mean, na.rm = TRUE)
  month_av_yrs$breaks <- cut(month_av_yrs$y_raw,
                             n_states, right = FALSE,
                             include.lowest = TRUE)
  colwell_mat <- with(month_av_yrs, table(breaks, month))

  x <- colSums(colwell_mat, na.rm = TRUE)
  y <- rowSums(colwell_mat, na.rm = TRUE)
  z <- sum(colwell_mat, na.rm = TRUE)
  hx <- -sum((x / z) * log2(x / z), na.rm = TRUE)
  hy <- -sum((y / z) * log2(y / z), na.rm = TRUE)
  hxy <- -sum((colwell_mat / z) * log2(colwell_mat / z), na.rm = TRUE)
  colwell_c <- 1 - (hy / log2(n_states))
  colwell_m <- (hx + hy - hxy) / log2(n_states)
  colwell_p <- colwell_c + colwell_m
  data.frame(colwell_c  =  colwell_c,
             colwell_m  =  colwell_m,
             colwell_p  =  colwell_p
  )
}
