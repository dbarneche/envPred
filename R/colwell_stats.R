#' Environmental time series metrics from Colwell (1974)
#'
#' Calculates Constancy (C), Contingency (M), and Predictability (P) metrics from Colwell (1974)
#' for seasonally varying environmental data.
#' 
#' @inheritParams env_stats
#' 
#' @return A \code{\link[base]{data.frame}}.
#' 
#' @author Diego Barneche and Scott Burgess.
#' 
#' @seealso \code{\link{seasonality_and_colour}}, \code{\link{env_stats}}.
#' 
#' @importFrom stats aggregate
#' 
#' @examples
#' library(envPred)
#' data(sst)
#' data(npp)
#' n_states <- 11
#' fct <- envPred:::colwell_stats
#' fct(sst$time_series, sst$dates, n_states)
#' fct(npp$time_series, npp$dates, n_states)
colwell_stats <- function(time_series, dates, n_states = 11) {
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
