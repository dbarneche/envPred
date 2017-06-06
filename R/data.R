#' Sea Surface Temperature (SST) Time Series (1997-01-01 - 2007-12-31)
#'
#' A dataset containing raw time series for SST at one random coordinate.
#' This time series is evenly distributed with delta time = 1 day.
#' Data are extracted by Diego Barneche using the noaaErddap GitHub R package.
#'
#' @format A data frame with 4017 rows and 2 variables:
#' \itemize{
#'		\item rawTimeSeries: A numeric vector containing a raw environmental time series
#'		\item datesVector:  A vector of class Date of format YYYY-MM-DD
#' }
#' @name sst
#' @docType data
NULL

#' Net Primary Productivity (NPP) Time Series (1997-09-10 - 2008-01-05)
#'
#' A dataset containing raw time series for NPP at one random coordinate.
#' This time series is unevenly distributed with average delta time = 8 days.
#' Data are extracted by Diego Barneche using the noaaErddap GitHub R package.
#'
#' @format A data frame with 465 rows and 2 variables:
#' \itemize{
#'		\item rawTimeSeries: A numeric vector containing a raw environmental time series
#'		\item datesVector:  A vector of class Date of format YYYY-MM-DD
#' }
#' @name npp
#' @docType data
NULL