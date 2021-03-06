#' The 'envPred' package.
#'
#' @description 'envPred' provides user-friendly functions to calculate five statistics from environmental time-series data: seasonality, the colour of environmental noise, constancy, contingency and predictability. These statistics can be calculated using the main function \code{\link{env_stats}}, which takes both evenly/unevenly-distributed, complete/incomplete date environmental time series.
#' 
#' Seasonality entails the regularity in the timing and magnitude of fluctuations in the average environmental state over seasons. Colour is defined by how predictable and similar the environment is between successive time points, or how far into the future the environmental state is likely to stay the same, independent of the mean environmental state. Constancy measures the extent to which the environment is the same for all months in all years. Contingency measures the extent to which the environmental differences between months are the same in all years. Predictability is the sum of constancy and contingency. Maximum predictability can be attained as a consequence of either complete constancy, complete contingency, or a combination of constancy and contingency.
#' 
#' The user can visually inspect whether the seasonal interpolation and the colour of environmental noise are appropriate to the input data using a ggplot2 wrapper \code{\link{gg_envpred}}.
#'
#' @docType package
#' @name envPred-package
#' @aliases envPred
#' 
#' @references
#' Barneche DR, Burgess SC & Marshall DJ (2018) Global
#' environmental drivers of marine fish egg size.
#' Global Ecology and Biogeography, 27(8), 890-898.
#' doi: 10.1111/geb.12748
#' 
#' Colwell RK (1974) Predictability, constancy, and contingency of
#' periodic phenomena. Ecology, 55(5), 1148-1153. 
#' doi: 10.2307/1940366
#'
#' Marshall DJ & Burgess SC (2015) Deconstructing environmental
#' predictability: seasonality, environmental colour and the
#' biogeography of marine life histories. Ecology Letters,
#' 18(2), 174-181. doi: 10.1111/ele.12402
#' 
#' Vasseur DA & Yodzis P (2004) The color of environmental noise.
#' Ecology, 85(4), 1146-1152. doi: 10.1890/02-3122
#'
NULL
