#' Plot predictability object
#'
#' @param object A \code{\link{predictability}} object.
#' @param type Either \code{detrended} (default) or \code{spectral}.
#' See Details.
#' @details Two types of plots are provided:
#' detrended: show the time series after linear
#' de-trending, including the seasonality interpolation.
#' spectral: The spectral power density as
#' a function of frequency.
#' @return A \code{\link[ggplot2]{ggplot}} object.
#' @importFrom dplyr %>% rename
#' @importFrom tidyr pivot_longer
#' @importFrom ggplot2 ggplot aes geom_line scale_colour_manual theme_bw labs theme coord_trans
#' @examples
#' \dontrun{
#' library(envPred)
#' data(sst)
#' dat <- predictability(sst$time_series,
#'                       sst$dates,
#'                       delta = 1,
#'                       is_uneven = FALSE,
#'                       interpolate = FALSE,
#'                       show_warns = TRUE,
#'                       noise_method = 'spectrum')
#'
#' gg_envpred(dat, type = "detrended")
#' gg_envpred(dat, type = "spectral")
#' }
#' @export
gg_envpred <- function(object, type = c("detrended", "spectral")) {
  if (missing(type)) {
    type <- "detrended"
  }

  switch(match.arg(type),
         "detrended" = {
           attr(object, "detrended_data") %>%
             dplyr::rename(a = resids,
                           b = interpolated_seasons) %>%
             tidyr::pivot_longer(c(a, b),
                                 names_to = "Variable",
                                 values_to = "value") %>%
             ggplot(aes(x = dates, y = value, colour = Variable)) +
               geom_line() +
               scale_colour_manual(values = c("tomato", "black"),
                                   labels = c("De-trended",
                                              "Interpolated seasonality")) +
               theme_bw() +
               labs(x = "Date", y = "De-trended response") +
               theme(legend.position = "bottom")
         },
         "spectral" = {
           attr(object, "noise_data") %>%
             ggplot(aes(x = freq, y = spec)) +
               geom_line(colour = "tomato") +
               theme_bw() +
               labs(x = "Frequency", y = "Spectral power density") +
               coord_trans(x = "log10", y = "log10")
         }
  )
}
