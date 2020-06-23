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
#' @importFrom ggplot2 ggplot aes geom_line scale_colour_manual theme_bw labs theme
#' @importFrom ggplot2 scale_x_continuous scale_y_continuous labs
#' @importFrom stats predict coef
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
           data <- attr(object, "noise_data")
           lmod <- attr(object, "noise_model")
           r2_lmod <- round(summary(lmod)$r.squared, 3)
           preds_lmod <- data.frame(freq = seq(min(data$freq),
                                               max(data$freq),
                                               length.out = 50))
           preds_lmod <- preds_lmod %>%
              predict(lmod, newdata = .) %>%
              data.frame(Estimate = 10^., freq = preds_lmod$freq)
           fixefs_lmod <- coef(summary(lmod))           
           sig <- fixefs_lmod["log10(freq)", "Pr(>|t|)"]
           if (sig < 0.001) {
             p_val <- substitute(italic(p) < 0.001 * "; " * b,
                                 list(b = substitute(italic(R^2) == c, 
                                                     list(c = r2_lmod))))
           } else {
             p_val <- substitute(italic(p) == a * "; " * b,
                                 list(a = round(sig, 3),
                                      b = substitute(italic(R^2) == c, 
                                                     list(c = r2_lmod))))
           }
           data %>%
             ggplot(aes(x = freq, y = spec)) +
               geom_line(colour = "tomato") +
               geom_line(data = preds_lmod, aes(x = freq, y = Estimate), size = 0.8,
                         linetype = 2) +
               theme_bw() +
               labs(x = "Frequency", y = "Spectral power density") +
               scale_x_continuous(trans = "log10") +
               scale_y_continuous(trans = "log10") +
               labs(title = substitute(italic(y) == 10^a %.% italic(x)^b,
                                       list(a = round(fixefs_lmod["(Intercept)", "Estimate"], 2),
                                            b = round(fixefs_lmod["log10(freq)", "Estimate"], 2))),
                    subtitle = p_val)

         }
  )
}
