context("Deprecated main functions")

library(envPred)
data(sst)
data(npp)

# copy of time series starting and ending at the same day different years
sst3  <-  sst[1:which(sst$dates == as.Date("2007-01-01")), ]

# introduce NAs in complete SST series
sst2  <-  sst3
sst2$time_series[sample(seq_len(nrow(sst2)), 50)]  <-  NA

# copy of short time series to show warning
sst4  <-  sst[1:500, ]
n_states <- 11

test_that("Simple corner cases", {
    # dont run many tests as individual behaviours are already
    # probed via seasonality_and_colour and colwell_stats separately
    # returns correct structure both for complete or missing data
    expect_message(expect_is(envPredictabilityAndColwell(sst3$time_series, sst3$dates, delta = 1, is_uneven = FALSE, interpolate = FALSE, show_warns = TRUE, noise_method = "spectrum", n_states), "data.frame"))
    expect_message(test_check <- envPredictabilityAndColwell(sst3$time_series, sst3$dates, delta = 1, is_uneven = FALSE, interpolate = FALSE, show_warns = TRUE, noise_method = "spectrum", n_states))
    expect_message(test_check2 <- envPredictabilityAndColwell(sst3$time_series, sst3$dates, nStates = n_states, delta = 1, is_uneven = FALSE, interpolate = FALSE, show_warns = TRUE, noise_method = "spectrum"))
    just_c <- envPred:::colwell_stats(sst3$time_series, sst3$dates, n_states)
    expect_true(all(test_check[names(just_c)] == just_c))
    expect_equal(test_check, test_check2)

    # returns correct structure
    expect_message(expect_is(envPredictability(sst3$time_series, sst3$dates, delta = 1, isUneven = FALSE, interpolate = FALSE, checkPlots = FALSE, showWarnings = TRUE, noiseMethod = "spectrum"), "list"))
    expect_message(expect_is(envPredictability(sst3$time_series, sst3$dates, delta = 1, isUneven = FALSE, interpolate = FALSE, checkPlots = FALSE, showWarnings = TRUE, noiseMethod = "spectrum"), "list"))
    expect_message(expect_is(envPredictability(sst3$time_series, sst3$dates, delta = 1, isUneven = FALSE, interpolate = FALSE, checkPlots = FALSE, showWarnings = TRUE, noiseMethod = "spectrum"), "list"))

    # returns correct structure, but with warnings
    expect_message(expect_warning(expect_is(envPredictability(sst$time_series, sst$dates, delta = 1, isUneven = FALSE, interpolate = FALSE, checkPlots = FALSE, showWarnings = TRUE, noiseMethod = "spectrum"), "list")))
    expect_message(expect_warning(expect_is(envPredictability(sst$time_series, sst$dates, delta = 1, isUneven = FALSE, interpolate = FALSE, checkPlots = FALSE, showWarnings = TRUE, noiseMethod = "spectrum"), "list")))
    expect_message(expect_warning(expect_is(envPredictability(sst$time_series, sst$dates, delta = 1, isUneven = FALSE, interpolate = FALSE, checkPlots = FALSE, showWarnings = TRUE, noiseMethod = "spectrum"), "list")))
    expect_message(expect_warning(expect_is(envPredictability(npp$time_series, npp$dates, delta = 8, isUneven = TRUE, interpolate = FALSE, checkPlots = TRUE, showWarnings = TRUE, noiseMethod = "LombScargle"), "list")))
    expect_message(expect_warning(expect_is(envPredictability(npp$time_series, npp$dates, delta = 8, isUneven = TRUE, interpolate = TRUE, checkPlots = TRUE, showWarnings = TRUE, noiseMethod = "LombScargle"), "list")))
    
    # returns correct structure, but with warnings
    expect_message(expect_warning(envPredictability(sst4$time_series, sst4$dates, delta = 1, isUneven = FALSE, interpolate = FALSE, checkPlots = FALSE, showWarnings = TRUE, noiseMethod = "spectrum")))

    # returns error because method spectrum cannot be used with NAs
    expect_message(expect_error(envPredictability(sst2$time_series, sst2$dates, delta = 1, isUneven = FALSE, interpolate = FALSE, checkPlots = FALSE, showWarnings = FALSE, noiseMethod = "spectrum")))
    expect_message(expect_warning(expect_error(envPredictability(sst2$time_series, sst2$dates, delta = 1, isUneven = FALSE, interpolate = FALSE, checkPlots = FALSE, showWarnings = TRUE, noiseMethod = "spectrum"))))
    
    # returns correct structure because interpolate = TRUE
    expect_message(expect_is(envPredictability(sst2$time_series, sst2$dates, delta = 1, isUneven = FALSE, interpolate = TRUE, checkPlots = FALSE, showWarnings = TRUE, noiseMethod = "spectrum"), "list"))

    # noiseMethod not specified should return error
    expect_message(expect_error(envPredictability(sst3$time_series, sst3$dates, delta = 1, isUneven = FALSE, interpolate = FALSE, checkPlots = FALSE, showWarnings = TRUE), "argument \"noiseMethod\" is missing, with no default"))

    # uneven time series passes for even. interpolate to remove NAs
    expect_message(expect_is(envPredictability(npp$time_series, npp$dates, delta = 8, isUneven = FALSE, interpolate = TRUE, checkPlots = FALSE, showWarnings = FALSE, noiseMethod = "LombScargle"), "list"))

    # uneven time series should be used with noiseMethod LombScargle, otherwise returns error
    expect_message(expect_error(envPredictability(npp$time_series, npp$dates, delta = 8, isUneven = TRUE, interpolate = TRUE, checkPlots = FALSE, showWarnings = FALSE, noiseMethod = "spectrum"), "Time series is uneven, please use noise method lomb_scargle"))

    # dateVector is not in progressive order
    expect_message(expect_error(envPredictability(sst3$time_series, rev(sst3$dates), delta = 1, isUneven = FALSE, interpolate = FALSE, checkPlots = FALSE, showWarnings = FALSE, noiseMethod = "spectrum")))

    # dates is not of class Date
    expect_message(expect_error(envPredictability(sst3$time_series, rpois(nrow(sst3), lambda = 100), delta = 1, isUneven = FALSE, interpolate = FALSE, checkPlots = FALSE, showWarnings = FALSE, noiseMethod = "spectrum")))
    expect_message(expect_error(envPredictability(sst3$time_series, sample(letters, nrow(sst3), replace = TRUE), delta = 1, isUneven = FALSE, interpolate = FALSE, checkPlots = FALSE, showWarnings = FALSE, noiseMethod = "spectrum")))
    
    # rawTimeSeries and dates do not have same length
    expect_message(expect_error(envPredictability(sst3$time_series, sst3$dates[-1], delta = 1, isUneven = FALSE, interpolate = FALSE, checkPlots = FALSE, showWarnings = TRUE, noiseMethod = "spectrum")))
    expect_message(expect_error(envPredictability(sst3$time_series[-1], sst3$dates, delta = 1, isUneven = FALSE, interpolate = FALSE, checkPlots = FALSE, showWarnings = TRUE, noiseMethod = "spectrum")))

    # rawTimeSeries is not of numeric
    expect_message(expect_warning(expect_error(envPredictability(sample(letters, nrow(sst3), replace = TRUE), sst3$dates, delta = 1, isUneven = FALSE, interpolate = FALSE, checkPlots = FALSE, showWarnings = TRUE, noiseMethod = "spectrum"))))
    expect_message(expect_warning(expect_error(envPredictability(rep(NA, nrow(sst3)), sst3$dates, delta = 1, isUneven = FALSE, interpolate = FALSE, checkPlots = FALSE, showWarnings = TRUE, noiseMethod = "spectrum"))))
    expect_message(expect_error(envPredictability(rep(NULL, nrow(sst3)), sst3$dates, delta = 1, isUneven = FALSE, interpolate = FALSE, checkPlots = FALSE, showWarnings = TRUE, noiseMethod = "spectrum")))

    # delta is not numeric
    expect_message(expect_error(envPredictability(sst3$time_series, sst3$dates, delta = "a", isUneven = FALSE, interpolate = FALSE, checkPlots = FALSE, showWarnings = TRUE, noiseMethod = "spectrum"), "non-numeric argument to binary operator"))
})
