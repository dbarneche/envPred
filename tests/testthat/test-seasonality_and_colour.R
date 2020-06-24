context("Calculating seasonality and colour")

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

test_that("Simple corner cases", {
    # returns correct structure
    expect_is(envPred:::seasonality_and_colour(sst3$time_series, sst3$dates, delta = 1, is_uneven = FALSE, interpolate = FALSE, show_warns = TRUE, noise_method = "spectrum"), "list")
    expect_is(envPred:::seasonality_and_colour(sst3$time_series, sst3$dates, delta = 1, is_uneven = FALSE, interpolate = FALSE, show_warns = TRUE, noise_method = "spectrum"), "list")
    expect_is(envPred:::seasonality_and_colour(sst3$time_series, sst3$dates, delta = 1, is_uneven = FALSE, interpolate = FALSE, show_warns = TRUE, noise_method = "spectrum"), "list")

    test_check <- envPred:::seasonality_and_colour(sst3$time_series, sst3$dates, delta = 1, is_uneven = FALSE, interpolate = FALSE, show_warns = TRUE, noise_method = "spectrum")
    expect_length(test_check, 3)
    expect_identical(names(test_check), c("data", "detrended_data", "noise_list" ))
    expect_is(test_check$data, "data.frame")
    expect_is(test_check$detrended_data, "data.frame")
    expect_is(test_check$noise_list, "list")

    # returns correct structure, but with warnings
    expect_warning(expect_is(envPred:::seasonality_and_colour(sst$time_series, sst$dates, delta = 1, is_uneven = FALSE, interpolate = FALSE, show_warns = TRUE, noise_method = "spectrum"), "list"))
    expect_warning(expect_is(envPred:::seasonality_and_colour(sst$time_series, sst$dates, delta = 1, is_uneven = FALSE, interpolate = FALSE, show_warns = TRUE, noise_method = "spectrum"), "list"))
    expect_warning(expect_is(envPred:::seasonality_and_colour(sst$time_series, sst$dates, delta = 1, is_uneven = FALSE, interpolate = FALSE, show_warns = TRUE, noise_method = "spectrum"), "list"))
    expect_warning(expect_is(envPred:::seasonality_and_colour(npp$time_series, npp$dates, delta = 8, is_uneven = TRUE, interpolate = FALSE, show_warns = TRUE, noise_method = "lomb_scargle"), "list"))
    expect_warning(expect_is(envPred:::seasonality_and_colour(npp$time_series, npp$dates, delta = 8, is_uneven = TRUE, interpolate = TRUE, show_warns = TRUE, noise_method = "lomb_scargle"), "list"))
    
    # returns correct structure, but with warnings
    expect_warning(envPred:::seasonality_and_colour(sst4$time_series, sst4$dates, delta = 1, is_uneven = FALSE, interpolate = FALSE, show_warns = TRUE, noise_method = "spectrum"))

    # returns error because method spectrum cannot be used with NAs
    expect_error(envPred:::seasonality_and_colour(sst2$time_series, sst2$dates, delta = 1, is_uneven = FALSE, interpolate = FALSE, show_warns = FALSE, noise_method = "spectrum"))
    expect_warning(expect_error(envPred:::seasonality_and_colour(sst2$time_series, sst2$dates, delta = 1, is_uneven = FALSE, interpolate = FALSE, show_warns = TRUE, noise_method = "spectrum")))
    
    # returns correct structure because interpolate = TRUE
    expect_is(envPred:::seasonality_and_colour(sst2$time_series, sst2$dates, delta = 1, is_uneven = FALSE, interpolate = TRUE, show_warns = TRUE, noise_method = "spectrum"), "list")

    # noise_method not specified should return error
    expect_error(envPred:::seasonality_and_colour(sst3$time_series, sst3$dates, delta = 1, is_uneven = FALSE, interpolate = FALSE, show_warns = TRUE), "argument \"noise_method\" is missing, with no default")

    # uneven time series passes for even. interpolate to remove NAs
    expect_is(envPred:::seasonality_and_colour(npp$time_series, npp$dates, delta = 8, is_uneven = FALSE, interpolate = TRUE, show_warns = FALSE, noise_method = "lomb_scargle"), "list")

    # uneven time series should be used with noise_method lomb_scargle, otherwise returns error
    expect_error(envPred:::seasonality_and_colour(npp$time_series, npp$dates, delta = 8, is_uneven = TRUE, interpolate = TRUE, show_warns = FALSE, noise_method = "spectrum"), "Time series is uneven, please use noise method lomb_scargle")

    # dateVector is not in progressive order
    expect_error(envPred:::seasonality_and_colour(sst3$time_series, rev(sst3$dates), delta = 1, is_uneven = FALSE, interpolate = FALSE, show_warns = FALSE, noise_method = "spectrum"))

    # dates is not of class Date
    expect_error(envPred:::seasonality_and_colour(sst3$time_series, rpois(nrow(sst3), lambda = 100), delta = 1, is_uneven = FALSE, interpolate = FALSE, show_warns = FALSE, noise_method = "spectrum"))
    expect_error(envPred:::seasonality_and_colour(sst3$time_series, sample(letters, nrow(sst3), replace = TRUE), delta = 1, is_uneven = FALSE, interpolate = FALSE, show_warns = FALSE, noise_method = "spectrum"))
    
    # rawTimeSeries and dates do not have same length
    expect_error(envPred:::seasonality_and_colour(sst3$time_series, sst3$dates[-1], delta = 1, is_uneven = FALSE, interpolate = FALSE, show_warns = TRUE, noise_method = "spectrum"))
    expect_error(envPred:::seasonality_and_colour(sst3$time_series[-1], sst3$dates, delta = 1, is_uneven = FALSE, interpolate = FALSE, show_warns = TRUE, noise_method = "spectrum"))

    # rawTimeSeries is not of numeric
    expect_warning(expect_error(envPred:::seasonality_and_colour(sample(letters, nrow(sst3), replace = TRUE), sst3$dates, delta = 1, is_uneven = FALSE, interpolate = FALSE, show_warns = TRUE, noise_method = "spectrum")))
    expect_warning(expect_error(envPred:::seasonality_and_colour(rep(NA, nrow(sst3)), sst3$dates, delta = 1, is_uneven = FALSE, interpolate = FALSE, show_warns = TRUE, noise_method = "spectrum")))
    expect_error(envPred:::seasonality_and_colour(rep(NULL, nrow(sst3)), sst3$dates, delta = 1, is_uneven = FALSE, interpolate = FALSE, show_warns = TRUE, noise_method = "spectrum"))

    # delta is not numeric
    expect_error(envPred:::seasonality_and_colour(sst3$time_series, sst3$dates, delta = "a", is_uneven = FALSE, interpolate = FALSE, show_warns = TRUE, noise_method = "spectrum"), "non-numeric argument to binary operator")
})
