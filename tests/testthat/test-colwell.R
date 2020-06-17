context("Calculating Colwells metrics")

library(envPred)
data(sst)
data(npp)

# introduce NAs in complete SST series
sst2  <-  sst
set.seed(10)
sst2$time_series[sample(seq_len(nrow(sst2)), 600)]  <-  NA

# copy of time series starting and ending at the same day different years
sst3  <-  sst[1:which(sst$dates == as.Date("2007-01-01")), ]
n_states <- 11

test_that("Simple corner cases", {
    # predictability_and_colwell
    # dont run many tests as individual behaviours are already
    # probed via predictability and colwell separately
    # returns correct structure both for complete or missing data
    expect_is(predictability_and_colwell(sst3$time_series, sst3$dates, delta = 1, is_uneven = FALSE, interpolate = FALSE, show_warns = TRUE, noise_method = "spectrum", n_states), "data.frame")
    both <- predictability_and_colwell(sst3$time_series, sst3$dates, delta = 1, is_uneven = FALSE, interpolate = FALSE, show_warns = TRUE, noise_method = "spectrum", n_states)
    just_c <- colwell74(sst3$time_series, sst3$dates, n_states)
    expect_true(all(both[names(just_c)] == just_c))
    
    # returns correct structure both for complete or missing data
    expect_is(colwell74(npp$time_series, npp$dates, n_states), "data.frame")
    expect_is(colwell74(sst$time_series, sst$dates, n_states), "data.frame")
    expect_length(colwell74(sst$time_series, sst$dates, n_states), 3)
    expect_length(colwell74(npp$time_series, npp$dates, n_states), 3)
    expect_false(all(colwell74(sst$time_series, sst$dates, n_states) == colwell74(sst2$time_series, sst2$dates, n_states)))
    
    # characters allowed for date vector
    expect_is(colwell74(sst$time_series, as.character(sst$dates), 2), "data.frame")
    expect_equal(colwell74(sst$time_series, sst$dates, 2), colwell74(sst$time_series, as.character(sst$dates), 2))

    # vectors have different lengths
    expect_error(colwell74(sst$time_series[1:100], sst$dates, 2))
    expect_error(colwell74(sst$time_series, sst$dates[1:100], 2))
    expect_error(colwell74(sst$time_series[-1], sst$dates, 2))
    expect_error(colwell74(sst$time_series, sst$dates[-1], 2))

    # when n_states < 2
    expect_error(colwell74(sst$time_series, sst$dates, 0))
    expect_error(colwell74(sst$time_series, sst$dates, 1))

    # when input is incorrect
    expect_error(colwell74(NA, sst$dates, n_states))
    expect_error(colwell74(NA, NA, n_states))
    expect_error(colwell74(NA, NA, 0))
    expect_error(colwell74(NA, NA, NA))

    expect_error(colwell74(NULL, sst$dates, n_states))
    expect_error(colwell74(NULL, sst$dates, n_states))
    expect_error(colwell74(sst$time_series, sst$dates, NA))
    expect_error(colwell74(sst$time_series, sst$dates, NULL))
    expect_error(colwell74(NULL, NULL, NULL))

    expect_warning(expect_error(colwell74(as.character(sst$time_series), sst$dates, 2)))

    # wrong time vector structure
    expect_error(colwell74(sst$time_series, format(sst$dates, format = "%d%m%Y"), 2))
    expect_error(colwell74(sst$time_series, format(sst$dates, format = "%m-%Y"), 2))
    expect_error(colwell74(sst$time_series, format(sst$dates, format = "%Y"), 2))
    expect_error(colwell74(sst$time_series, format(sst$dates, format = "%d-%Y"), 2))
    expect_false(all(colwell74(sst$time_series, format(sst$dates, format = "%d-%m-%Y"), 2) == colwell74(sst$time_series, sst$dates, 2)))
})
