context("Calculating Colwell's metrics")

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
    # returns correct structure both for complete or missing data
    expect_is(envPred:::colwell_stats(npp$time_series, npp$dates, n_states), "data.frame")
    expect_is(envPred:::colwell_stats(sst$time_series, sst$dates, n_states), "data.frame")
    expect_length(envPred:::colwell_stats(sst$time_series, sst$dates, n_states), 3)
    expect_length(envPred:::colwell_stats(npp$time_series, npp$dates, n_states), 3)
    expect_false(all(envPred:::colwell_stats(sst$time_series, sst$dates, n_states) == envPred:::colwell_stats(sst2$time_series, sst2$dates, n_states)))
    
    # characters allowed for date vector
    expect_is(envPred:::colwell_stats(sst$time_series, as.character(sst$dates), 2), "data.frame")
    expect_equal(envPred:::colwell_stats(sst$time_series, sst$dates, 2), envPred:::colwell_stats(sst$time_series, as.character(sst$dates), 2))

    # vectors have different lengths
    expect_error(envPred:::colwell_stats(sst$time_series[1:100], sst$dates, 2))
    expect_error(envPred:::colwell_stats(sst$time_series, sst$dates[1:100], 2))
    expect_error(envPred:::colwell_stats(sst$time_series[-1], sst$dates, 2))
    expect_error(envPred:::colwell_stats(sst$time_series, sst$dates[-1], 2))

    # when n_states < 2
    expect_error(envPred:::colwell_stats(sst$time_series, sst$dates, 0))
    expect_error(envPred:::colwell_stats(sst$time_series, sst$dates, 1))

    # when input is incorrect
    expect_error(envPred:::colwell_stats(NA, sst$dates, n_states))
    expect_error(envPred:::colwell_stats(NA, NA, n_states))
    expect_error(envPred:::colwell_stats(NA, NA, 0))
    expect_error(envPred:::colwell_stats(NA, NA, NA))

    expect_error(envPred:::colwell_stats(NULL, sst$dates, n_states))
    expect_error(envPred:::colwell_stats(NULL, sst$dates, n_states))
    expect_error(envPred:::colwell_stats(sst$time_series, sst$dates, NA))
    expect_error(envPred:::colwell_stats(sst$time_series, sst$dates, NULL))
    expect_error(envPred:::colwell_stats(NULL, NULL, NULL))

    expect_warning(expect_error(envPred:::colwell_stats(as.character(sst$time_series), sst$dates, 2)))

    # wrong time vector structure
    expect_error(envPred:::colwell_stats(sst$time_series, format(sst$dates, format = "%d%m%Y"), 2))
    expect_error(envPred:::colwell_stats(sst$time_series, format(sst$dates, format = "%m-%Y"), 2))
    expect_error(envPred:::colwell_stats(sst$time_series, format(sst$dates, format = "%Y"), 2))
    expect_error(envPred:::colwell_stats(sst$time_series, format(sst$dates, format = "%d-%Y"), 2))
    expect_false(all(envPred:::colwell_stats(sst$time_series, format(sst$dates, format = "%d-%m-%Y"), 2) == envPred:::colwell_stats(sst$time_series, sst$dates, 2)))
})
