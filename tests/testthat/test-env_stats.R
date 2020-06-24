context("Main function")

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
    # dont run many tests as individual behaviours are already
    # probed via seasonality_and_colour and colwell_stats separately
    # returns correct structure both for complete or missing data
    expect_is(env_stats(sst3$time_series, sst3$dates, delta = 1, is_uneven = FALSE, interpolate = FALSE, show_warns = TRUE, noise_method = "spectrum", n_states), "data.frame")
    test_check <- env_stats(sst3$time_series, sst3$dates, delta = 1, is_uneven = FALSE, interpolate = FALSE, show_warns = TRUE, noise_method = "spectrum", n_states)
    test_check2 <- env_stats(sst3$time_series, sst3$dates, n_states = n_states, delta = 1, is_uneven = FALSE, interpolate = FALSE, show_warns = TRUE, noise_method = "spectrum")
    just_c <- envPred:::colwell_stats(sst3$time_series, sst3$dates, n_states)
    expect_true(all(test_check[names(just_c)] == just_c))
    expect_equal(test_check, test_check2)
})
