context("Testing helper functions")

library(envPred)
data(sst)
data(npp)

# introduce NAs in complete SST series
sst2  <-  sst
sst2$time_series[sample(seq_len(nrow(sst2)), 50)]  <-  NA

# copy of time series starting and ending at the same day different years
sst3  <-  sst[1:which(sst$dates == as.Date("2007-01-01")), ]

test_that("Simple corner cases", {
    test_check <- env_stats(sst3$time_series, sst3$dates, n_states = 11, delta = 1, is_uneven = FALSE, interpolate = FALSE, show_warns = TRUE, noise_method = "spectrum")
    expect_true(is.envpreddata(test_check))
    expect_false(is.envpreddata(NULL))
    expect_false(is.envpreddata(NA))
    expect_false(is.envpreddata(""))
    expect_false(is.envpreddata(1))
    expect_false(is.envpreddata(TRUE))
    expect_false(is.envpreddata(FALSE))
    expect_false(is.envpreddata(data.frame(z = 10)))
    expect_false(is.envpreddata(list(z = 10)))
    expect_false(is.envpreddata(matrix(1:3, 1, 3)))

    ################
    # linear_detrend
    ################
    # returns correct structure both for complete or missing data
    fct <- envPred:::linear_detrend
    expect_is(fct(sst$time_series, sst$dates), "data.frame")
    expect_is(fct(npp$time_series, npp$dates), "data.frame")
    dat <- fct(sst$time_series, sst$dates)
    dat2 <- fct(npp$time_series, npp$dates)
    expect_true(nrow(dat) == nrow(sst))
    expect_true(all(names(dat) == c("predictor", "resids", "dates")))
    expect_length(dat, 3)
    expect_true(sum(is.na(dat2$resids)) == sum(is.na(npp$time_series)))

    # characters not allowed for date vector
    expect_is(fct(sst$time_series, as.character(sst$dates)), "data.frame")
    expect_error(fct(as.character(sst$time_series), sst$dates))
    expect_error(expect_identical(fct(sst$time_series, as.character(sst$dates)), fct(sst$time_series, sst$dates)))

    # vectors have different lengths
    expect_error(fct(sst$time_series[1:100], sst$dates))
    expect_error(fct(sst$time_series, sst$dates[1:100]))
    expect_error(fct(sst$time_series[-1], sst$dates))
    expect_error(fct(sst$time_series, sst$dates[-1]))

    # when input is incorrect
    expect_error(fct(NA, sst$dates))
    expect_error(fct(sst$time_series, NA))
    expect_error(fct(NA, NA))

    expect_error(fct(NULL, sst$dates))
    expect_error(fct(sst$time_series, NULL))
    expect_error(fct(NULL, NULL))

    # wrong time vector structure
    expect_error(fct(sst$time_series, format(sst$dates, format = "%d%m%Y")))
    expect_error(fct(sst$time_series, format(sst$dates, format = "%m-%Y")))
    expect_error(fct(sst$time_series, format(sst$dates, format = "%Y")))
    expect_error(fct(sst$time_series, format(sst$dates, format = "%d-%Y")))
    expect_false(all(fct(sst$time_series, format(sst$dates, format = "%d-%m-%Y")) == fct(sst$time_series, sst$dates)))

    ##############
    # monthly_bins
    ##############
    fct <- envPred:::monthly_bins
    expect_is(fct(dat$resids, sst$dates), "data.frame")
    expect_is(fct(dat2$resids, npp$dates), "data.frame")
    expect_is(fct(rpois(nrow(sst), lambda = 20), sst$dates), "data.frame")

    dat3 <- fct(dat$resids, sst$dates)
    dat4 <- fct(dat2$resids, npp$dates)
    expect_true(nrow(dat3) == nrow(sst))
    expect_true(all(names(dat3) == c("resid_time_series", "interpolated_seasons")))
    expect_length(dat3, 2)
    expect_true(sum(is.na(dat2$resids)) == sum(is.na(npp$time_series)))

    # characters not allowed for date vector
    expect_warning(expect_error(fct(as.character(dat$resids), sst$dates)))
    expect_error(fct(dat$resids, as.character(sst$dates)))

    # vectors have different lengths
    expect_error(fct(dat$resids[1:100], sst$dates))
    expect_error(fct(dat$resids, sst$dates[1:100]))
    expect_error(fct(dat$resids[-1], sst$dates))
    expect_error(fct(dat$resids, sst$dates[-1]))

    # when input is incorrect
    expect_error(fct(NA, sst$dates))
    expect_error(fct(dat$resids, NA))
    expect_error(fct(NA, NA))

    expect_error(fct(NULL, sst$dates))
    expect_error(fct(dat$resids, NULL))
    expect_error(fct(NULL, NULL))

    # wrong time vector structure
    expect_error(fct(dat$resids, format(sst$dates, format = "%d%m%Y")))
    expect_error(fct(dat$resids, format(sst$dates, format = "%m-%Y")))
    expect_error(fct(dat$resids, format(sst$dates, format = "%Y")))
    expect_error(fct(dat$resids, format(sst$dates, format = "%d-%Y")))
    expect_error(fct(dat$resids, format(sst$dates, format = "%d-%m-%Y")))

    #################
    # detrend_and_bin
    #################
    # dont run many tests as individual behaviours are already
    # probed via linear_detrend and monthly_bins separately
    fct <- envPred:::detrend_and_bin
    # returns correct structure both for complete or missing data
    expect_is(fct(sst$time_series, sst$dates), "data.frame")
    expect_is(fct(npp$time_series, npp$dates), "data.frame")
    dat5 <- fct(sst$time_series, sst$dates)
    dat6 <- fct(npp$time_series, npp$dates)
    expect_true(nrow(dat5) == nrow(sst))
    expect_true(all(names(dat5) == c("predictor", "resids", "dates", "resid_time_series", "interpolated_seasons")))
    expect_length(dat5, 5)
    expect_true(sum(is.na(dat6$resid_time_series)) == sum(is.na(npp$time_series)))

    ##########
    # last_day
    ##########
    fct <- envPred:::last_day
    # only returns something for class Date
    expect_is(fct(as.Date("2019-02-01")), "Date")
    expect_error(fct("2019-02-01"))
    expect_error(fct(""))
    expect_error(fct(NA))
    expect_error(fct(NULL))

    # return 29 of Feb correct years
    expect_equal(fct(as.Date("2020-02-01")), as.Date("2020-02-29"))

    # returns correct even if it is the last day f the month
    expect_equal(fct(as.Date("2020-03-31")), as.Date("2020-03-31"))
})
