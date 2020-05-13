context('Calculating environmental predictability components')

library(envPred)
data(sst)
data(npp)

# copy of time series starting and ending at the same day different years
sst3  <-  sst[1:which(sst$dates == as.Date('2007-01-01')), ]

# introduce NAs in complete SST series
sst2  <-  sst3
sst2$time_series[sample(1:nrow(sst2), 50)]  <-  NA

# copy of short time series to show warning
sst4  <-  sst[1:500, ]

test_that('Simple corner cases', {
    # returns correct structure
    expect_is(predictability(sst3$time_series, sst3$dates, delta = 1, is_uneven = FALSE, interpolate = FALSE, show_warns = TRUE, noise_method = 'spectrum'), 'data.frame')
    expect_is(predictability(sst3$time_series, sst3$dates, delta = 1, is_uneven = FALSE, interpolate = FALSE, show_warns = TRUE, noise_method = 'spectrum'), 'data.frame')
    expect_is(predictability(sst3$time_series, sst3$dates, delta = 1, is_uneven = FALSE, interpolate = FALSE, show_warns = TRUE, noise_method = 'spectrum'), 'data.frame')

    # returns correct structure, but with warnings
    expect_warning(expect_is(predictability(sst$time_series, sst$dates, delta = 1, is_uneven = FALSE, interpolate = FALSE, show_warns = TRUE, noise_method = 'spectrum'), 'data.frame'))
    expect_warning(expect_is(predictability(sst$time_series, sst$dates, delta = 1, is_uneven = FALSE, interpolate = FALSE, show_warns = TRUE, noise_method = 'spectrum'), 'data.frame'))
    expect_warning(expect_is(predictability(sst$time_series, sst$dates, delta = 1, is_uneven = FALSE, interpolate = FALSE, show_warns = TRUE, noise_method = 'spectrum'), 'data.frame'))
    expect_warning(expect_is(predictability(npp$time_series, npp$dates, delta = 8, is_uneven = TRUE, interpolate = FALSE, show_warns = TRUE, noise_method = 'lomb_scargle'), 'data.frame'))
    expect_warning(expect_is(predictability(npp$time_series, npp$dates, delta = 8, is_uneven = TRUE, interpolate = TRUE, show_warns = TRUE, noise_method = 'lomb_scargle'), 'data.frame'))
    
    # returns correct structure, but with warnings
    expect_warning(predictability(sst4$time_series, sst4$dates, delta = 1, is_uneven = FALSE, interpolate = FALSE, show_warns = TRUE, noise_method = 'spectrum'))

    # returns error because method spectrum cannot be used with NAs
    expect_error(predictability(sst2$time_series, sst2$dates, delta = 1, is_uneven = FALSE, interpolate = FALSE, show_warns = FALSE, noise_method = 'spectrum'))
    expect_warning(expect_error(predictability(sst2$time_series, sst2$dates, delta = 1, is_uneven = FALSE, interpolate = FALSE, show_warns = TRUE, noise_method = 'spectrum')))
    
    # returns correct structure because interpolate = TRUE
    expect_is(predictability(sst2$time_series, sst2$dates, delta = 1, is_uneven = FALSE, interpolate = TRUE, show_warns = TRUE, noise_method = 'spectrum'), 'data.frame')

    # noise_method not specified should return error
    expect_error(predictability(sst3$time_series, sst3$dates, delta = 1, is_uneven = FALSE, interpolate = FALSE, show_warns = TRUE), 'argument "noise_method" is missing, with no default')

    # uneven time series passes for even. interpolate to remove NAs
    expect_is(predictability(npp$time_series, npp$dates, delta = 8, is_uneven = FALSE, interpolate = TRUE, show_warns = FALSE, noise_method = 'lomb_scargle'), 'data.frame')

    # uneven time series should be used with noise_method lomb_scargle, otherwise returns error
    expect_error(predictability(npp$time_series, npp$dates, delta = 8, is_uneven = TRUE, interpolate = TRUE, show_warns = FALSE, noise_method = 'spectrum'), 'Time series is uneven, please use noise method lomb_scargle')

    # dateVector is not in progressive order
    expect_error(predictability(sst3$time_series, rev(sst3$dates), delta = 1, is_uneven = FALSE, interpolate = FALSE, show_warns = FALSE, noise_method = 'spectrum'))

    # dates is not of class Date
    expect_error(predictability(sst3$time_series, rpois(nrow(sst3), lambda = 100), delta = 1, is_uneven = FALSE, interpolate = FALSE, show_warns = FALSE, noise_method = 'spectrum'))
    expect_error(predictability(sst3$time_series, sample(letters, nrow(sst3), replace = TRUE), delta = 1, is_uneven = FALSE, interpolate = FALSE, show_warns = FALSE, noise_method = 'spectrum'))
    
    # rawTimeSeries and dates do not have same length
    expect_error(predictability(sst3$time_series, sst3$dates[-1], delta = 1, is_uneven = FALSE, interpolate = FALSE, show_warns = TRUE, noise_method = 'spectrum'))
    expect_error(predictability(sst3$time_series[-1], sst3$dates, delta = 1, is_uneven = FALSE, interpolate = FALSE, show_warns = TRUE, noise_method = 'spectrum'))

    # rawTimeSeries is not of numeric
    expect_warning(expect_error(predictability(sample(letters, nrow(sst3), replace = TRUE), sst3$dates, delta = 1, is_uneven = FALSE, interpolate = FALSE, show_warns = TRUE, noise_method = 'spectrum')))
    expect_warning(expect_error(predictability(rep(NA, nrow(sst3)), sst3$dates, delta = 1, is_uneven = FALSE, interpolate = FALSE, show_warns = TRUE, noise_method = 'spectrum')))
    expect_error(predictability(rep(NULL, nrow(sst3)), sst3$dates, delta = 1, is_uneven = FALSE, interpolate = FALSE, show_warns = TRUE, noise_method = 'spectrum'))

    # delta is not numeric
    expect_error(predictability(sst3$time_series, sst3$dates, delta = 'a', is_uneven = FALSE, interpolate = FALSE, show_warns = TRUE, noise_method = 'spectrum'), 'non-numeric argument to binary operator')
})
