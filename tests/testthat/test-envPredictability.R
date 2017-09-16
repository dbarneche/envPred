context('Calculating environmental predictability components')

library(envPred)
data(sst)
data(npp)

# copy of time series starting and ending at the same day different years
sst3  <-  sst[1:which(sst$datesVector == as.Date('2007-01-01')), ]

# introduce NAs in complete SST series
sst2  <-  sst3
sst2$rawTimeSeries[sample(1:nrow(sst2), 50)]  <-  NA

# copy of short time series to show warning
sst4  <-  sst[1:500, ]

test_that('Simple corner cases', {
    # returns correct structure
    expect_is(envPredictability(sst3$rawTimeSeries, sst3$datesVector, delta = 1, isUneven = FALSE, interpolate = FALSE, checkPlots = FALSE, showWarnings = TRUE, noiseMethod = 'spectrum'), 'data.frame')
    expect_is(envPredictability(sst3$rawTimeSeries, sst3$datesVector, delta = 1, isUneven = FALSE, interpolate = FALSE, checkPlots = FALSE, showWarnings = TRUE, noiseMethod = 'spectrum'), 'data.frame')
    expect_is(envPredictability(sst3$rawTimeSeries, sst3$datesVector, delta = 1, isUneven = FALSE, interpolate = FALSE, checkPlots = FALSE, showWarnings = TRUE, noiseMethod = 'spectrum'), 'data.frame')

    # returns correct structure, but with warnings
    expect_warning(expect_is(envPredictability(sst$rawTimeSeries, sst$datesVector, delta = 1, isUneven = FALSE, interpolate = FALSE, checkPlots = FALSE, showWarnings = TRUE, noiseMethod = 'spectrum'), 'data.frame'))
    expect_warning(expect_is(envPredictability(sst$rawTimeSeries, sst$datesVector, delta = 1, isUneven = FALSE, interpolate = FALSE, checkPlots = FALSE, showWarnings = TRUE, noiseMethod = 'spectrum'), 'data.frame'))
    expect_warning(expect_is(envPredictability(sst$rawTimeSeries, sst$datesVector, delta = 1, isUneven = FALSE, interpolate = FALSE, checkPlots = FALSE, showWarnings = TRUE, noiseMethod = 'spectrum'), 'data.frame'))
    expect_warning(expect_is(envPredictability(npp$rawTimeSeries, npp$datesVector, delta = 8, isUneven = TRUE, interpolate = FALSE, checkPlots = TRUE, showWarnings = TRUE, noiseMethod = 'LombScargle'), 'data.frame'))
    dev.off(); dev.off(); dev.off()
    expect_warning(expect_is(envPredictability(npp$rawTimeSeries, npp$datesVector, delta = 8, isUneven = TRUE, interpolate = TRUE, checkPlots = TRUE, showWarnings = TRUE, noiseMethod = 'LombScargle'), 'data.frame'))
    dev.off(); dev.off(); dev.off()
    
    # returns correct structure, but with warnings
    expect_warning(envPredictability(sst4$rawTimeSeries, sst4$datesVector, delta = 1, isUneven = FALSE, interpolate = FALSE, checkPlots = FALSE, showWarnings = TRUE, noiseMethod = 'spectrum'))

    # returns error because method spectrum cannot be used with NAs
    expect_error(envPredictability(sst2$rawTimeSeries, sst2$datesVector, delta = 1, isUneven = FALSE, interpolate = FALSE, checkPlots = FALSE, showWarnings = FALSE, noiseMethod = 'spectrum'))
    expect_warning(expect_error(envPredictability(sst2$rawTimeSeries, sst2$datesVector, delta = 1, isUneven = FALSE, interpolate = FALSE, checkPlots = FALSE, showWarnings = TRUE, noiseMethod = 'spectrum')))
    
    # returns correct structure because interpolate = TRUE
    expect_is(envPredictability(sst2$rawTimeSeries, sst2$datesVector, delta = 1, isUneven = FALSE, interpolate = TRUE, checkPlots = FALSE, showWarnings = TRUE, noiseMethod = 'spectrum'), 'data.frame')

    # noiseMethod not specified should return error
    expect_error(envPredictability(sst3$rawTimeSeries, sst3$datesVector, delta = 1, isUneven = FALSE, interpolate = FALSE, checkPlots = FALSE, showWarnings = TRUE), 'argument "noiseMethod" is missing, with no default')

    # uneven time series passes for even. interpolate to remove NAs
    expect_is(envPredictability(npp$rawTimeSeries, npp$datesVector, delta = 8, isUneven = FALSE, interpolate = TRUE, checkPlots = FALSE, showWarnings = FALSE, noiseMethod = 'LombScargle'), 'data.frame')

    # uneven time series should be used with noiseMethod LombScargle, otherwise returns error
    expect_error(envPredictability(npp$rawTimeSeries, npp$datesVector, delta = 8, isUneven = TRUE, interpolate = TRUE, checkPlots = FALSE, showWarnings = FALSE, noiseMethod = 'spectrum'), 'Time series is uneven, please use noise method LombScargle')

    # dateVector is not in progressive order
    expect_error(envPredictability(sst3$rawTimeSeries, rev(sst3$datesVector), delta = 1, isUneven = FALSE, interpolate = FALSE, checkPlots = FALSE, showWarnings = FALSE, noiseMethod = 'spectrum'))

    # datesVector is not of class Date
    expect_error(envPredictability(sst3$rawTimeSeries, rpois(nrow(sst3), lambda = 100), delta = 1, isUneven = FALSE, interpolate = FALSE, checkPlots = FALSE, showWarnings = FALSE, noiseMethod = 'spectrum'))
    expect_error(envPredictability(sst3$rawTimeSeries, sample(letters, nrow(sst3), replace = TRUE), delta = 1, isUneven = FALSE, interpolate = FALSE, checkPlots = FALSE, showWarnings = FALSE, noiseMethod = 'spectrum'))
    
    # rawTimeSeries and datesVector do not have same length
    expect_error(envPredictability(sst3$rawTimeSeries, sst3$datesVector[-1], delta = 1, isUneven = FALSE, interpolate = FALSE, checkPlots = FALSE, showWarnings = TRUE, noiseMethod = 'spectrum'))
    expect_error(envPredictability(sst3$rawTimeSeries[-1], sst3$datesVector, delta = 1, isUneven = FALSE, interpolate = FALSE, checkPlots = FALSE, showWarnings = TRUE, noiseMethod = 'spectrum'))

    # rawTimeSeries is not of numeric
    expect_warning(expect_error(envPredictability(sample(letters, nrow(sst3), replace = TRUE), sst3$datesVector, delta = 1, isUneven = FALSE, interpolate = FALSE, checkPlots = FALSE, showWarnings = TRUE, noiseMethod = 'spectrum')))
    expect_warning(expect_error(envPredictability(rep(NA, nrow(sst3)), sst3$datesVector, delta = 1, isUneven = FALSE, interpolate = FALSE, checkPlots = FALSE, showWarnings = TRUE, noiseMethod = 'spectrum')))
    expect_warning(expect_error(envPredictability(rep(NULL, nrow(sst3)), sst3$datesVector, delta = 1, isUneven = FALSE, interpolate = FALSE, checkPlots = FALSE, showWarnings = TRUE, noiseMethod = 'spectrum')))

    # delta is not numeric
    expect_error(envPredictability(sst3$rawTimeSeries, sst3$datesVector, delta = 'a', isUneven = FALSE, interpolate = FALSE, checkPlots = FALSE, showWarnings = TRUE, noiseMethod = 'spectrum'), 'non-numeric argument to binary operator')
})
