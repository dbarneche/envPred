<!-- README.md is generated from README.Rmd. Please edit that file -->

envPred
=======

<!-- badges: start -->

[![lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![R build
status](https://github.com/dbarneche/envPred/workflows/R-CMD-check/badge.svg)](https://github.com/dbarneche/envPred/actions)
[![Travis-CI Build
Status](http://badges.herokuapp.com/travis/dbarneche/envPred?branch=master&env=BUILD_NAME=trusty_release&label=linux)](https://travis-ci.org/dbarneche/envPred)
[![Build
Status](http://badges.herokuapp.com/travis/dbarneche/envPred?branch=master&env=BUILD_NAME=osx_release&label=osx)](https://travis-ci.org/dbarneche/envPred)
[![Codecov test
coverage](https://codecov.io/gh/dbarneche/envPred/branch/master/graph/badge.svg)](https://codecov.io/gh/dbarneche/envPred?branch=master)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/dbarneche/envPred?branch=master&svg=true)](https://ci.appveyor.com/project/dbarneche/envPred)
![pkgdown](https://github.com/dbarneche/envPred/workflows/pkgdown/badge.svg)
[![packageversion](https://img.shields.io/badge/Package%20version-1.0.0-orange.svg)](commits/master)
[![license](https://img.shields.io/badge/license-GPL--2-blue.svg)](https://www.gnu.org/licenses/old-licenses/gpl-2.0.html)
<!-- badges: end -->

`envPred` is an R package that calculates two components of
environmental predictability out of environmental time series
(e.g. temperature, productivity, rainfall, etc.).

Example data sources in envPred
-------------------------------

The following sample data were obtained using the
[noaaErddap](https://github.com/dbarneche/noaaErddap/) R package:

-   [Net Primary Productivity (NPP)
    data](http://coastwatch.pfeg.noaa.gov/erddap/griddap/erdPPbfp18day.html)
-   [Sea Surface Temperature
    (SST)](http://www.esrl.noaa.gov/psd/data/gridded/data.noaa.oisst.v2.highres.html)

Installation
------------

The `envPred` package can be installed from github using the
[`devtools`](https://cran.r-project.org/web/packages/devtools/index.html)
package using `devtools::install_github`.

If you do not yet have `devtools`, install with
`install.packages("devtools")`.

Then install `envPred` using the following:  
`library(devtools)`  
`install_github('dbarneche/envPred')`  
`library(envPred)`

Examples
--------

    library(envPred)

    # first, carefully read help page for main function
    ?envPredictability

    # then calculate seasonality and color of environmental noise using sample data (temporally even SST data)
    envPredictability(sst$rawTimeSeries, sst$datesVector, delta = 1, isUneven = FALSE, interpolate = FALSE, checkPlots = FALSE, showWarnings = TRUE, noiseMethod = 'spectrum')

    # address first warning
    sst2  <-  sst[sst$datesVector <= as.Date('2007-01-01'), ]
    envPredictability(sst2$rawTimeSeries, sst2$datesVector, delta = 1, isUneven = FALSE, interpolate = FALSE, checkPlots = FALSE, showWarnings = TRUE, noiseMethod = 'spectrum')

    # then do the same using temporally uneven NPP data with NAs (carefully check warnings, inspect plots)
    envPredictability(npp$rawTimeSeries, npp$datesVector, delta = 8, isUneven = TRUE, interpolate = TRUE, checkPlots = TRUE, showWarnings = TRUE, noiseMethod = 'LombScargle')
    dev.off(); dev.off(); dev.off()

Authors
-------

Dr. Diego Barneche (Australian Institute of Marine Science) and
Dr. Scott Burgess (Florida State University Tallahassee)

Bug reporting
-------------

-   Please [report any issues or
    bugs](https://github.com/dbarneche/envPred/issues).
