[![CRAN version](http://www.r-pkg.org/badges/version/banter?color=red)](https://cran.r-project.org/package=banter)
[![CRAN last day downloads](http://cranlogs.r-pkg.org/badges/last-day/banter?color=red)](https://cran.r-project.org/package=banter)
[![CRAN last week downloads](http://cranlogs.r-pkg.org/badges/last-week/banter?color=red)](https://cran.r-project.org/package=banter)
[![CRAN last month downloads](http://cranlogs.r-pkg.org/badges/banter?color=red)](https://cran.r-project.org/package=banter)
[![CRAN total downloads](http://cranlogs.r-pkg.org/badges/grand-total/banter?color=red)](https://cran.r-project.org/package=banter)  
[![R-CMD-check](https://github.com/EricArcher/banter/workflows/R-CMD-check/badge.svg)](https://github.com/EricArcher/banter/actions)  

# banter

### Description

*banter* is a package for creating hierarchical acoustic event classifiers out of multiple call type detectors.

### Installation

To install the latest version from GitHub:

```r
# make sure you have devtools installed
if(!require('devtools')) install.packages('devtools')

# install package from GitHub
devtools::install_github('SWFSC/banter')
```

### For a complete tutorial, run `banterGuide()`.

### Quick Tutorial

The BANTER (Bio-Acoustic eveNT classifiER) model is initialized with a data frame of events. There is one row per event and it must have a column called `event.id` which is a unique id for each event, and a column called `species` which assigns each event to a given species. Every other column in the data.frame will be used as a predictor variable for the events.  
In the package, an example data.frame is in the `train.data` example data list as the `$events` element.
```r
data(train.data)
bant.mdl <- initBanterModel(train.data$events)
```

Next, detector data is added to the initialized BANTER model object. Each detector is a data.frame with a column called `event.id` that associates the detected call with an event that the model was initialized with, and a `call.id` column that provides a unique identifier for each call. Every other column will be used as a predictor variable for the calls.  
In the package, example data.frames for three detectors are provided in the `$detectors` element of the `train.data` example data list. Here is an example of adding the burst pulse (`bp`) detector.

```r
bant.mdl <- addBanterDetector(
  bant.mdl, 
  data = train.data$detectors$bp, 
  name = "bp",
  ntree = 10, 
  sampsize = 1
)
```

The `addBanterDetector` function can be called repeatedly to add additional detectors. Alternatively, if the detectors are all in a named list, they can be added at once:

```r
bant.mdl <- addBanterDetector(
  bant.mdl, 
  data = train.data$detectors, 
  ntree = 10, 
  sampsize = 1
)
```

Once all of the detectors have been added, then the full BANTER model is run:
```r
bant.mdl <- runBanterModel(bant.mdl, ntree = 5000, sampsize = 3)
```

The model can be easily summarized:
```r
summary(bant.mdl)
```

The actual `randomForest` model can be extracted for the event or detector models:
```r
# extract event Random Forest model
event.rf <- getBanterModel(bant.mdl, "event")

# extract burst pulse (bp) Random Forest model
bp.rf <- getBanterModel(bant.mdl, "bp")
```

These can then be visualized using other tools, such as those in the `rfPermute` package:
```r
library(rfPermute)
plotVotes(event.rf)
```

To predict novel data, it must be in a list with the event data in the `$events` element, and the detector data in a named list called `$detectors`:
```r
data(test.data)
predict(bant.mdl, test.data)
```

### Contact

* submit suggestions and bug-reports: <https://github.com/SWFSC/banter/issues>
* send a pull request: <https://github.com/SWFSC/banter/>
* e-mail: <eric.ivan.archer@gmail.com>

### Reference
Rankin, S., Archer, F., Keating, J. L., Oswald, J. N., Oswald, M., Curtis, A. and Barlow, J. (2017) Acoustic classification of dolphins in the California Current using whistles, echolocation clicks, and burst pulses. Mar Mam Sci, 33: 520-540. [doi:10.1111/mms.12381](https://onlinelibrary.wiley.com/doi/abs/10.1111/mms.12381)

### version 0.9.8 (on CRAN)

* move package to SWFSC GitHub site

### version 0.9.6

* changed behavior of `addBanterDetector()` to provide a warning instead of an error if a detector didn't have enough detections of a species and add all detectors that were valid. 
* minor bug fixes and edits for CRAN

### version 0.9.5

* add `subsampleDetections()` to draw a random number of detections per event
* fixed `predict()` so that species names in `new.data` are modified using `make.names()` like they are in constructing the initial banter model.
* added run timestamps to all models and changed `summary()` to display a matrix of all.
* fixed trace and inbag plots in `summary()` and `plotDetectorTrace()`.

### version 0.9.4

* Detector names and detector predictor column names submitted to `addBanterDetector()` are now first checked to make sure they are syntactically valid and unique by comparing them with the results from `make.names()`.
* Detector names and detector predictor column names in `new.data` submitted to `predict()` are first checked to make sure at least one detector from model is present.
* Detectors missing in `new.data` for `predict()` will automatically have all species detector probabilities and detector proportions set to 0.
* Added validation option to `predict()` if `species` column exists in `new.data`
* Default value for num.cores has been set to 1.
* Added `banterGuide()`

### version 0.9.3

* Initial CRAN release
