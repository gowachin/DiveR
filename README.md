
<!-- README.md is generated from README.Rmd. Please edit that file -->

# DiveR <img src="https://raw.githubusercontent.com/gowachin/DiveR/master/inst/images/DiveR_hex.png" alt="logo" align="right" height=200px/>

<!-- badges: start -->

<!-- [![R build status](https://github.com/gowachin/DiveR/workflows/R-CMD-check/badge.svg)](https://github.com/gowachin/DiveR/actions) -->
[![](https://img.shields.io/badge/devel%20version-0.1.0-blue.svg)](https://github.com/gowachin/DiveR)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end --> R package for dive planification tools. It contains
functions and methods to represent dive curves, desaturation time and
gas consumption. At this day, only mn90 tables models are coded for
single, consecutive or successive dives. This mean all profile are
square ones and only maximum depth and dive time are used to compute
desaturation.

**The shiny application is not yet in production.**

Future parts are work in progress, like more precise planification setup
with different depths and time input. Desaturation planification with
other models are also planned along with maybe other gas than air
supported for consumption.

Intended to be used by french dive student, traduction of the lexic is
on it’s way.

## Installation

You can install the development version of `{DiveR}` from
[github](https://github.com/gowachin/DiveR) with:

``` r
# install.packages("devtools")
devtools::install_github('https://github.com/gowachin/DiveR')
# or 
# install.packages("remotes")
remotes::install_github("gowachin/DiveR")
```

<!--
## Usage

### Planning a single dive

Apart from default/advanced settings, a dive can be resumed by it's maximum depth and duration. For example, here is the default dive for this pacakge, a maximum depth at 20 meters for 40 minutes. Note here that despite going underwater at an altitude of -20, we use positive numeric values.

dive(20,40)

### Planning a second dive

A second dive depends heavily on the first one as the desaturation is not perfect and residual azote will impact the second saturation. 

You'll still need to render `README.Rmd` regularly, to keep `README.md` up-to-date. `devtools::build_readme()` is handy for this. You could also use GitHub Actions to re-render `README.Rmd` every time you push. An example workflow can be found here: <https://github.com/r-lib/actions/tree/master/examples>. -->

## Disclaimer

This application is intended for use in education about scubadiving
planification and academic interest only. It is not designed for actual
use in scuba diving and underwater activity. It is emphatically not
suitable for use in actual diving. Scuba diving is a dangerous activity
with risks of death and serious injury. No-one should attempt scuba
diving without training, certification, supervision and regular medical
assessment. It is also dangerous for trained scuba divers to exceed the
limitations of their training.

This application will provide planinfication about dive profile and air
consumption, without giving any warning if the activity would be
dangerous or fatal. In doing so, it does not take account of safety
restrictions, other physical laws, or other important information.
Despite using diving table as base for computation, no output from this
application should be misconstrued as a diving table. The author does
not warrant that the application is correct in any sense whatsoever.
Even if correctly computed, the predictions of a theoretical physical
model may not be correct predictions.

Note here that all dives simulated will also being at sea level.

## Actual in work part

Refactorisation of Consumption, plot.conso for multi tank dives.

## To do :

-   Complete README with examples
-   Redaction of vignettes
-   doc on new conso and dive functions
-   make a beautiful doc for every functions
-   desaturation with complex models

## Doc in work (mostly missing examples) :

-   attributes summary.dive

-   conso bloc press\_time conso

-   deco majoration dtcurve

-   graphics plot.dive

-   mn90 mn90 dive ndive

Feel free to fork this, and use it. Any recommendation is welcome :)
