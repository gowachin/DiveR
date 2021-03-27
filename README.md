<!-- README.md is generated from README.Rmd. Please edit that file -->

DiveR <img src="https://raw.githubusercontent.com/gowachin/DiveR/master/inst/images/DiveR_hex.png" alt="logo" align="right" height="200px/"/>
=============================================================================================================================================

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![](https://img.shields.io/badge/devel%20version-0.3.0-blue.svg)](https://github.com/gowachin/DiveR)
[![R build
status](https://github.com/gowachin/DiveR/workflows/R-CMD-check/badge.svg)](https://github.com/gowachin/DiveR/actions)
[![Coverage
status](https://codecov.io/gh/gowachin/DiveR/branch/master/graph/badge.svg)](https://codecov.io/github/gowachin/DiveR?branch=master)

<!-- badges: end -->

R package for dive planification tools. It contains functions and
methods to represent dive curves, desaturation time and gas consumption.
At this day, only mn90 tables models are coded for single, consecutive
or successive dives. This mean all profile are square ones and only
maximum depth and dive time are used to compute desaturation.

**The shiny application is not yet in production.**

Future parts are work in progress, like more precise planification setup
with different depths and time input. Desaturation planification with
other models are also planned along with maybe other gas than air
supported for consumption.

Intended to be used by french dive student, traduction of the lexic is
on it’s way.

Installation
------------

You can install the development version of `{DiveR}` from
[github](https://github.com/gowachin/DiveR) with:

    # install.packages("devtools")
    devtools::install_github('https://github.com/gowachin/DiveR')
    # or 
    # install.packages("remotes")
    remotes::install_github("gowachin/DiveR")

Usage
-----

### Dive planification

**A vignette need to be written**

<!--
### Planning a single dive

Apart from default/advanced settings, a dive can be resumed by it's maximum depth and duration. For example, here is the default dive for this pacakge, a maximum depth at 20 meters for 40 minutes. Note here that despite going underwater at an altitude of -20, we use positive numeric values.

dive(20,40)

### Planning a second dive

A second dive depends heavily on the first one as the desaturation is not perfect and residual azote will impact the second saturation. 
 -->

### Gas Consumption

Read [Gas
Consumption](https://gowachin.github.io/DiveR/articles/gas_conso.html)
Vignette to learn how to simulate the use of gas during a dive.

Disclaimer
----------

This application is intended for use in education about scubadiving
planification and academic interest only. It is not designed for actual
use in scuba diving and underwater activity. It is emphatically not
suitable for use in actual diving. Scuba diving is a dangerous activity
with risks of death and serious injury. No-one should attempt scuba
diving without training, certification, supervision and regular medical
assessment. It is also dangerous for trained scuba divers to exceed the
limitations of their training.

This application will provide planification about dive profile and air
consumption, without giving any warning if the activity would be
dangerous or fatal. In doing so, it does not take account of safety
restrictions, other physical laws, or other important information.
Despite using diving table as base for computation, no output from this
application should be misconstrued as a diving table. The author does
not warrant that the application is correct in any sense whatsoever.
Even if correctly computed, the predictions of a theoretical physical
model may not be correct predictions.

Note here that all dives simulated will also being at sea level.

Want to help ?
--------------

Go check the projects of this repository !

Feel free to fork this, and use it. Any recommendation is welcome :)
