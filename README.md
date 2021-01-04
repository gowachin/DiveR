# MN90

R package and shiny application for dive planification tools. It contains functions and methods to represent dive curves, desaturation time and gas consumption. At this day, only mn90 tables models are coded for single, consecutive or successive dives. This mean all profile are square ones and only maximum depth and dive time are used to compute desaturation.

The shiny application is not yet in production.

Future parts are work in progress, like more precise planification setup with different depths and time input. Desaturation planification with other models are also planned along with maybe other gas than air supported for consumption

Intended to be used by french dive student, traduction of the lexic is on it's way. For this part the shiny app is written in english with french translation possible in settings.

<!--
## Installation

This package can be installed with the devtools package or by cloning this repository.

## Usage

### Planning a single dive

Apart from default/advanced settings, a dive can be resumed by it's maximum depth and duration. For example, here is the default dive for this pacakge, a maximum depth at 20 meters for 40 minutes. Note here that despite going underwater at an altitude of -20, we use positive numeric values.

dive(20,40)

### Planning a second dive

A second dive depends heavily on the first one as the desaturation is not perfect and residual azote will impact the second saturation. 
-->


## Actual in work part

Consumption plot.conso and plot.ndive.

## To do : 

- Complete README with examples.
- Change package, repository name

- fix plot.ndive when consecutive dives (depths)
- doc on new conso and dive functions
- more attributes
- make a beautiful doc for every functions
- desaturation with complex models
- multiple gas tank consumption.

## Doc in work (mostly missing examples) :

- attributes
dtr
speed
summary.dive
dtime
depth

- conso
bloc
press_time
conso

- deco
majoration
dtcurve

- graphics
plot.dive
plot.ndive

- mn90
mn90
dive
ndive

Feel free to fork this, and use it. Any recommendation is welcome :) 
