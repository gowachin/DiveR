# DiveR 0.4.0

## Major improvements

* Refactorisation of the dive class and all functions to cope with curve dive (input with multiples depth and time values.)

* Test all of the dive class functions (except for graphic ones...)

* Dive Planification Vignette.

## Historic Modifications

* `max_depth_t` has been renamed to `max_depth_time`

* desaturation with table has been rewritten from `palier` and `dtcurve` functions to `ìnit_dtcurve`, `add_desat` and `desat_table` functions. 

* replacing all azote text with nitrogen...thinking about non french divers after all.

* Idiot proof is made with correct functions and `check_val` function has been removed

* Most summary are pretty now.

* `ndive` will now call function depending on the second dive desaturation model.

## Bugs fixed

* dtime work now on curve dives ( #1 )

* Consecutive dives was adding interval time to total time, it has been corrected. ( #10 )

* Correction of additional time when using successive dives. Was adding between 4 and 1 minute more than expected when using tables. ( #11 )

# DiveR 0.3.0

## Major improvements

* Consumption functions work on single tank dive, multiple tanks dive.

* Gas consumption vignette.

* testthat is set for consumption functions.

## Minor improvements

* R.utils dependance was not used and removed

* viridis has been remplaced by viridisLite

## Documentation

* Usage of `pkgdown` package for documentation

# DiveR 0.2.0

## Major improvements

* Added a `NEWS.md` file to track changes to the package.

* Vignette about gas consumption in work

## Modifications about consumption

* Consumption functions (`bloc`, `press_time`, `conso`, `plot.conso`) have been trough total refactorisation. 

* `dtime.conso` and `press_time` have been removed. 

* `simpl`, `depth_at_time` and `time_at_depth` have been slightly modified to fit the `conso` function.

