
# Run all tests
rm(list = ls())
devtools::document('.')
devtools::load_all('.')
devtools::test()

library(covr, testthat)
# get a shiny to find which line is not yet tested. Very helpfull
report(x = package_coverage(line_exclusions = list("R/graphic.R")))
report(x = package_coverage())
# covr::package_coverage()

# rerun all coverage for dataframe analysis.
# cov <- package_coverage()
# view results as a data.frame
# df_cov <- as.data.frame(cov)

# df_cov[df_cov$functions=='tank',]

file_coverage('R/conso.R', 'tests/testthat/test-conso.R')
file_coverage('R/methods_conso.R', 'tests/testthat/test-methods_conso.R')
x <- file_coverage('R/dive_utils.R', 'tests/testthat/test-dive_utils.R')
x <- file_coverage('R/DiveR.R', 'tests/testthat/test-dive.R')
x <- file_coverage('R/desat_table.R', 'tests/testthat/test-desat_table.R')
x <- file_coverage('R/methods_dive.R', 'tests/testthat/test-methods_dive.R')
# x <- file_coverage('R/deco.R', 'tests/testthat/test-deco.R')
report(x)
# zero_coverage() shows only uncovered lines.
# If run within RStudio, `zero_coverage()` will open a marker pane with the
# uncovered lines.
# zero_coverage(cov)


# Update the doc part of the github with pkgdown
# pkgdown::build_site()

# Link between functions
library(mvbutils)
foodweb(where = asNamespace( "DiveR"), cex = 0.8, color.lines = F)

# Update the coverage to codecov, don't forget the token
# covr::codecov()
