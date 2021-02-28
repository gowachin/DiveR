
# Run all tests
rm(list = ls())
devtools::document('.')
devtools::load_all('.')
devtools::test()

library(covr)
# get a shiny to find which line is not yet tested. Very helpfull
report()
# covr::package_coverage()

# rerun all coverage for dataframe analysis.
# cov <- package_coverage()
# view results as a data.frame
# df_cov <- as.data.frame(cov)

# df_cov[df_cov$functions=='tank',]

file_coverage('R/conso.R', 'tests/testthat/test-conso.R')
file_coverage('R/methods_conso.R', 'tests/testthat/test-methods_conso.R')

# zero_coverage() shows only uncovered lines.
# If run within RStudio, `zero_coverage()` will open a marker pane with the
# uncovered lines.
# zero_coverage(cov)
