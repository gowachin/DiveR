
covr::package_coverage()

library(covr)

cov <- package_coverage()

# view results as a data.frame
df_cov <- as.data.frame(cov)

file_coverage('R/conso.R', 'tests/testthat/test-conso.R')

# zero_coverage() shows only uncovered lines.
# If run within RStudio, `zero_coverage()` will open a marker pane with the
# uncovered lines.
zero_coverage(cov)
