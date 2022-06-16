

#### Test MODcheck function ####

test_that("exp_nitrox_maxdepth", {
  expect_equal(nitrox_maxdepth(), 66 )
  expect_equal(nitrox_maxdepth(0.26), 51 )
  expect_equal(nitrox_maxdepth(0.4), 30 )
  expect_equal(nitrox_maxdepth(1), 6)
})

# Test for correct input
 
test_that("exp_MODcheck", {
  expect_false(MODcheck(70, force = TRUE))
  expect_true(MODcheck(20, force = TRUE))
  err = "This depth is highter than the maximum depth possible \\(66m\\)
while using this gas melange."
  expect_error(MODcheck(80, force = FALSE), err)
})

#### Test nitrox_depth function ####

test_that("exp_nitrox_depth", {
  err <- "This depth is highter than the maximum depth possible \\(66m\\)
while using this gas melange."
  expect_equal(nitrox_depth(50), 50)
  expect_error(nitrox_depth(70), err)
  expect_equal(round(nitrox_depth(50, 0.26), 2), 46.13)
})
