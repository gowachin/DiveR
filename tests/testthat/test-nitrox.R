

#### Test MODcheck function ####

# Test for correct input
test_that("err_nitrox_depth", {
  err <- "depth must be positive numeric value\\(s\\)."
  expect_error(MODcheck(-5), err )
  expect_error(MODcheck("20"), err )
})

test_that("err_nitrox_ppn2h", {
  err <- "ppn2 must be positive numeric value\\(s\\) between 0 and 1."
  expect_error(MODcheck(20, -0.1), err )
  expect_error(MODcheck(20, 1.1), err )
  expect_error(MODcheck(20, "0.2"), err )
})

test_that("err_nitrox_force", {
  err <- "force must be TRUE or FALSE"
  expect_error(MODcheck(20, force = "TRUE"), err )
  expect_error(MODcheck(20, force = NA), err )
})

test_that("exp_nitrox", {
  err <- "This depth is highter than the maximum depth possible \\(66m\\)
while using this gas melange."
  expect_error(MODcheck(70), err)
  expect_false(MODcheck(70, force = TRUE))
  expect_true(MODcheck(20, force = TRUE))
})

#### Test nitrox_depth function ####

# Test for correct input
test_that("err_nitrox_depth", {
  err <- "depth must be positive numeric value\\(s\\)."
  expect_error(nitrox_depth(-5), err )
  expect_error(nitrox_depth("20"), err )
})

test_that("err_nitrox_ppn2h", {
  err <- "ppn2 must be positive numeric value\\(s\\) between 0 and 1."
  expect_error(nitrox_depth(20, -0.1), err )
  expect_error(nitrox_depth(20, 1.1), err )
  expect_error(nitrox_depth(20, "0.2"), err )
})

test_that("exp_nitrox", {
  err <- "This depth is highter than the maximum depth possible \\(66m\\)
while using this gas melange."
  expect_equal(nitrox_depth(50), 50)
  expect_error(nitrox_depth(70), err)
  expect_equal(round(nitrox_depth(50, 0.74), 2), 46.13)
})
