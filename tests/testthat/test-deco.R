

#### Test tablecheck ####
# Test for correct input
test_that("err_tablecheck_depth", {
  err <- "depth must be positive numeric value."
  expect_error(tablecheck(-5, 40), err )
  expect_error(tablecheck("20", 40), err )
  expect_error(tablecheck(c(20, 30), 40), err )
})

test_that("err_max_depth_time_depth", {
  err <- "depth must be positive numeric value."
  expect_error(max_depth_time(-5), err )
  expect_error(max_depth_time("20"), err )
  expect_error(max_depth_time(c(20, 30)), err )
})

test_that("err_tablecheck_time", {
  err <- "time must be positive numeric value."
  expect_error(tablecheck(20, -40), err )
  expect_error(tablecheck(20, "20"), err )
  expect_error(tablecheck(20, c(20, 40)), err )
})

test_that("err_tablecheck_outtable", {
  err <- "Time or depth values are outside the mn90 table,
depth must be not exceed 65 and time 3h \\(180 minutes\\)
please read doc with \\?tablecheck or help\\(tablecheck\\)"
  expect_error(tablecheck(20, 240), err )
  expect_error(tablecheck(80, 5), err )
})

test_that("err_max_depth_time_outtable", {
  err <- "depth value is outside the mn90 table, depth
must be not exceed 65 meter
please read doc with \\?tablecheck or help\\(tablecheck\\)"
  expect_error(max_depth_time(80), err )
})

test_that("err_tablecheck_outtable", {
  err <- "Maximum time at 20 meters is 75 minutes"
  expect_error(tablecheck(20, 100), err )
})

test_that("out_tablecheck_intable", {
  expect_true(tablecheck(20, 40, force = TRUE) )
  expect_false(tablecheck(20, 80, force = TRUE) )
  expect_false(tablecheck(80, 80, force = TRUE) )
})

test_that("out_max_depth_time_intable", {
  expect_equal(max_depth_time(20), 75 )
  expect_equal(max_depth_time(80, force = TRUE), 0 )
})



