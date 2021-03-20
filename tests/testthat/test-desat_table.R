

test_that("exp_is_desat", {
  dtcurve <- init_dtcurve(20, 40)
  d <- desat_table(dtcurve)
  expect_true(is.desat(d))
  expect_false(is.desat('desat'))
})

#### Test tablecheck and max_depth_time ####
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

#### Test desat_table ####

test_that("err_desat_table_depth", {
  dtcurve <- list(depth = c(0, 20, 20, 0),
                  time = c(0, 5, 40, 43))
  
  err <- "dtcurve must be a data.frame with 2 columns named depth and time without any NA value"
  expect_error(desat_table(dtcurve), err )
  
  dtcurve <- as.data.frame(dtcurve)
  tmp <- colnames(dtcurve)
  colnames(dtcurve) <- c("depths", "time")
  expect_error(desat_table(dtcurve), err )
  colnames(dtcurve) <- tmp
  dtcurve[2, 1] <- NA
  expect_error(desat_table(dtcurve), err )
})

test_that("err_desat_table_depth", {
  dtcurve <- data.frame(depth = c(0, -20, 20, 0),
                        time = c(0, 5, 40, 43))
  err <- "depth must be positive numeric value."
  expect_error(desat_table(dtcurve), err )
  dtcurve$depth[2] <- "hello"
  expect_error(desat_table(dtcurve), err )
})

test_that("err_desat_table_time", {
  dtcurve <- data.frame(depth = c(0, 20, 20, 0),
                        time = c(0, -5, 40, 43))
  err <- "time must be positive numeric value."
  expect_error(desat_table(dtcurve), err )
  dtcurve$time[2] <- "hello"
  expect_error(desat_table(dtcurve), err )
  dtcurve <- data.frame(depth = c(0, 20, 20, 0),
                        time = c(0, 50, 40, 43))
  err <- "time values need to be sorted, you don't own a subaquatic dolorean"
  expect_error(desat_table(dtcurve), err )
})

test_that("err_desat_table_maj", {
  dtcurve <- data.frame(depth = c(0, 20, 20, 0),
                        time = c(0, 5, 40, 43))
  err <- "maj must be a single positive numeric value."
  expect_error(desat_table(dtcurve, maj = -5), err )
  expect_error(desat_table(dtcurve, maj = "hello"), err )
  expect_error(desat_table(dtcurve, maj = c(0, 0)), err )
})

test_that("exp_desat_table_maj", {
  dtcurve <- init_dtcurve(20, 40)
  exp <- list(desat_stop = data.frame(depth = c(9, 6, 3),time = c(0, 0, 0),
                                      hour = rep(NA, 3),
                                      row.names = c("m9", "m6", "m3")), 
              group = "H")
  class(exp) <- "desat"
  expect_equal(desat_table(dtcurve), exp )
  dtcurve <- init_dtcurve(20, 60)
  exp$desat_stop$time[3] <- 13
  exp$group = "K"
  expect_equal(desat_table(dtcurve), exp )
  dtcurve <- init_dtcurve(20, 60)
  exp$desat_stop$time[3] <- 20
  exp$group = "L"
  expect_equal(desat_table(dtcurve, maj = 10), exp )
})


#### Test majoration ####


# Test for correct input
test_that("err_majoration_depth", {
  err <- "depth must be positive numeric value."
  expect_error(majoration(depth = -5, group = 'A', inter = 40), err )
  expect_error(majoration(depth = "20", group = 'A', inter = 40), err )
  expect_error(majoration(depth = c(20, 30), group = 'A', inter = 40), err )
  err <- "depth must be inferior or equal to 60."
  expect_error(majoration(depth = 65, group = 'A', inter = 40), err )
})

test_that("err_majoration_group", {
  err <- "group must be a capital letter between A and P or Z"
  expect_error(majoration(depth = 40, group = 'a', inter = 40), err )
  expect_error(majoration(depth = 40, group = 2, inter = 40), err )
  expect_error(majoration(depth = 40, group = 'Q', inter = 40), err )
})

test_that("err_majoration_inter", {
  err <- "inter must be positive numeric value between."
  expect_error(majoration(depth = 40, group = 'A', inter = 10), err )
  expect_error(majoration(depth = 40, group = 'A', inter = "10"), err )
  expect_error(majoration(depth = 40, group = 'A', inter = c(10, 20)), err )
})

test_that("err_majoration_inter", {
  err <- "Majoration can not be computed with a group Z and less than 12h interval"
  expect_error(majoration(depth = 40, group = 'Z', inter = 500), err )
})

# Test for correct output

test_that("exp_majoration", {
  expect_equal(majoration(depth = 40, group = 'Z', inter = 730), 0)
  expect_equal(majoration(depth = 40, group = 'I', inter = 430), 3)
  expect_equal(majoration(depth = 20, group = 'D', inter = 720), 0)
})

