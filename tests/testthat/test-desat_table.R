

test_that("exp_is_desat", {
  dtcurve <- init_dtcurve(20, 40)
  d <- desat_table(dtcurve)
  expect_true(is.desat(d))
  expect_false(is.desat('desat'))
})

#### Test tablecheck max_depth_time and no_deco_depth ####

# Test for correct input
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

test_that("err_max_depth_time_outtable", {
  err <- "no deco dives are possible below 48m"
  expect_error(max_depth_time(50, no_deco = TRUE), err )
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
  expect_equal(max_depth_time(20, no_deco = TRUE), 40 )
  expect_equal(max_depth_time(50, no_deco = TRUE, force = TRUE), 0)
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

test_that("err_desat_table_time", {
  dtcurve <- data.frame(depth = c(0, 20, 20, 0),
                        time = c(0, 50, 40, 43))
  err <- "time values need to be sorted, you don't own a subaquatic dolorean"
  expect_error(desat_table(dtcurve), err )
})


test_that("exp_desat_table_maj", {
  dtcurve <- init_dtcurve(20, 40)
  exp <- list(desat_stop = data.frame(depth = c(9, 6, 3),time = c(0, 0, 0),
                                      hour = rep(NA, 3),
                                      row.names = c("m9", "m6", "m3")), 
              group = "H", model = "table")
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
  err <- "Majoration can not be computed with a group Z and less than 12h interval"
  expect_error(majoration(depth = 40, group = 'Z', inter = 500), err )
})

# Test for correct output

test_that("exp_majoration", {
  expect_equal(majoration(depth = 40, group = 'Z', inter = 730), 0)
  expect_equal(majoration(depth = 40, group = 'I', inter = 430), 3)
  expect_equal(majoration(depth = 20, group = 'D', inter = 720), 0)
})


#### table_ndive ####

# test for correct input
test_that("err_ndive_dives", {
  err <- "dive1 must be a dive object"
  expect_error(table_ndive("dive(20, 40)", dive(20, 40), inter = 730), err )
  err <- "dive1 must be a dive object"
  expect_error(table_ndive("dive(20, 40)", "dive(20, 40)", inter = 730), err )
  err <- "dive2 must be a dive object"
  expect_error(table_ndive(dive(20, 40), "dive(20, 40)", inter = 730), err )
  err <- "This function is intended to use dive2 with the table desaturation model"
  expect_error(table_ndive(dive(20, 40), 
                           suppressMessages(dive(20, 40, desat_model = "other")), 
                           inter = 730), err )
})

test_that("exp_no_conso_table_ndive_no_consec", {
  dive1 <- dive2 <- dive(20, 40)
  exp <- list(dive1 = dive1, dive2 = "STOP", inter = 1, type = "solo" )
  class(exp) <- "ndive"
  war1 <- 'A minimum of 15 minutes is requiered between dives to consider them
            as different dives.'
  war2 <- 'Cumulated time of both dives and interval is larger than table.'
  w <- capture_warnings(res <- table_ndive(dive1, dive2, inter = 1))
  expect_match(w, war1, all = FALSE)
  expect_match(w, war2, all = FALSE)
  expect_equal(res, exp)
})

test_that("exp_no_conso_table_ndive_no_success", {
  dive1 <- dive2 <- dive(20, 40)
  exp <- list(dive1 = dive1, dive2 = "STOP", inter = 16, type = "solo" )
  class(exp) <- "ndive"
  war1 <- 'Second dive impossible due to majoration of time'
  w <- capture_warnings(res <- table_ndive(dive1, dive2, inter = 16))
  expect_match(w, war1, all = FALSE)
  expect_equal(res, exp)
  
  dive1 <- dive2 <- dive(61, 5)
  exp <- list(dive1 = dive1, dive2 = "STOP", inter = 16, type = "solo" )
  class(exp) <- "ndive"
  war1 <- 'Second dive impossible in less than 12h after a dive a 60 more meters'
  w <- capture_warnings(res <- table_ndive(dive1, dive2, inter = 16))
  expect_match(w, war1, all = FALSE)
  expect_equal(res, exp)
})

test_that("exp_no_conso_table_ndive_diff", {
  dive1 <- dive2 <- dive(20, 40)
  dive2$hour <- dive1$hour + 730 + max(dive1$hour)
  exp <- list(dive1 = dive1, dive2 = dive2, inter = 730, type = "diff" )
  class(exp) <- "ndive"
  res <- table_ndive(dive1, dive2, inter = 730)
  expect_equal(res, exp)
})

test_that("exp_no_conso_table_ndive_success", {
  dive1 <- dive(20, 40)
  dive2 <- dive(20, 40, maj = 10)
  dive2$hour <- dive2$hour + 240 + max(dive1$hour)
  exp <- list(dive1 = dive1, dive2 = dive2, inter = 240, type = "success" )
  class(exp) <- "ndive"
  res <- table_ndive(dive1, dive2, inter = 240)
  expect_equal(res, exp)
})

test_that("exp_no_conso_table_ndive_consec", {
  dive1 <- dive(20, 40)
  dive2 <- dive(20, 28)
  exp_dive2 <- dive(20, 28, maj = 47)
  exp_dive2$params["maj"] <- 0
  exp_dive2$hour <- exp_dive2$hour + 1 + max(dive1$hour)
  exp <- list(dive1 = dive1, dive2 = exp_dive2, inter = 1, type = "consec" )
  class(exp) <- "ndive"
  war1 <- 'A minimum of 15 minutes is requiered between dives to consider them
            as different dives.'
  w <- capture_warnings(res <- table_ndive(dive1, dive2, inter = 1))
  expect_match(w, war1, all = FALSE)
  expect_equal(res, exp)
})

