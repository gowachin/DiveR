
#### Test init_dtcurve ####

# Test for correct input
test_that("err_init_dtcurve_depth", {
  err <- "depth must be positive numeric value\\(s\\)."
  expect_error(init_dtcurve(-5, 40), err )
  expect_error(init_dtcurve("20", 40), err )
})

test_that("err_init_dtcurve_time", {
  err <- "time must be positive numeric value\\(s\\)."
  expect_error(init_dtcurve(20, -40), err )
  expect_error(init_dtcurve(20, "20"), err )
})

test_that("err_init_dtcurve_time_order", {
  err <- "time values need to be sorted, you don't own a subaquatic dolorean"
  expect_error(init_dtcurve(20, c(60,40)), err )
})

test_that("err_init_dtcurve_ascent_sp", {
  err <- "ascent_speed must be a single positive numeric value\\(s\\)."
  expect_error(init_dtcurve(20, 40, ascent_speed = 0), err )
  expect_error(init_dtcurve(20, 40, ascent_speed = -5), err )
  expect_error(init_dtcurve(20, 40, ascent_speed = c(2,3)), err )
  expect_error(init_dtcurve(20, 40, ascent_speed = "10"), err )
})

test_that("err_init_dtcurve_depth_time", {
  err <- "depth and time must be of same length"
  expect_error(init_dtcurve(c(20,10), 40), err )
  expect_error(init_dtcurve(20, c(20,40)), err )
})

# Test for correct output

test_that("exp_init_dtcurve_depth_time", {
  exp <- data.frame(depth = c(0, 20,20, 0), 
                    time = c(0, 0, 40, 42))
  expect_identical(init_dtcurve(20, 40), exp)
  expect_identical(init_dtcurve(c(0, 20,20, 0), c(0, 0, 40, 42)), exp)
  expect_identical(init_dtcurve(c(20,20, 0), c(0, 40, 42)), exp)
  expect_identical(init_dtcurve(c(20,20), c(0, 40)), exp)
  
  exp$time[4] <- 40.6
  exp$depth <- c(0, 6, 6, 0)
  expect_identical(init_dtcurve(c(6,6), c(0, 40)), exp)
  
  exp <- data.frame(depth = c(0, 6,6, 0), 
                    time = c(0, 0, 40, 40.6))
  expect_identical(init_dtcurve(6, 40), exp)
  expect_identical(init_dtcurve(c(6,6), c(0, 40)), exp)
  
  exp <- data.frame(depth = c(0, 20,15, 20, 0), 
                    time = c(0, 5, 20, 35, 40))
  expect_identical(init_dtcurve(c(20,15), c(5, 20), way = "WB"), exp)
  expect_identical(init_dtcurve(c(0, 20,15), c(0, 5, 20), way = "WB"), exp)
})

#### Test add_desat ####

# Test for correct errors

test_that("err_add_desat_format", {
  dtcurve <- list(depth = c(0, 20, 20, 0),
                  time = c(0, 5, 40, 43))
  desat <- list(desat_stop = data.frame(depth = c(9, 6, 3),time = c(0, 0, 0),
                                        hour = rep(NA, 3),
                                      row.names = c("m9", "m6", "m3")),
              group = "H")
  class(desat) <- "desat"
  err <- "dtcurve must be a data.frame with 2 columns named depth and time without any NA value"
  expect_error(add_desat(dtcurve, desat), err )

  dtcurve <- as.data.frame(dtcurve)
  tmp <- colnames(dtcurve)
  colnames(dtcurve) <- c("depths", "time")
  expect_error(add_desat(dtcurve, desat), err )
  colnames(dtcurve) <- tmp
  dtcurve[2, 1] <- NA
  expect_error(add_desat(dtcurve, desat), err )
})

test_that("err_add_desat_depth", {
  dtcurve <- data.frame(depth = c(0, -20, 20, 0),
                        time = c(0, 5, 40, 43))
  desat <- list(desat_stop = data.frame(depth = c(9, 6, 3),time = c(0, 0, 0),
                                        hour = rep(NA, 3),
                                        row.names = c("m9", "m6", "m3")),
                group = "H")
  class(desat) <- "desat"
  err <- "depth must be positive numeric value."
  expect_error(add_desat(dtcurve, desat), err )
  dtcurve$depth[2] <- "hello"
  expect_error(add_desat(dtcurve, desat), err )
})

test_that("err_add_desat_time", {
  dtcurve <- data.frame(depth = c(0, 20, 20, 0),
                        time = c(0, -5, 40, 43))
  desat <- list(desat_stop = data.frame(depth = c(9, 6, 3),time = c(0, 0, 0),
                                        hour = rep(NA, 3),
                                        row.names = c("m9", "m6", "m3")),
                group = "H")
  class(desat) <- "desat"
  err <- "time must be positive numeric value."
  expect_error(add_desat(dtcurve, desat), err )
  dtcurve$time[2] <- "hello"
  expect_error(add_desat(dtcurve, desat), err )
  dtcurve$time <- c(0,50,40, 43)
  err <- "time values need to be sorted, you don't own a subaquatic dolorean"
  expect_error(add_desat(dtcurve, desat), err )
})

test_that("err_add_desat_ascent_speed", {
  dtcurve <- data.frame(depth = c(0, 20, 20, 0),
                        time = c(0, 5, 40, 43))
  desat <- list(desat_stop = data.frame(depth = c(9, 6, 3),time = c(0, 0, 0),
                                        hour = rep(NA, 3),
                                        row.names = c("m9", "m6", "m3")),
                group = "H")
  class(desat) <- "desat"
  err <- "ascent_speed must be a single positive numeric value."
  expect_error(add_desat(dtcurve, desat, ascent_speed = -5), err )
  expect_error(add_desat(dtcurve, desat, ascent_speed = "hello"), err )
  expect_error(add_desat(dtcurve, desat, ascent_speed = c(0, 0)), err )
})

test_that("err_add_desat_secu", {
  dtcurve <- data.frame(depth = c(0, 20, 20, 0),
                        time = c(0, 5, 40, 43))
  desat <- list(desat_stop = data.frame(depth = c(9, 6, 3),time = c(0, 0, 0),
                                        hour = rep(NA, 3),
                                        row.names = c("m9", "m6", "m3")),
                group = "H")
  class(desat) <- "desat"
  err <- "secu must be TRUE or FALSE"
  expect_error(add_desat(dtcurve, desat, secu = "TRUE"), err )
  expect_error(add_desat(dtcurve, desat, secu = NA), err )
})

test_that("err_add_desat_desat", {
  dtcurve <- data.frame(depth = c(0, 20, 20, 0),
                        time = c(0, 5, 40, 43))
  desat <- list(desat_stop = data.frame(depth = c(9, 6, 3),time = c(0, 0, 0),
                                        hour = rep(NA, 3),
                                        row.names = c("m9", "m6", "m3")),
                group = "H")
  err <- "desat must be of class desat made with desat_model function"
  expect_error(add_desat(dtcurve, desat), err )
})

# Test for correct output

test_that("exp_add_desat", {
  dtcurve <- init_dtcurve(20, 40)
  # dtcurve <- data.frame(depth = c(0, 20, 20, 0),
  #                       time = c(0, 0, 40, 43))
  desat <- list(desat_stop = data.frame(depth = c(9, 6, 3),time = c(0, 0, 0),
                                        hour = rep(NA, 3),
                                        row.names = c("m9", "m6", "m3")),
                group = "H")
  class(desat) <- "desat"
  exp <- data.frame(depth = c(0, 20,20, 0), 
                    time = c(0, 0, 40, 42))
  expect_identical(add_desat(dtcurve, desat), exp)
  exp <- data.frame(depth = c(0, 20,20, 3, 3, 0), 
                    time = c(0, 0, 40, 41.7, 44.7, 45.2))
  expect_identical(add_desat(dtcurve, desat, secu = TRUE), exp)
  desat <- list(desat_stop = data.frame(depth = 5,time = 0, hour = NA,
                                        row.names = "m5"),
                group = "H")
  class(desat) <- "desat"
  expect_identical(add_desat(dtcurve, desat, secu = TRUE), exp)
})

#### minute_to_time ####

test_that("err_minute_to_time", {
  err <- "time must be positive numeric value."
  expect_error(minute_to_time(- 130), err )
  expect_error(minute_to_time("20"), err )
})

test_that("err_minute_to_time_sec", {
  err <- "sec must be TRUE or FALSE"
  expect_error(minute_to_time(130, sec = "TRUE"), err )
  expect_error(minute_to_time(130, sec = NA), err )
})

test_that("err_minute_to_time_day", {
  err <- "day must be TRUE or FALSE"
  expect_error(minute_to_time(130, day = "TRUE"), err )
  expect_error(minute_to_time(130, day = NA), err )
})

test_that("exp_minute_to_time", {
  expect_equal(minute_to_time(130.5, sec = TRUE), "02:10:30")
  expect_equal(minute_to_time(130.5, sec = FALSE), "02:10")
  
  expect_equal(minute_to_time(130.5, sec = TRUE, sep = 'h'), "02h10:30" )
  expect_equal(minute_to_time(130.5, sec = FALSE, sep = 'h'), "02h10" )
  
  expect_equal(minute_to_time(1440, sec = FALSE, sep = 'h'), "00h00" )
  expect_equal(minute_to_time(1664, sec = FALSE, sep = 'h'), "03h44" )
})

