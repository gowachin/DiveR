
#### Test dive object ####

# Test for correct input
test_that("err_dive_depth", {
  err <- "depth must be positive numeric value\\(s\\)."
  expect_error(dive(-5, 40), err )
  expect_error(dive("20", 40), err )
})

test_that("err_dive_time", {
  err <- "time must be positive numeric value\\(s\\)."
  expect_error(dive(20, -40), err )
  expect_error(dive(20, "20"), err )
})

test_that("err_dive_secu", {
  err <- "secu must be TRUE or FALSE"
  expect_error(dive(20, 40, secu = "TRUE"), err )
  expect_error(dive(20, 40, secu = NA), err )
})

test_that("err_dive_ascent", {
  err <- "ascent_speed must be a single positive numeric value."
  expect_error(dive(20, 40, ascent_speed = 0), err )
  expect_error(dive(20, 40, ascent_speed = -5), err )
  expect_error(dive(20, 40, ascent_speed = c(2,3)), err )
  expect_error(dive(20, 40, ascent_speed = "10"), err )
  err <- "This is not the sport to do if you want to go to the moon"
  expect_error(dive(20, 40, ascent_speed = 150), err )
})

test_that("err_dive_maj", {
  err <- "maj must be a single positive numeric value."
  expect_error(dive(20, 40, maj = -5), err )
  expect_error(dive(20, 40, maj = c(2,3)), err )
  expect_error(dive(20, 40, maj = "10"), err )
})

test_that("err_dive_hour", {
  err <- "hour must be a single positive numeric value in minute."
  expect_error(dive(20, 40, hour = -5), err )
  expect_error(dive(20, 40, hour = c(2,3)), err )
  expect_error(dive(20, 40, hour = "10"), err )
})

test_that("war_init_dtcurve_ascent_sp", {
  war <- "Ascent speed is usually set between 10 and 20 m/min in most desaturation models. 
6m/min is used between 6m and the surface"
  expect_warning(dive(20, 40, ascent_speed = 7), war )
  expect_warning(dive(20, 40, ascent_speed = 16), war )
})

# Test for correct output
test_that("dive_square_output", {
  d <- dive(depth = 39, time = 22, secu = TRUE, ascent_speed = 10)
  
  exp <- list(
    dtcurve = data.frame(depths = c(0, 39, 39,  6, 6, 3, 3, 0), 
                         times = c(0, 0, 22, 25.3, 27.3, 27.8, 49.8, 50.3)),
    desat = list(desat_stop = data.frame(depth = c(9, 6, 3), time = c(0, 2, 22),
                                         hour = c(NA, 25.3, 27.8), 
                                         row.names = paste0("m", c(9,6,3))),
                 group = "J", model = "table"),
    hour = c(0, 50.3),
    params = c(maj = 0, secu = 1, ascent_speed = 10, dtr = 28.3, ppo2 = 0.209)
  )
  class(exp) <- "dive"
  class(exp$desat) <- "desat"
  expect_equal(d, exp)
  
  suppressMessages(d <- dive(20, 40, desat_model = "other", secu = TRUE))
  exp <- list(
    dtcurve = data.frame(depths = c(0, 20, 20, 3, 3,  0),  
                         times = c(0, 0, 40, 41.7, 44.7, 45.2)),
    desat = list(desat_stop = data.frame(depth = c(0, 3), time = c(0, 3),
                                         hour = c(NA, 41.7), 
                                         row.names = paste0("m", c(0,3))),
                 group = "Z", model = "other"),
    hour = c(0, 45.2),
    params = c(maj = 0, secu = 1, ascent_speed = 10, dtr = 5.2, ppo2 = 0.209)
  )
  class(exp) <- "dive"
  class(exp$desat) <- "desat"
  expect_equal(d, exp)
})

test_that("dive_curve_output", {
  d <- dive(depth = c(0, 20, 19, 10, 7), 
            time = c(0, 2, 15, 20,  40)) 
  exp <- list(
    dtcurve = data.frame(depths = c(0, 20, 19, 10, 7, 3, 3, 0), 
                         times = c(0, 2, 15, 20,  40, 40.4, 43.4, 43.9)),
    desat = list(desat_stop = data.frame(depth = c(9, 6, 3), time = c(0, 0, 3),
                                         hour = c(NA, NA, 40.4), 
                                         row.names = paste0("m", c(9,6,3))),
                 group = "H", model = "table"),
    hour = c(0, 43.9),
    params = c(maj = 0, secu = 1, ascent_speed = 10, dtr = 3.9, ppo2 = 0.209)
  )
  class(exp) <- "dive"
  class(exp$desat) <- "desat"
  expect_equal(d, exp)
  
  
  d <- dive(depth = c(0, 20, 19, 10, 2), 
            time = c(0, 2, 15, 20, 40)) 
  exp$dtcurve$depths[5] <- 2
  exp$dtcurve$times <- c(0, 2, 15, 20, 40, 40.1, 43.1, 43.6)
  exp$desat$desat_stop[3,3] <- 40.1
  exp$params["dtr"] <- 3.6
  exp$hour[2] <- 43.6
  expect_equal(d, exp)
  
  d <- dive(depth = c(0, 39, 30, 15, 7), 
            time = c(0, 2, 7, 15, 22))
  exp <- list(
    dtcurve = data.frame(depths = c(0, 39, 30, 15, 7, 6, 6, 3, 3, 0), 
                         times = c(0, 2, 7, 15, 22, 22.1, 24.1, 24.6, 46.6, 47.1)),
    desat = list(desat_stop = data.frame(depth = c(9, 6, 3), time = c(0, 2, 22),
                                         hour = c(NA, 22.1, 24.6), 
                                         row.names = paste0("m", c(9,6,3))),
                 group = "J", model = "table"),
    hour = c(0, 47.1),
    params = c(maj = 0, secu = 1, ascent_speed = 10, dtr = 25.1, ppo2 = 0.209)
  )
  class(exp) <- "dive"
  class(exp$desat) <- "desat"
  expect_equal(d, exp)
  
  d <- dive(depth = c(0, 39, 30, 15, 3), 
            time = c(0, 2, 7, 15, 22))
  exp$dtcurve$depths[5] <- 3
  exp$dtcurve$times <- c(0, 2, 7, 15, 22, 22.3, 24.3, 24.8, 46.8, 47.3)
  exp$desat$desat_stop[c(2,3),3] <- c(22.3, 24.8)
  exp$params["dtr"] <- 25.3
  exp$hour[2] <- 47.3
  expect_equal(d, exp)
  
})

##### Test ndive ####

# test for correct input
test_that("err_ndive_dives", {
  err <- "dive1 must be a dive object"
  expect_error(ndive("dive(20, 40)", dive(20, 40), inter = 730), err )
  err <- "dive1 must be a dive object"
  expect_error(ndive("dive(20, 40)", "dive(20, 40)", inter = 730), err )
  err <- "dive2 must be a dive object"
  expect_error(ndive(dive(20, 40), "dive(20, 40)", inter = 730), err )
})

test_that("err_ndive_inter", {
  err <- "inter must be positive numeric value."
  expect_error(ndive(dive(20, 40), dive(20, 40), inter = -10), err )
  expect_error(ndive(dive(20, 40), dive(20, 40), inter = "10"), err )
  expect_error(ndive(dive(20, 40), dive(20, 40), inter = c(10, 20)), err )
})

test_that("err_ndive_verbose", {
  err <- "verbose must be TRUE or FALSE"
  expect_error(ndive(dive(20, 40), dive(20, 40), verbose = "TRUE"), err )
  expect_error(ndive(dive(20, 40), dive(20, 40), verbose = NA), err )
})

test_that("err_ndive_model", {
  err <- "There is no other model yet"
  expect_error(ndive(dive(20, 40), 
                     suppressMessages(dive(20, 40, desat_model = "other"))), 
               err )
})
