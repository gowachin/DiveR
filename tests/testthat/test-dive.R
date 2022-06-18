
#### Test dive object ####

# Test for correct input
test_that("err_dive_ascent", {
  err <- "This is not the sport to do if you want to go to the moon"
  expect_error(dive(20, 40, ascent_speed = 150), err )
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
    params = c(maj = 0, altitude = 0, secu = 1, ascent_speed = 10, 
               dtr = 28.3, ppo2 = 0.209)
  )
  class(exp) <- "dive"
  class(exp$desat) <- "desat"
  expect_equal(d, exp)
  
  d <- dive(depth = 24, time = 40, altitude = 2000, secu = FALSE, 
            ascent_speed = 10)
  
  exp <- list(
    dtcurve = data.frame(depths = c(0, 24, 24, 2.4, 2.4, 0), 
                         times = c(0, 0, 40, 42.16, 66.16, 66.56)),
    desat = list(desat_stop = data.frame(depth = c(7.2, 4.8, 2.4), 
                                         time = c(0, 0, 24),
                                         hour = c(NA, NA, 42.16), 
                                         row.names = paste0("m", c(9,6,3))),
                 group = "K", model = "table"),
    hour = c(0, 66.56),
    params = c(maj = 0, altitude = 2000, secu = 0, ascent_speed = 10, 
               dtr = 26.560, ppo2 = 0.209)
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
    params = c(maj = 0, altitude = 0, secu = 1, ascent_speed = 10, 
               dtr = 5.2, ppo2 = 0.209)
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
    params = c(maj = 0, altitude = 0, secu = 1, ascent_speed = 10, 
               dtr = 3.9, ppo2 = 0.209)
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
    params = c(maj = 0, altitude = 0, secu = 1, ascent_speed = 10, 
               dtr = 25.1, ppo2 = 0.209)
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


test_that("err_ndive_model", {
  err <- "There is no other model yet"
  expect_error(ndive(dive(20, 40), 
                     suppressMessages(dive(20, 40, desat_model = "other"))), 
               err )
})
