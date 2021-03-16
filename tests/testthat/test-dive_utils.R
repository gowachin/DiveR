
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

test_that("war_init_dtcurve_ascent_sp", {
  war <- "Ascent speed is usually set between 10 and 20 m/min in most desaturation models. 
6m/min is used between 6m and the surface"
  expect_warning(init_dtcurve(20, 40, ascent_speed = 7), war )
  expect_warning(init_dtcurve(20, 40, ascent_speed = 16), war )
})

test_that("err_init_dtcurve_depth_time", {
  err <- "depth and time must be of same length"
  expect_error(init_dtcurve(c(20,10), 40), err )
  expect_error(init_dtcurve(20, c(20,40)), err )
})

# Test for correct output

test_that("exp_init_dtcurve_depth_time", {
  exp <- data.frame(depth = c(0, 20,20, 0), 
                    time = c(0, 0, 40, 42.4))
  expect_identical(init_dtcurve(20, 40), exp)
  expect_identical(init_dtcurve(c(0, 20,20, 0), c(0, 0, 40, 42.4)), exp)
  expect_identical(init_dtcurve(c(20,20, 0), c(0, 40, 42.4)), exp)
  expect_identical(init_dtcurve(c(20,20), c(0, 40)), exp)
  
  exp$time[4] <- 41
  exp$depth <- c(0, 6, 6, 0)
  expect_identical(init_dtcurve(c(6,6), c(0, 40)), exp)
  
  exp <- data.frame(depth = c(0, 6,6, 0), 
                    time = c(0, 0, 40, 41))
  expect_identical(init_dtcurve(6, 40), exp)
  expect_identical(init_dtcurve(c(6,6), c(0, 40)), exp)
  
  exp <- data.frame(depth = c(0, 20,15, 20, 0), 
                    time = c(0, 5, 20, 35, 40))
  expect_identical(init_dtcurve(c(20,15), c(5, 20), way = "WB"), exp)
  expect_identical(init_dtcurve(c(0, 20,15), c(0, 5, 20), way = "WB"), exp)
})
