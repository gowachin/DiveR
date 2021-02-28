
#### Test attributes method ####

test_that("exp_pressure_tank", {
  t <- tank(12,200)
  is.tank(t)
  expect_true(is.tank(t))
  expect_false(is.tank('tank'))
})

# Test for correct output
test_that("exp_pressure_tank", {
  t <- tank(12,200)
  expect_equal(unname(pressure(t)), 200)
})

# Test for correct output
test_that("exp_volume_tank", {
  t <- tank(12,200)
  expect_equal(unname(volume(t)), 12)
})


#### Conso attributes method ####

# Test for errors
test_that("err_pressure_tankn", {
  t <- tank(12,200)
  d <- dive(20, 15)
  c <- conso(d, t, cons = 20)
  
  err <- paste("tankn must be a numeric vector with values inferior",
               "or equal to the number of tank used")
  expect_error(pressure(c, tankn = -5), err )
  expect_error(pressure(c, tankn = 0), err )
  expect_error(pressure(c, tankn = '1'), err )
  expect_error(pressure(c, tankn = 2), err )
  
  err <- "tankn can't have fractionnal part"
  expect_error(pressure(c, tankn = 0.9), err )
})

test_that("err_pressure_time", {
  t <- tank(12,200)
  d <- dive(20, 15)
  c <- conso(d, t, cons = 20)
  
  err <- paste("time must be a numeric value inside the time",
               "of the dive. Check the times of the dive and you usage of",
               "the hour parameter")
  expect_error(pressure(c, time = -5), err )
  expect_error(pressure(c, time = '1'), err )
  expect_error(pressure(c, time = 25), err )
  expect_error(pressure(c, time = c(7, 10) ), err )
  
  err <- 'hour must be TRUE or FALSE'
  expect_error(pressure(c, time = 2, hour = 2), err )
  expect_error(pressure(c, time = 2, hour = "TRUE"), err )
  expect_error(pressure(c, time = 2, hour = NA), err )
})


test_that("exp_pressure_time", {
  t1 <- tank(vol = 12, press = 200)
  t2 <- tank(vol = 10, press = 200)
  d <- dive(20, 15)
  c <- conso(d, t1, cons = 20)
  
  expect_equal(unname(pressure(c)), 111.45)
  expect_equal(unname(pressure(c, time = 10)), 150)
  
  c <- conso(d, list(t1,t2), cons = 20)
  
  expect_equal(unname(pressure(c)), c(111.45, 200.00))
  expect_equal(unname(pressure(c, time = 10)), c(150, 200))
  
  expect_equal(unname(pressure(c, tankn = 1)), 111.45)
  expect_equal(unname(pressure(c, tankn = 1, time = 10)), 150)
  
  d <- dive(20, 15, hour = 1776)
  c <- conso(d, t1, cons = 20)
  expect_equal(unname(pressure(c, time = 1786, hour = TRUE)), 150)
  
  d <- dive(20, 40)
  suppressWarnings(c <- conso(d, t1, cons = 20)) 
  # warnings are watch by another test
  expect_equal(unname(pressure(c)), 0)
  
})
