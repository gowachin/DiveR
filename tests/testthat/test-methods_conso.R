
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
  expect_equal(pressure(t), 200)
})

# Test for correct output
test_that("exp_volume_tank", {
  t <- tank(12,200)
  expect_equal(volume(t), 12)
})

# Test for correct output
test_that("exp_rules_tank", {
  t <- tank(12,200)
  expect_equal(rules(t), c('mid' = 100, 'res' = 50))
})


# Test for correct output
test_that("exp_summary_tank", {
  t <- tank(12,200)
  mess <- "Tank : 12 litre at 200 bar
rules : 1 mid : 100 bar
        2 res :  50 bar
The tank type is back and contain Air 
Named : back12 "
  expect_equal(capture_output(summary(t)), mess)
})


#### Conso attributes method ####

# Pressures
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


test_that("exp_pressure_conso", {
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

test_that("exp_summary_conso", {
  simp_dive <- dive(depth = 20, time = 40, secu = TRUE)
  Tank_12L <- tank(vol = 12, press = 200)
  death <- suppressWarnings(conso(dive = simp_dive, tank = Tank_12L, 
                 cons = 20, failure_label = 'Air failure'))
  Tank_15L <- tank(vol = 15, press = 200)
  viable <- conso(dive = simp_dive, tank = Tank_15L, 
                  cons = 20, failure_label = 'Air failure')
  
  mess <- "Consumption simulated on dive at 20 m for 45.2 minutes
---------------------------------------------------------------------
       Tank name |         Rule | Pressure |    Time | Final pressure 
---------------------------------------------------------------------
     Tank back12 |          mid |  100 bar |  20 min | 0 bar
                 |          res |   50 bar |  30 min |    
                 |  Air failure |    0 bar |  40 min |     
---------------------------------------------------------------------
The dive is deadly !"
  expect_equal(capture_output(summary(death)), mess)
  mess <- "Consumption simulated on dive at 20 m for 45.2 minutes
---------------------------------------------------------------------
       Tank name |         Rule | Pressure |    Time | Final pressure 
---------------------------------------------------------------------
     Tank back15 |          mid |  100 bar |  25 min | 29.16 bar
                 |          res |   50 bar |  38 min |    
                 |  Air failure |    0 bar |  NA min |     
---------------------------------------------------------------------
The dive is viable !"
  expect_equal(capture_output(summary(viable)), mess)
  
  
  A_10L <- tank(vol = 10, press = 200, name = 'A_10L')
  B_10L <- tank(vol = 10, press = 200, name = 'B_10L')
  bi_conso <- conso(dive = simp_dive, tank = list(A_10L, B_10L), 
                    cons = 20, failure_label = 'Air failure')
  mess <- "Consumption simulated on dive at 20 m for 45.2 minutes
---------------------------------------------------------------------
       Tank name |         Rule | Pressure |    Time | Final pressure 
---------------------------------------------------------------------
      Tank A_10L |          mid |  100 bar |  17 min | 50 bar
                 |          res |   50 bar |  43 min |    
                 |  Air failure |    0 bar |  NA min |     
---------------------------------------------------------------------
      Tank B_10L |          mid |  100 bar |  33 min | 93.74 bar
                 |          res |   50 bar |  NA min |    
                 |  Air failure |    0 bar |  NA min |     
---------------------------------------------------------------------
The dive is viable !"
  expect_equal(capture_output(summary(bi_conso)), mess)
})

# Rules
test_that("err_rules_tankn", {
  t <- tank(12,200)
  d <- dive(20, 15)
  c <- conso(d, t, cons = 20)
  
  err <- paste("tankn must be a numeric vector with values inferior",
               "or equal to the number of tank used")
  expect_error(rules(c, tankn = -5), err )
  expect_error(rules(c, tankn = 0), err )
  expect_error(rules(c, tankn = '1'), err )
  expect_error(rules(c, tankn = 2), err )
  
  err <- "tankn can't have fractionnal part"
  expect_error(rules(c, tankn = 0.9), err )
  
  err <- 'hour must be TRUE or FALSE'
  expect_error(rules(c, hour = 2), err )
  expect_error(rules(c, hour = "TRUE"), err )
  expect_error(rules(c, hour = NA), err )
})


test_that("err_rules_conso", {
  t <- tank(12,200)
  d <- dive(20, 15)
  cons <- conso(d, t, cons = 20)

  err <- "n must be a numeric vector with values between 0 and 2"
  expect_error(rules(cons, n = -5), err )
  expect_error(rules(cons, n = '1'), err )
  expect_error(rules(cons, n = 25), err )
  expect_error(rules(cons, n = c(1, 10) ), err )

})


test_that("exp_rules_conso", {
  t1 <- tank(vol = 12, press = 200)
  t2 <- tank(vol = 10, press = 200)
  d <- dive(20, 15)
  c <- conso(d, t1, cons = 20)
  
  expect_equal(rules(c), c$rules)
  expect_equal(rules(c, tankn = 1), c$rules)
  
  expect_equal(rules(c), c$rules)
  expect_equal(rules(c, n = 1), c$rules[,1:3])
  expect_equal(rules(c, n = 2), c$rules[,4:6])
  expect_equal(rules(c, n = c(1,2)), c$rules[,1:6])
  expect_equal(rules(c, n = 0), c$rules[,7:9])
  expect_equal(rules(c, n = c(1,0)), c$rules[,c(1:3,7:9)])
  expect_equal(rules(c, n = c(2,0)), c$rules[,c(4:9)])
  expect_equal(rules(c, n = c(1,2,0)), c$rules)
  
  c <- conso(d, list(t1,t2), cons = 20)
  
  expect_equal(rules(c), c$rules)
  expect_equal(rules(c, tankn = 2), c$rules[2,])
  expect_equal(rules(c, tankn = 2, n = 1), c$rules[2, 1:3])
  expect_equal(rules(c, tankn = 2, n = 2), c$rules[2, 4:6])
  expect_equal(rules(c, tankn = 2, n = c(1,2)), c$rules[2, 1:6])
  expect_equal(rules(c, tankn = 2, n = 0), c$rules[2, 7:9])
  expect_equal(rules(c, tankn = 2, n = c(1,0)), c$rules[2, c(1:3,7:9)])
  expect_equal(rules(c, tankn = 2, n = c(2,0)), c$rules[2, c(4:9)])
  expect_equal(rules(c, tankn = 2, n = c(1,2,0)), c$rules[2, ])
  
  d <- dive(20, 40, hour = 1776)
  suppressWarnings(c <- conso(d, t1, cons = 20))
  r <- c$rules
  r[, c(3, 6, 9)] <- r[, c(3, 6, 9)] + c$hour[1]
  expect_equal(rules(c, hour = TRUE), r)

  
})

