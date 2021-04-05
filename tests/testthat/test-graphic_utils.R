#### test depth_inf ####

# test for correct input
# test_that("err_depth_inf_x", {
#   err <- "x must be a dive object"
#   expect_error(depths_inf("dive(20, 40)"), err )
# })

test_that("err_depth_inf_only_desat", {
  err <- "only_desat must be TRUE or FALSE"
  expect_error(depths_inf(dive(20, 40), only_desat = "TRUE"), err )
  expect_error(depths_inf(dive(20, 40), only_desat = NA), err )
})

test_that("depth_inf_output", {
  d <- dive(depth = c(0, 20, 19, 10, 7), 
            time = c(0, 2, 15, 20,  40)) 
  exp <- data.frame(x = c(2, 15, 20, 40, 43.4), y = c(-20, -19, -10, -7, -3),
                    labels = c("-20 m", "-19 m", "-10 m", "-7 m", "-3 m"), 
                    pos = c(1, 4, 4, 4, 4), col = rep("black", 5), 
                    stringsAsFactors = FALSE)
  expect_equal(depths_inf(d), exp)
})

test_that("depth_inf_output", {
  d <- dive(depth = c(0, 20, 19, 10, 7), 
            time = c(0, 2, 15, 20,  40)) 
  exp <- data.frame(x = c(NA, NA, 43.4), y = c(-9, -6, -3),
                    labels = c("-9 m", "-6 m", "-3 m"), 
                    pos = c(4, 4, 4), col = rep("black", 3), 
                    stringsAsFactors = FALSE)
  expect_equal(depths_inf(d, only_desat = TRUE), exp)
})

#### test time_inf ####

# test for correct input
# test_that("err_time_inf_x", {
#   err <- "x must be a dive object"
#   expect_error(times_inf("dive(20, 40)"), err )
# })

test_that("time_inf_output", {
  d <- dive(depth = c(0, 20, 19, 10, 7), 
            time = c(0, 2, 15, 20,  40)) 
  exp <- data.frame(x = c(8.5, 17.5, 30.0, 41.9, NA), 
                    y = c(-19.5, -14.5, -8.5, -3.0, -9.0),
                    labels = c("13'", "5'", "20'", "3'", "0'"), 
                    pos = rep(3, 5), col = rep("black", 5), 
                    stringsAsFactors = FALSE)
  expect_equal(times_inf(d), exp)
})


