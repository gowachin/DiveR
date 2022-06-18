
#### Test attributes method ####

test_that("exp_is_dive", {
  d <- dive(20,40)
  expect_true(is.dive(d))
  expect_false(is.dive('dive'))
})

test_that("exp_depth_dive", {
  d <- dive(20,40)
  expect_equal(depth(d), 20)
  d <- dive(c(20, 10),c(10, 40))
  expect_equal(depth(d), 20)
})

test_that("exp_depth_ndive", {
  d1 <- dive(20,40)
  d2 <- dive(30,40)
  nd <- ndive(d1, d2, inter = 721)
  expect_equal(depth(nd), c(20,30))
})

test_that("exp_altitude_dive", {
  d <- dive(20, 40)
  expect_equal(altitude(d), 0)
  d <- dive(20, 40, altitude = 500)
  expect_equal(altitude(d), 500)
})

test_that("exp_altitude_ndive", {
  d1 <- dive(20,40, altitude = 500)
  d2 <- dive(30,40, altitude = 450)
  nd <- ndive(d1, d2, inter = 721)
  expect_equal(altitude(nd), c(500,450))
})

test_that("exp_altitude_pressure",{
  expect_equal(altitude_pressure(0), 1)
  expect_equal(altitude_pressure(1e3), 0.9)
})

test_that("exp_altitude_depth",{
  expect_equal(altitude_depth(depth = 20), 20)
  expect_equal(altitude_depth(depth = 24, altitude = 2000), 30)
})

test_that("exp_dtime_dive", {
  d <- dive(20,40)
  expect_equal(dtime(d), 40)
  d <- dive(c(20, 10),c(10, 40))
  expect_equal(dtime(d), 40)
})

test_that("exp_depth_ndive", {
  d1 <- dive(20,40)
  d2 <- dive(30,40)
  nd <- ndive(d1, d2, inter = 721)
  expect_equal(dtime(nd), c(40,40))
})

# test for correct input

test_that("exp_depth_time_dive", {
  d <- dive(20,40)
  expect_equal(depth_at_time(d, 10), 20)
  expect_equal(depth_at_time(d, 40), 20)
  expect_equal(depth_at_time(d, 42), 3)
  expect_equal(depth_at_time(d, 50), 0)
  d <- dive(depth = c(0, 20, 20, 10, 10, 7), 
            time = c(0, 2, 15, 20, 35,  40))
  expect_equal(depth_at_time(d, 10), 20)
  expect_equal(depth_at_time(d, 20), 10)
  expect_equal(depth_at_time(d, 40), 7)
  expect_equal(depth_at_time(d, 42), 3)
  expect_equal(depth_at_time(d, 50), 0)
})


test_that("exp_dtr_dive", {
  d <- dive(20,40)
  expect_equal(dtr(d), 5.2)
  d <- dive(depth = c(0, 20, 20, 10, 10, 7), 
            time = c(0, 2, 15, 20, 35,  40))
  expect_equal(dtr(d), 3.9)
})

test_that("exp_ppo2_dive", {
  d <- dive(20,40)
  expect_equal(ppo2(d), 0.209)
  d <- dive(20,40, gas = "NX40")
  expect_equal(ppo2(d), 0.4)
  nd <- ndive(d, d)
  expect_equal(ppo2(nd), c(0.4, 0.4))
})

#### Summary ####

test_that("exp_summary_dive", {
  object <- dive(20, 40, secu = FALSE)
  mess <- "--------------------------------------------------
Maximum depth :  20 m  | Depth dive time :  40 min 
 Dive ascent :   2 min | Underwater time :  42 min
  Majoration :   0 min | Security stop : FALSE 
  Start :     00:00:00 | End :     00:42:00 
--------------------------------------------------

|- Desaturation -|
--------- No desat stop ---------
    Group : H | Model :   table "
  expect_equal(capture_output(summary(object)), mess)
  
  object <- dive(20, 40)
  mess <- "--------------------------------------------------
Maximum depth :  20 m  | Depth dive time :  40 min 
 Dive ascent :   5 min | Underwater time :  45 min
  Majoration :   0 min | Security stop :  TRUE 
  Start :     00:00:00 | End :     00:45:12 
--------------------------------------------------

|- Desaturation -|
---------------------------------
 Stop | Depth | Duration |   Time 
---------------------------------
 n  3 |   3 m |    3 min | 42 min 
---------------------------------
    Group : H | Model :   table "
  expect_equal(capture_output(summary(object)), mess)
  
  object <- dive(20, 45)
  mess <- "--------------------------------------------------
Maximum depth :  20 m  | Depth dive time :  45 min 
 Dive ascent :   6 min | Underwater time :  51 min
  Majoration :   0 min | Security stop :  TRUE 
  Start :     00:00:00 | End :     00:51:12 
--------------------------------------------------

|- Desaturation -|
---------------------------------
 Stop | Depth | Duration |   Time 
---------------------------------
 n  3 |   3 m |    4 min | 47 min 
---------------------------------
    Group : I | Model :   table "
  expect_equal(capture_output(summary(object)), mess)
  
  object <- dive(39, 22)
  mess <- "--------------------------------------------------
Maximum depth :  39 m  | Depth dive time :  22 min 
 Dive ascent :  28 min | Underwater time :  50 min
  Majoration :   0 min | Security stop :  TRUE 
  Start :     00:00:00 | End :     00:50:18 
--------------------------------------------------

|- Desaturation -|
---------------------------------
 Stop | Depth | Duration |   Time 
---------------------------------
 n  2 |   6 m |    2 min | 25 min 
---------------------------------
 n  3 |   3 m |   22 min | 28 min 
---------------------------------
    Group : J | Model :   table "
  expect_equal(capture_output(summary(object)), mess)
  
  object <- dive(50, 22)
  mess <- "--------------------------------------------------
Maximum depth :  50 m  | Depth dive time :  22 min 
 Dive ascent :  50 min | Underwater time :  72 min
  Majoration :   0 min | Security stop :  TRUE 
  Start :     00:00:00 | End :     01:12:36 
--------------------------------------------------

|- Desaturation -|
---------------------------------
 Stop | Depth | Duration |   Time 
---------------------------------
 n  1 |   9 m |    1 min | 26 min 
---------------------------------
 n  2 |   6 m |    8 min | 28 min 
---------------------------------
 n  3 |   3 m |   35 min | 36 min 
---------------------------------
    Group : L | Model :   table "
  expect_equal(capture_output(summary(object)), mess)
})


#### rm ####

test_that("err_rm_secu_dive", {
  err <- "dive must be a dive object"
  expect_error(rm_secu("dive(20, 40)"), err )
})

test_that("exp_rm_secu_dive", {
  expect_equal(rm_secu(dive(20, 40, secu = TRUE)), dive(20, 40, secu = FALSE))
  expect_equal(rm_secu(dive(39, 22, secu = TRUE)), dive(39, 22, secu = FALSE))
})

# test for correct input
test_that("err_rm_desat_dive", {
  err <- "dive must be a dive object"
  expect_error(rm_desat("dive(20, 40)"), err )
})

test_that("exp_rm_desat_dive", {
  expect_equal(rm_desat(dive(20, 40)), 
               suppressMessages(dive(20, 40, desat_model = "other",
                                     secu = FALSE)))
})


