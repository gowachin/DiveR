
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

# test_that("exp_depth_ndive", {
#   d1 <- dive(20,40)
#   d2 <- dive(30,40)
#   nd <- ndive(d1, d2, inter = 721)
#   expect_equal(depth(nd), c(20,30))
# })

test_that("exp_dtime_dive", {
  d <- dive(20,40)
  expect_equal(dtime(d), 40)
  d <- dive(c(20, 10),c(10, 40))
  expect_equal(dtime(d), 40)
})

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


