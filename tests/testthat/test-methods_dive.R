
#### Test attributes method ####

test_that("exp_is_dive", {
  d <- dive(20,40)
  expect_true(is.dive(d))
  expect_false(is.dive('dive'))
})

test_that("exp_depth_dive", {
  d <- dive(20,40)
  expect_equal(depth(d), 20)
})

test_that("exp_depth_ndive", {
  d1 <- dive(20,40)
  d2 <- dive(30,40)
  nd <- ndive(d1, d2, inter = 721)
  expect_equal(depth(nd), c(20,30))
})
