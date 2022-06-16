
## Test half_life ####

test_that("exp_half_life", {
  expect_equal(half_life(50, 50), 50 )
  expect_equal(half_life(c(50, 25), 50),  c(50, 75) )
  expect_equal(half_life(c(50, 25), c(50, 25)), c(50, 50))
})
