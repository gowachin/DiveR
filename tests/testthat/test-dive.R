
#### Test square dive object ####
# test_that("multiplication works", {
#   expect_equal(2 * 2, 4)
# })

#### Test square dive attributes ####
# depth.dive
test_that("sq_depth.dive", {
  d <- dive(20,40)
  expect_equal(depth(d), 20)
})

# dtime.dive
test_that("sq_dtime.dive", {
  d <- dive(20,40)
  expect_equal(dtime(d), 40)
})

# dtr.dive
test_that("sq_dtr.dive", {
  d <- dive(20,40)
  expect_equal(dtr(d), 5.2)
  
  d <- dive(20, 40, secu = FALSE)
  expect_equal(dtr(d), 2)
})
