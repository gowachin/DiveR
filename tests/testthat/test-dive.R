
#### Test dive object ####

# Test for correct input
test_that("err_dive_depth", {
  err <- "depth must be positive numeric value\\(s\\)."
  expect_error(dive(-5, 40), err )
  expect_error(dive("20", 40), err )
})

test_that("err_dive_time", {
  err <- "time must be positive numeric value\\(s\\)."
  expect_error(dive(20, -40), err )
  expect_error(dive(20, "20"), err )
})

test_that("err_dive_secu", {
  err <- "secu must be TRUE or FALSE"
  expect_error(dive(20, 40, secu = "TRUE"), err )
  expect_error(dive(20, 40, secu = NA), err )
})

test_that("err_dive_secu", {
  err <- "ascent_speed must be a single positive numeric value\\(s\\)."
  expect_error(dive(20, 40, ascent_speed = 0), err )
  expect_error(dive(20, 40, ascent_speed = -5), err )
  expect_error(dive(20, 40, ascent_speed = c(2,3)), err )
  expect_error(dive(20, 40, ascent_speed = "10"), err )
})

test_that("err_dive_secu", {
  err <- "maj must be a single positive numeric value."
  expect_error(dive(20, 40, maj = -5), err )
  expect_error(dive(20, 40, maj = c(2,3)), err )
  expect_error(dive(20, 40, maj = "10"), err )
})
# 
# test_that("war_tank_rules", {
#   # Warnings
#   war <- 'NA values in rules are set to 0 with empty names'
#   expect_warning(t <- tank(vol = 12, press = 200, 
#                            rules = list(rules= c('mid' = 50, 'res' = NA), sys = '%')),
#                  war )
#   expect_equal(unname(t$carac['rule2']), 0)
# })
# 
# # Test for correct output
test_that("dive_output", {
  d = dive(depth = 39, time = 22, secu = TRUE, ascent_speed = 10)
  plot(d)
  
  # t <- tank(vol = 12, press = 200)
  # # text if init of rules
  # expect_equal(unname(t$carac[c('rule1','rule2')]), c(100, 50))
  # # test if init of typo
  # expect_equal(unname(t$typo[c('rule1','rule2')]), c("mid", "res"))
  # # test if init of limit
  # l <- c(0, 66, NA, NA)
  # names(l) <- c('mind', 'maxd', 't1', 't2')
  # expect_equal(t$limit, l)
  # # test if init name
  # expect_equal(unname(t$typo['name']), 'back12')
  # # test class
  # expect_s3_class(t, 'tank', exact = FALSE)
  # 
  # t <- tank(vol = 15, press = 100, typ = 'relay',
  #           rules = list(rules = c('return' = 50, 'end' = 25), sys = 'bar'))
  # expect_equal(unname(t$carac[c('rule1','rule2')]), c(50, 25))
  # expect_equal(unname(t$typo['name']), 'relay15')
  # expect_equal(unname(t$typo[c('rule1','rule2')]), c("return", "end"))
})

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



