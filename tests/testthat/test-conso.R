
#### Test tank object ####

# Test for correct input
test_that("err_tank_vol", {
  err <- "vol must be a single positive numeric value."
  expect_error(tank(vol = 0, press = 200), err )
  expect_error(tank(vol = -5, press = 200), err )
  expect_error(tank(vol = '12', press = 200), err )
  expect_error(tank(vol = c(7.5, 7.5), press = 200), err )
})

test_that("err_tank_press", {
  err <- 'press must be a single positive, 0 possible, numeric value.'
  expect_error(tank(vol = 12, press = -50), err )
  expect_error(tank(vol = 12, press = '12'), err )
  expect_error(tank(vol = 12, press = c(100, 100)), err )
})

test_that("err_tank_rules", {
  # Errors
  err <- paste('rules must be a list of length 2 with a vector of 2 numeric',
               'named rules and a single character string being % or bar')
  expect_error(tank(vol = 12, press = 200, 
                    rules = list(rules = c('mid' = 50,'res' = 25))), err )
  expect_error(tank(vol = 12, press = 200, 
                    rules = list(rules = c('mid' = 50,'res' = 25), syst = '%')),
               err )
  err <- 'Element rules of rules argument must be a vector of 2 numeric'
  expect_error(tank(vol = 12, press = 200, 
                    rules = list(rules = c('mid' = 50), sys = '%')),
               err )
})

test_that("war_tank_rules", {
  # Warnings
  war <- 'NA values in rules are set to 0 with empty names'
  expect_warning(t <- tank(vol = 12, press = 200, 
                      rules = list(rules= c('mid' = 50, 'res' = NA), sys = '%')),
                 war )
  expect_equal(unname(t$carac['rule2']), 0)
  
  war <- 'negative rules are not possible and therefor set to 0'
  expect_warning(t <- tank(vol = 12, press = 200, 
                    rules = list(rules= c('mid' = 50, 'res' = -10), sys = '%')),
               war )
  expect_equal(unname(t$carac['rule2']), 0)
  
  war <- 'There was no names for rules, consider setting them for later use'
  expect_warning(t <- tank(vol = 12, press = 200, 
                      rules = list(rules=c(50, 10), sys = '%')),
                 war )
  expect_equal(unname(t$typo[c('rule1','rule2')]), c("", ""))
  
  war <- paste('The rule is superior to 100 % Therefore it is changed to',
               'the maximum pression')
  expect_warning(t <- tank(vol = 12, press = 200, 
                      rules=list(rules= c('mid' = 110, 'res' = 50), sys = '%')),
                 war )
  expect_equal(unname(t$carac['rule1']), 200)
  
  war <- paste('The rule is superior to the pression in the tank.',
               'Therefore it is changed to the maximum pression')
  expect_warning(t <- tank(vol = 12, press = 20, 
                      rules=list(rules= c('mid' = 30, 'res' = 10), sys ='bar')),
                 war )
  expect_equal(unname(t$carac['rule1']), 20)
  
  war <- 'Rule 1 must be superior to rule 2, the order is therefor inversed'
  expect_warning(t <- tank(vol = 12, press = 200, 
                           rules=list(rules= c('mid' = 50, 'res' = 100), 
                                      sys ='bar')),
                 war )
  expect_equal(t$carac[c('rule1','rule2')], c('rule1' = 100, 'rule2' = 50))
})

# Test for correct output
test_that("tank_output", {
  t <- tank(vol = 12, press = 200)
  # text if init of rules
  expect_equal(unname(t$carac[c('rule1','rule2')]), c(100, 50))
  # test if init of typo
  expect_equal(unname(t$typo[c('rule1','rule2')]), c("mid", "res"))
  # test if init of limit
  l <- c(0, 66, NA, NA)
  names(l) <- c('mind', 'maxd', 't1', 't2')
  expect_equal(t$limit, l)
  # test if init name
  expect_equal(unname(t$typo['name']), 'back12')
  # test class
  expect_s3_class(t, 'tank', exact = FALSE)
  
  t <- tank(vol = 15, press = 100, typ = 'relay', 
            rules = list(rules = c('return' = 50, 'end' = 25), sys = 'bar'))
  expect_equal(unname(t$carac[c('rule1','rule2')]), c(50, 25))
  expect_equal(unname(t$typo['name']), 'relay15')
  expect_equal(unname(t$typo[c('rule1','rule2')]), c("return", "end"))
})


#### Test expand function ####

test_that("err_expand", {
  # definition of common tanks and dive parameters
  t1 <- tank(vol = 10, press = 200)
  t2 <- tank(vol = 12, press = 200)
  dive <- dive(20, 40)

  # Errors
  err <- 'tank must be a single tank object or a list of tanks'
  expect_error(expand(list("A", "B"), dive), err )
  expect_error(expand(list(t1, "B"), dive), err )

  err <- 'dive must to be a dive object'
  expect_error(expand(list(t1, t2), list("A", "B")), err )
})

test_that("expand_output", {
  # definition of common tanks and dive parameters
  t1 <- tank(vol = 10, press = 200)
  t2 <- tank(vol = 12, press = 200)
  dive <- dive(20, 40)

  exp <- structure(list(
    min_depth = 0, max_depth = 20, begin = 0, end = 45.2, type = "back",
    press = 100, vol = 10, rul1type = "back", rul1press = 50, 
    rul1vol = 10, rul2type = "back", rul2press = 50, rul2vol = 10
  ), row.names = "mind", class = "data.frame")
  expect_identical(expand(t1, dive), exp)
  
  exp <- structure(
    list(0, 20, 0, 45.2, "back", 100, 10, "back", 100, 12, "back",
         50, 10, "back", 50, 12, "back", 50, 10, "back", 50, 12), 
  .Names = c("min_depth", "max_depth", "begin", "end", "back10", "back10_press",
             "back10_vol", "back12", "back12_press", "back12_vol", rep(NA, 12)),
  row.names = 1L, class = "data.frame")
  expect_identical(expand(list(t1, t2), dive), exp)
  # expect_snapshot_value(expand(list(t1, t2), dive), style = "deparse")
  
  # modify for depth limitation
  t1$limit['mind'] <- 5
  t1$limit['maxd'] <- 15
  exp <- structure(
    list(min_depth = c(0, 5, 15), max_depth = c(5, 15, 20), begin = rep(0, 3), 
         end = rep(45.2, 3), type = rep("back", 3), press = c(0,100, 0), 
         vol = rep(10, 3), rul1type = rep("back", 3), rul1press = c(0, 50, 0), 
         rul1vol = rep(10, 3), rul2type = rep("back", 3), rul2press = c(0, 50, 0),
         rul2vol = rep(10, 3)),
    row.names = c("mind", "mind1", "mind2"), class = "data.frame")
  expect_identical(expand(t1, dive), exp)
  
})


#### Test conso function ####

test_that("err_conso", {
  # definition of common tanks and dive parameters
  t1 <- tank(vol = 10, press = 200)
  t2 <- tank(vol = 12, press = 200)
  dive <- dive(20, 40)
  
  err <- 'dive must to be a dive object'
  expect_error(conso(list("A", "B"), list(t1, t2)), err )
  
  err <- 'tank must be a single tank object or a list of tanks'
  expect_error(conso(dive, list("A", "B")), err )
  expect_error(conso(dive, list(t1, "B")), err )
  
  err <- "cons must be a single positive numeric value."
  expect_error(conso(dive, list(t1, t2), cons = -5), err )
  expect_error(conso(dive, list(t1, t2), cons = '20'), err )
  expect_error(conso(dive, list(t1, t2), cons = c(10, 20)), err )
  
  err <- "failure_label must be a single character string"
  expect_error(conso(dive, list(t1, t2), failure_label = 0), err )
  expect_error(conso(dive, list(t1, t2), 
                     failure_label = c('Death', "not proud")), err )
})


test_that("conso_output", {
  back <- tank(12, 200,
               rules = list(rules = c('retour' = 150, 'reserve' = 100),
                            sys = "bar"))
  back15 <- tank(15, 200,
                 rules = list(rules = c('retour' = 150, 'reserve' = 100),
                              sys = "bar"))
  relay <- tank(12, 200,
                rules = list(rules = c('retour' = 120, 'reserve' = 120),
                             sys = "bar"), typ = 'relay')
  dive <- dive(20, 40)
  
  # deadly dive
  war1 <- 'No tank is available between 40 and 41.7 minutes so you died. Try again !'
  war2 <- 'No tank is available between 41.7 and 44.7 minutes so you died. Try again !'
  war3 <- 'No tank is available between 44.7 and 45.2 minutes so you died. Try again !'
  w <- capture_warnings(c <- conso(dive, back))
  expect_match(w, war1, all = FALSE)
  expect_match(w, war2, all = FALSE)
  expect_match(w, war3, all = FALSE)
  expect_snapshot(c)
  # safe dive
  expect_snapshot(conso(dive, back15))
  # multiple tank dive
  expect_snapshot(conso(dive, list(relay, back)))

  dive <- dive(20, 40, hour = 67)
  # safe dive now
  expect_snapshot(conso(dive, back))
  # safe dive
  expect_snapshot(conso(dive, back15))
  # multiple tank dive
  expect_snapshot(conso(dive, list(relay, back)))
})


