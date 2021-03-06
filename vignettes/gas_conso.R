## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.height = 6.1,
  fig.width = 8.5,
  dpi = 96
)

## ----basic_tank---------------------------------------------------------------
library(DiveR)
tank1 <- tank(vol = 12, press = 200)
tank1

## ----limit_tank---------------------------------------------------------------
# set in percentage
tank2 <- tank(vol = 12, press = 200, 
              rules = list(rules = c('mid' = 50, 'quarter' = 25), sys = '%'))
rules(tank2)
# set in bar
tank3 <- tank(vol = 12, press = 200, 
              rules = list(rules = c('return' = 130, 'end' = 80), sys = 'bar'))
rules(tank3)

## ----relay_tank---------------------------------------------------------------
relay <- tank(vol = 12, press = 180, typ = 'relay',
              rules = list(rules = c('gap' = 120, 'end_gap' = 120), sys = 'bar'))

## ----desat_tank---------------------------------------------------------------
desat <- tank(vol = 12, press = 180, typ = 'relay',
              rules = list(rules = c('gap' = 100, 'end_gap' = 25), sys = '%'))
desat$carac[c('rule1', 'rule2')]

## ----attribute_tank-----------------------------------------------------------
pressure(relay)
volume(relay)
rules(relay)
summary(relay)

## ----ex_dive------------------------------------------------------------------
simp_dive <- dive(depth = 20, time = 40, secu = TRUE)
summary(simp_dive)

## ----death_conso, dev='png', out.width="100%"---------------------------------
Tank_12L <- tank(vol = 12, press = 200)
death <- conso(dive = simp_dive, tank = Tank_12L, 
               cons = 20, failure_label = 'Air failure')
plot(death, line_print = FALSE)

## ----viable_conso, dev='png', out.width="100%"--------------------------------
Tank_15L <- tank(vol = 15, press = 200)
viable <- conso(dive = simp_dive, tank = Tank_15L, 
               cons = 20, failure_label = 'Air failure')
plot(viable, line_print = FALSE)

## ----pressure_conso-----------------------------------------------------------
pressure(death)

## ----rules_conso--------------------------------------------------------------
rules(death)
rules(viable)

## ----sum_conso----------------------------------------------------------------
summary(death)
summary(viable)

## ----mult_tank, dev='png', out.width="100%"-----------------------------------
A_10L <- tank(vol = 10, press = 200, name = 'A_10L')
B_10L <- tank(vol = 10, press = 200, name = 'B_10L')
bi_conso <- conso(dive = simp_dive, tank = list(A_10L, B_10L), 
               cons = 20, failure_label = 'Air failure')
plot(bi_conso, line_print = FALSE, def_cols = TRUE)
pressure(bi_conso)

## ----mult_relay, dev='png', out.width="100%"----------------------------------
Relay <- tank(vol = 10, press = 200, typ = 'relay', rules = list(
  rules = c(' ' = 66, 'drop relay' = 66),
  sys = '%'), name = "Relay"
  )

Tank_10L <- tank(vol = 10, press = 200, rules = list(
  rules = c('Return' = 100, "Reserve" = 50), 
  sys = "bar"), name = "Back"
  )

relay_conso <- conso(dive = simp_dive, 
                     tank = list(Relay, Tank_10L), 
                     cons = 20, failure_label = 'Air failure')
plot(relay_conso, line_print = FALSE)
summary(relay_conso)

## ----mult_mrelay, eval = FALSE, include = FALSE-------------------------------
#  Relay1 <- tank(vol = 10, press = 200, typ = 'relay', rules = list(
#    rules = c(' ' = 66, 'drop relay' = 66),
#    sys = '%'), name = "Relay1"
#    )
#  Relay1$limit['maxd'] = 10
#  
#  Relay2 <- tank(vol = 10, press = 200, typ = 'relay', rules = list(
#    rules = c(' ' = 66, 'drop relay' = 66),
#    sys = '%'), name = "Relay2"
#    )
#  
#  Tank_10L <- tank(vol = 10, press = 200, rules = list(
#    rules = c('Return' = 100, "Reserve" = 50),
#    sys = "bar"), name = "Back"
#    )
#  
#  relay_conso <- conso(dive = dive(40, 35),
#                       tank = list(Relay1, Relay2, Tank_10L),
#                       cons = 20, failure_label = 'Air failure')
#  plot(relay_conso)
#  pressure(relay_conso)

## ----mult_deco, eval = FALSE, include = FALSE---------------------------------
#  Deco <- tank(vol = 6, press = 200, typ = 'relay', rules = list(
#    rules = c(' ' = 200, ' ' = 200),
#    sys = '%'), name = "Deco"
#    )
#  
#  Tank_15L <- tank(vol = 15, press = 200, rules = list(
#    rules = c('Return' = 100, "Reserve" = 50),
#    sys = "bar"), name = "Back"
#    )
#  
#  relay_conso <- conso(dive = simp_dive,
#                       tank = list(Deco, Tank_15L),
#                       cons = 20, failure_label = 'Air failure')
#  plot(relay_conso)
#  pressure(relay_conso)

