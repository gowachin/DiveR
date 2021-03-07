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

## ----tmp_attribute_tank, include = FALSE--------------------------------------

summary_conso <- function(object, ...){
  # parameters volume and pressure
  cat(paste("Tank :",volume(object), "litre at",pressure(object),"bar\n"))
  # dtr
  dtr <- dtr(object)
  cat('Total dive time is',dtime(object)+dtr,'with a dive ascent of',dtr,'minutes\n')
  # palier & maj
}


## ----ex_dive------------------------------------------------------------------
simp_dive <- dive(depth = 20, time = 40, secu = TRUE)
summary(simp_dive)

## ----death_conso, dev='png', out.width="100%"---------------------------------
Tank_12L <- tank(vol = 12, press = 200)
death <- conso(dive = simp_dive, tank = Tank_12L, 
               cons = 20, failure_label = 'Air failure')
plot(death)

## ----viable_conso, dev='png', out.width="100%"--------------------------------
Tank_15L <- tank(vol = 15, press = 200)
viable <- conso(dive = simp_dive, tank = Tank_15L, 
               cons = 20, failure_label = 'Air failure')
plot(viable)

## ----pressure_conso-----------------------------------------------------------
pressure(death)

## ----rules_conso--------------------------------------------------------------
rules(death)
rules(viable)

## ----mult_tank----------------------------------------------------------------
A_10L <- tank(vol = 10, press = 200, name = 'A_10L')
B_10L <- tank(vol = 10, press = 200, name = 'A_10L')
bi_conso <- conso(dive = simp_dive, tank = list(A_10L, B_10L), 
               cons = 20, failure_label = 'Air failure')
plot(bi_conso)

