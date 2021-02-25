## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(DiveR)

## ----basic_tank---------------------------------------------------------------
tank1 <- tank(vol = 12, press = 200)
tank1

## ----limit_tank---------------------------------------------------------------
# set in percentage
tank2 <- tank(vol = 12, press = 200, 
              rules = list(rules = c('mid' = 50, 'quarter' = 25), sys = '%'))
tank2$carac[c('rule1', 'rule2')]
tank2$typo[c('rule1', 'rule2')]
# set in bar
tank3 <- tank(vol = 12, press = 200, 
              rules = list(rules = c('return' = 130, 'end' = 80), sys = 'bar'))
tank3$carac[c('rule1', 'rule2')]
tank3$typo[c('rule1', 'rule2')]

## ----relay_tank---------------------------------------------------------------
relay <- tank(vol = 12, press = 180, typ = 'relay',
              rules = list(rules = c('gap' = 120, 'end_gap' = 120), sys = 'bar'))
relay$carac[c('rule1', 'rule2')]

## ----desat_tank---------------------------------------------------------------
desat <- tank(vol = 12, press = 180, typ = 'relay',
              rules = list(rules = c('gap' = 100, 'end_gap' = 25), sys = '%'))
desat$carac[c('rule1', 'rule2')]

## ----summary.tank, include = FALSE--------------------------------------------

pressure <- function(x){
  x$carac$press
}

volume <- function(x){
  x$carac$vol
}

summary_conso <- function(object, ...){
  # parameters volume and pressure
  cat(paste("Tank :",volume(object), "litre at",pressure(object),"bar\n"))
  # dtr
  dtr <- dtr(object)
  cat('Total dive time is',dtime(object)+dtr,'with a dive ascent of',dtr,'minutes\n')
  # palier & maj
}


## ---- fig.show='hold'---------------------------------------------------------
plot(1:10)
plot(10:1)

## ---- echo=FALSE, results='asis'----------------------------------------------
knitr::kable(head(mtcars, 10))

