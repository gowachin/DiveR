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

