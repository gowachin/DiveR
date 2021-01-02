library(shinyTime)
library(shiny.i18n)
library(lubridate)
source("init.R")

conso_panel <- conditionalPanel(
  condition = "input.type == 'sqr'",
  titlePanel(i18n$t("Consumption")),
  conditionalPanel(
    condition = "input.sec_plot == true",
    selectInput(
      "conso_selec", "Select the dive",
      c(
        # i18n$t("Square") = "sqr" #, # don't work, who knows why ?
        "1" = "first",
        "2" = "second"
      )
    )
  ),
  # bloc caracteristics
  sliderInput(
    inputId = "volume", label = "Volume (littre) :",
    min = 1, max = 18, value = 10, step = 0.5
  ),
  sliderInput(
    inputId = "press", label = i18n$t("Pressure (bar) :"),
    min = 10, max = 300, value = 200, step = 10
  ),
  sliderInput(
    inputId = "cons", label = i18n$t("Consumption (littre/minute) :"),
    min = 10, max = 30, value = 20, step = 1
  ),
  # rule 1
  helpText(paste("A rule is an important pression to watch during a dive")),
  fluidRow(
    column(width = 4, textInput('rule1', 'Rule 1', value = "Mid-pression")),
    column(3, selectInput(
      "conso_selec", "Select the dive",
      c("bar" = "bar", "%" = "percent")
    ))),
  sliderInput( inputId = "rule1_press", label = i18n$t("Pressure (bar) :"),
               min = 0, max = 200, value = 100, step = 10 ),
  
  fluidRow(
    column(width = 4, textInput('rule2', 'Rule 2', value = "Reserve")),
    column(3, selectInput(
      "conso_selec", "Select the dive",
      c("bar" = "bar", "%" = "percent")
    ))),
  sliderInput( inputId = "rule2_press", label = i18n$t("Pressure (bar) :"),
      min = 0, max = 200, value = 50, step = 10 )
)