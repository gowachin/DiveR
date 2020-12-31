library(shinyTime)
library(shiny.i18n)
source("init.R")

square_panel <- conditionalPanel(
  condition = "input.type == 'sqr'",
  titlePanel(i18n$t("First dive")),
  # Input: Slider for depths ----
  sliderInput(
    inputId = "depth1", label = i18n$t("Depth (meter):"),
    min = 6, max = 65, value = 20
  ),
  # Input: Slider for time ----
  sliderInput(
    inputId = "time1", label = i18n$t("Time (minutes):"),
    min = 1, max = 180, value = 40
  ),
  # Input: checkbox second dive ----
  checkboxInput("sec", i18n$t("Second Dive")),
  # Second dive ####
  conditionalPanel(
    condition = "input.sec == true",
    # Input: Slider for depths ----
    timeInput("interv", i18n$t("Interval time"),
              value = strptime("00:15:00", "%T"), seconds = FALSE
    ),
    sliderInput(
      inputId = "depth2", label = i18n$t("Depth (meter):"),
      min = 6, max = 60, value = 20
    ),
    # Input: Slider for time ----
    sliderInput(
      inputId = "time2", label = i18n$t("Time (minutes):"),
      min = 1, max = 180, value = 40
    )
  )
)

