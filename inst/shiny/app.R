# Load packages
library(shiny)
library(mn90)
library(shiny.i18n)
library(rhandsontable)
library(shinyTime)
library(lubridate)
library(pacman)
p_load_gh('burgerga/shinyTime@fix/gh8')
library(shinyWidgets)
# traduction settings (must be paste in sourced files!)
language = 'fr' # 'en' or 'fr' language available


fileConn <- file("init.R")
writeLines(c(paste('i18n <- Translator$new(translation_json_path =',
                   '"translations/translation.json")'),
             paste0('i18n$set_translation_language("',language,'")')), 
           fileConn)
close(fileConn)

source("init.R")
source('square.R')
source('functions.R')

# misc
DF <- data.frame() # initialise empty df for profile input

ui <- # navbarPage("My app",
  # put css here
  # tabPanel("truc",
  fluidPage(
    class = "R1",
    #### CSS ####
    # tags$head(tags$link(
    #   rel = "stylesheet",
    #   type = "text/css",
    #   href = "bootstrap.css"
    # )),
    ####
    # checkboxInput("help", i18n$t("hepl"), FALSE),
    tabsetPanel(
      type = "pills",
      #### Dive profile panel ####
      tabPanel(
        i18n$t("Dive Profile"),
        sidebarLayout(
          position = "right",
          ### Sidebar panel for inputs ####
          sidebarPanel(
            id = "sidebar",
            helpText(paste(
              i18n$t("You can input a dive with a depth and time or"),
              i18n$t("input multiple time/depths points")
            )),
            ### Profile type ####
            selectInput(
              "type", i18n$t("Profile type:"),
              c(
                # i18n$t("Square") = "sqr" #, # don't work, who knows why ?
                "Square" = "sqr" #,
                # "Profile" = "pro"
              )
            ),
            ## Square profile ####
            square_panel, # sourced in square.R
            # return following inputs : depth1 time1 sec depth2 time2
            ## Point profile ####
            conditionalPanel(
              condition = "input.type == 'pro'",
              titlePanel( i18n$t("First dive")),
              # Input : Number of points ----
              numericInput("rows", "Number of rows:", 10, min = 1),
              # Input : ways ----
              materialSwitch(
                inputId = "way", label = "Round trip",
                status = "warning"
              ),
              # Input : Points table ----
              rHandsontableOutput("hot"),
              # Input : Save ----
              wellPanel(
                h3("Save"),
                actionButton("save", "Save table")
              )
            ),
            ## Adv settings ####
            checkboxInput("advset", i18n$t("Advanced settings"), FALSE),
            conditionalPanel(
              condition = "input.advset",
              switchInput(
                inputId = "secu", label = i18n$t("Security stop"), value = TRUE,
                onStatus = "success", offStatus = "danger"
              ),
              # checkboxInput("secu", i18n$t("Security stop"), TRUE),
              numericInput("vup", i18n$t("Ascent speed:"), 10, min = 1),
              timeInput("time_input1", i18n$t("Immersion time"),
                value = strptime("00:00:00", "%T"), seconds = FALSE
              )
            )
          ),
          ######################################################################
          # Main panel for displaying outputs ----
          mainPanel(
            # h2("This is a dive with a square profile and stops
            # given from tables (MN90 tables from the FFESSM."),
            ## Square profile ####
            conditionalPanel(
              condition = "input.type == 'sqr'",
              plotOutput(outputId = "divePlot"),
              # textOutput("dive1"),
              verbatimTextOutput("dive"),
              conditionalPanel(
                condition = "input.sec == true",

                textOutput("dive2")
              )
            ),
            ## Point profile ####
            conditionalPanel(
              condition = "input.type == 'pro'",
              textOutput("divep")
              # plotOutput(outputId = "divePlot"),
            )
          )
        )
      ),
      tabPanel(
        "Consommation",
        "this is a test, i repete, it's a test"
      )
    )
  )
# ) ,
# tabPanel("Machin"))

# Define server logic required to draw a histogram ----
server <- function(input, output, session) {
  # max range of depth1
  observe({
    maxt1 <- max_depth_t(input$depth1)
    updateSliderInput(session, "time1",
      value = 1,
      min = 0, max = maxt1
    )
  })

  #### Square output ####
  observe({
    # if (input$type == 'sqr'){}
    if (!input$sec){
      dive1 <- dive(
        depth = input$depth1, time = input$time1,
        secu = input$secu, vup = input$vup
      )
      
      output$divePlot <- renderPlot({
        plot(dive1)
      })
      output$dive <- summarise_dive(dive1)
    } else {
      mult_dive <- ndive(dive(depth = input$depth1, time = input$time1,
                              secu = input$secu, vup = input$vup),
                         dive(depth = input$depth2, time = input$time2,
                              secu = input$secu, vup = input$vup),
                         inter = minute(input$interv) + 60 * hour(input$interv))

      output$divePlot <- renderPlot({
        plot(mult_dive)
      })
      output$dive <- summarise_dive(mult_dive$dive1)
      
      output$dive2 <- renderText({
        paste0('NOT YET IMPLEMENTED')
      })
    }
  })

  

  #### Profile input ####
  values <- reactiveValues()
  #### Handsontable ####
  observe({
    if (!is.null(input$hot)) {
      DF <- hot_to_r(input$hot)
      DF <- DF[1:input$rows, ]
      rownames(DF) <- 1:input$rows
    } else {
      if (is.null(values[["DF"]])) {
        DF <- data.frame(
          "depth" = rep(10, input$rows),
          "time" = rep(10, input$rows),
          "distance" = rep(10, input$rows),
          "name" = rep(letters, 10)[1:input$rows],
          stringsAsFactors = F
        )
      } else {
        DF <- values[["DF"]]
        DF <- DF[1:input$rows, ]
        rownames(DF) <- 1:input$rows
      }
    }
    values[["DF"]] <- DF
  })
  #### Profile output ####
  output$hot <- renderRHandsontable({
    DF <- values[["DF"]]
    if (!is.null(DF)) {
      rhandsontable(DF, useTypes = TRUE, stretchH = "all") %>%
        hot_cell(1, 1, "Test comment")
    } # %>%
    # hot_validate_numeric(cols = 1, min = -50, max = 50)
  })
  ## Save
  observeEvent(input$save, {
    finalDF <- isolate(values[["DF"]])
    saveRDS(finalDF, file = file.path(outdir, sprintf("%s.rds", outfilename)))
  })
}

# Create Shiny app ----
if (interactive()) {
  shinyApp(ui = ui, server = server)
}
