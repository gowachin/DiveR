# Load packages
library(shiny)
library(mn90)
library(shiny.i18n)
library(rhandsontable)
library(shinyTime)
library(shinyWidgets)

i18n <- Translator$new(translation_json_path = "translations/translation.json")
i18n$set_translation_language("en")

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
    tabsetPanel(
      type = "pills",
      #### Dive profile panel ####
      tabPanel(
        "Controls",
        sidebarLayout(
          position = "right",
          ### Sidebar panel for inputs ####
          sidebarPanel(
            id = "sidebar",
            helpText(paste("You can input a dive with a depth and time or",
                           "input multiple time/depths points")),
            ### Profile type ####
            selectInput("type", "Profile type:",
                        c("Square" = "sqr",
                          "Profile" = "pro")),
            ## Square profile ####
            conditionalPanel(
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
              sliderInput(
                inputId = "depth2", label = i18n$t("Depth (meter):"),
                min = 6, max = 65, value = 20
              ),
              # Input: Slider for time ----
              sliderInput(
                inputId = "time2", label = i18n$t("Time (minutes):"),
                min = 1, max = 180, value = 40
              )
            )
            ),
            ## Point profile ####
            conditionalPanel(
              condition = "input.type == 'pro'",
              titlePanel(i18n$t("First dive")),
              # Input : Number of points ----
              numericInput("rows", "Number of rows:", 10, min = 1),
              # Input : ways ----
              materialSwitch(inputId = "way", label = "Round trip", 
                             status = "warning"),
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
            conditionalPanel( condition = "input.advset",
              switchInput(
                inputId = "secu", label = i18n$t("Security stop"), value = TRUE,
                onStatus = "success", offStatus = "danger"
              ),
              # checkboxInput("secu", i18n$t("Security stop"), TRUE),
              numericInput("vup", "Ascent speed:", 10, min = 1),
              timeInput("time_input1", "Enter time", 
                        value = strptime("00:00:00", "%T"), seconds = FALSE)
            )
          ),
          ######################################################################
          # Main panel for displaying outputs ----
          mainPanel(
            # h2("This is a dive with a square profile and stops 
            # given from tables (MN90 tables from the FFESSM."),
            
            conditionalPanel(
                condition = "input.type == 'sqr'",
            textOutput("dive1"),
            plotOutput(outputId = "divePlot"),

            conditionalPanel(
              condition = "input.sec == true",

              textOutput("dive2")
            )
            ),
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
    updateSliderInput(session, "time1", value = 1,
                      min = 0, max = maxt1)
  })
  
  #### Square output ####
  output$divePlot <- renderPlot({
    dive <- dive(depth = input$depth1, time = input$time1, 
                 secu = input$secu, vup = input$vup)
    plot(dive)
  })

  output$dive1 <- renderText({
    paste("You have selected", input$depth1, 'for', input$time1)
  })

  output$dive2 <- renderText({
    paste("You have selected", input$depth2, 'for', input$time2)
  })
  
  #### Profile input ####
  values <- reactiveValues()
  #### Handsontable ####
  observe({
      if (!is.null(input$hot)) {
        DF = hot_to_r(input$hot)
        DF <- DF[1:input$rows,]
        rownames(DF) <- 1:input$rows
      } else {
        if (is.null(values[["DF"]])){
          DF <- data.frame('depth'= rep(10, input$rows),
                           'time'= rep(10, input$rows),
                           'distance'= rep(10, input$rows),
                           'name'= rep(letters, 10)[1:input$rows], 
                           stringsAsFactors = F)
        } else {
          DF <- values[["DF"]]
          DF <- DF[1:input$rows,]
          rownames(DF) <- 1:input$rows
        }
      }
      values[["DF"]] <- DF
    })
  #### Profile output ####
  output$hot <- renderRHandsontable({
    DF <- values[["DF"]]
    if (!is.null(DF))
      rhandsontable(DF, useTypes = TRUE, stretchH = "all") %>%
      hot_cell(1, 1, "Test comment")# %>%
      # hot_validate_numeric(cols = 1, min = -50, max = 50)
  })
  ## Save
  observeEvent(input$save, {
    finalDF <- isolate(values[["DF"]])
    saveRDS(finalDF, file=file.path(outdir, sprintf("%s.rds", outfilename)))
  })
  
}

# Create Shiny app ----
if (interactive()) {
  shinyApp(ui = ui, server = server)
}
