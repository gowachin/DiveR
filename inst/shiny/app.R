# Load packages
library(shiny)
library(mn90)
library(shiny.i18n)
library(rhandsontable)
library(shinyTime)
library(lubridate)
# library(pacman)
# p_load_gh('burgerga/shinyTime@fix/gh8')
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
              i18n$t("You can input a dive with a depth and time")#,
              # i18n$t("or input multiple time/depths points")
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
            # return following inputs : depth1 time1 sec interv depth2 time2
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
            ),
            # ghost panel for ui inf
            conditionalPanel(
              condition = "input.depth1 < 0",
              checkboxInput("ghost_sec", 'ghost second dive', FALSE),
              checkboxInput("time_sec", 'time second dive', FALSE)
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
              textOutput("dive2")
            ),
            ## Point profile ####
            conditionalPanel(
              condition = "input.type == 'pro'",
              textOutput("divep")
              # plotOutput(outputId = "divePlot"),
            )
          )
        )
      )#,
      # tabPanel(
      #   "Security curve & Tables",
      #   "this is a test, i repete, it's a test"
      # ),
      # tabPanel(
      #   "Consommation",
      #   "this is a test, i repete, it's a test"
      # )
    )
  )
# ) ,
# tabPanel("Machin"))

# Define server logic required to draw a histogram ----
server <- function(input, output, session) {
  #### Square intput ####
  ################ SLIDER UPDATE ################
  observe({
    interv <- minute(input$interv) +
      60 * hour(input$interv)
    maxt1 <- max_depth_t(input$depth1)
    tmp <- input$time1
    if (input$time1 > maxt1) {
      updateSliderInput(session, "time1", value = maxt1, min = 0, max = maxt1)
      return()
    } else {
      updateSliderInput(session, "time1", value = tmp, min = 0, max = maxt1)
      # print('compute dive1')
    }
    dive1 <- dive(
      depth = input$depth1, time = input$time1,
      secu = input$secu, vup = input$vup # ,
      # hour = minute(input$time_input1) + 60 * hour(input$time_input1)
    )
    # allow for second dive depending interval and depth
    updateCheckboxInput(session, 'ghost_sec', 'ghost second dive', 
                        value = ! (input$depth1 > 60 & interv < 720))
    # max range of depth2
    timet <- input$time2
    if (interv <= 15) {
      spendt <- max(dive1$dtcurve$times) + interv # already spent time
      maxt2 <- max_depth_t(max(input$depth1, input$depth2)) - spendt
    } else if (interv > 720) {
      maxt2 <- max_depth_t(input$depth2)
    } else {
      if (input$ghost_sec > 60){ 
        maxt2 <- -1
      } else {
        maj <- majoration(
          depth = input$depth2, inter = interv,
          group = dive1$palier$group
        )
        maxt2 <- max_depth_t(input$depth2) - maj
      }
    }
    tmp <- input$time2
    cat('\n\nghost check ', input$ghost_sec)
    cat('\nsec', input$sec,'\n')
    cat('inter ',interv, 'time2 ', input$time2)
    cat('\ndepth2 ', input$depth2, 'group ',dive1$palier$group)
    cat('\ntimet ',timet,'maxt2 ', maxt2, '\n')
    updateCheckboxInput(session, 'time_sec', 'ghost second dive', 
                        value = (maxt2 > 0))
    cat('\n time check ', input$time_sec)
    if (timet > maxt2 | !input$time_sec) {
      updateSliderInput(session, "time2", value = maxt2, min = 0, max = maxt2 )
      if(input$sec) return()
    } else {
      updateSliderInput(session, "time2", value = tmp, min = 0, max = maxt2 )
    }
    ################ compute the dives ################
    # if (input$type == 'sqr'){}
    cat('\n dives compute')
    if (!input$sec | !input$ghost_sec | !input$time_sec) {
      cat('\n single dive')
      # Plot the dive
      output$divePlot <- renderPlot({
        plot(dive1, ylab = i18n$t("Depth (m)"), xlab = i18n$t("Time (min)"))
      })
      # Dive summary
      output$dive <- summarise_dive(dive1)
      # Second dive impossible if depth > 60
      if(input$ghost_sec){
        output$dive2 <- renderText({""})
      } else {
        output$dive2 <- renderText({
          paste0( "Second dive impossible in less than 12h ",
                  "after a dive a 60 more meters" )
        })
      }
    } else {
      cat('\n multiples dives')
      # compute the dive
      mult_dive <- ndive(dive1,
                         dive( depth = input$depth2, time = input$time2,
                               secu = input$secu, vup = input$vup ),
                         inter = interv )
      # Plot the dive
      output$divePlot <- renderPlot({
        plot(mult_dive)
      })
      # Dive summary
      output$dive <- summarise_dive(mult_dive$dive1)
      output$dive2 <- renderText({
        paste0("NOT YET IMPLEMENTED dive is ", mult_dive$type)
      })
    }
    cat('\n done')
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
