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
source('conso.R')
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
              i18n$t("You can input a dive with a depth and time") # ,
              # i18n$t("or input multiple time/depths points")
            )),
            ### Profile type ####
            conditionalPanel(
              condition = "input.type == 'pro'",
            selectInput(
              "type", i18n$t("Profile type:"),
              c(
                # i18n$t("Square") = "sqr" #, # don't work, who knows why ?
                "Square" = "sqr" # ,
                # "Profile" = "pro"
              )
            )),
            ## Square profile ####
            square_panel, # sourced in square.R
            ## Point profile ####
            conditionalPanel(
              condition = "input.type == 'pro'",
              titlePanel(i18n$t("First dive")),
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
            # ghost panel for ui inf ####
            conditionalPanel(
              condition = "input.depth1 < 0",
              checkboxInput("ghost_sec", "ghost second dive", FALSE),
              checkboxInput("time_sec", "time second dive", FALSE),
              checkboxInput("sec_plot", "plot second dive", FALSE)
            )
            ### Sidebar panel end ####
          ),
          ######################################################################
          # Main panel for displaying outputs ----
          mainPanel(
            ## Square profile ####
            conditionalPanel(
              condition = "input.type == 'sqr'",
              plotOutput(outputId = "divePlot"),
              verbatimTextOutput("dive"),
              conditionalPanel(
                condition = "input.sec == true",
                verbatimTextOutput("dive2")
              )
            ),
            ## Point profile ####
            conditionalPanel(
              condition = "input.type == 'pro'",
              textOutput("divep")
              # plotOutput(outputId = "divePlot"),
            )
            # End of main panel ----
          )
        )
        #### Dive Profile panel end ####
      ),
      # tabPanel(
      # "Security curve & Tables",
      # "this is a test, i repete, it's a test"
      # ),
      tabPanel(
        "Consommation",
        sidebarLayout(
          position = "right",
          ### Sidebar panel for inputs ####
          sidebarPanel(
            conso_panel,
            # advanced pression settings ####
            # checkboxInput("advset_press", i18n$t("Advanced settings"), FALSE),
            conditionalPanel(
              condition = "input.advset_press",
              # checkboxInput("secu", i18n$t("Security stop"), TRUE),
              selectInput(
                "conso_selec", "Select the dive",
                c(
                  # i18n$t("Square") = "sqr" #, # don't work, who knows why ?
                  "bar" = "bar",
                  "psi" = "psi"
                )
              )
            )
          ),
          ######################################################################
          # Main panel for displaying outputs ----
          mainPanel(
            # h2("This is a dive with a square profile and stops
            # given from tables (MN90 tables from the FFESSM."),
            plotOutput(outputId = "plot_conso")
            # # end of main panel ----
          )
        )
        #### Consommation panel end
      )
    )
  )
# ) ,
# tabPanel("Machin"))

# Define server logic required to draw a histogram ----
server <- function(input, output, session) {
  #### Square intput ####
  observe({
    cat("\n", input$conso_selec, "\n")
    # compute the interval in minuyte ----
    interv <- minute(input$interv) +
      60 * hour(input$interv)
    maxt1 <- max_depth_t(input$depth1)
    tmp <- input$time1
    ################ SLIDER T1 UPDATE ################
    if (input$time1 > maxt1) {
      updateSliderInput(session, "time1", value = maxt1, min = 1, max = maxt1)
      return()
    } else {
      updateSliderInput(session, "time1", value = tmp, min = 1, max = maxt1)
    }

    ################ Compute dive 1 ################
    dive1 <- dive(
      depth = input$depth1, time = input$time1,
      secu = input$secu, vup = input$vup # ,
      # hour = minute(input$time_input1) + 60 * hour(input$time_input1)
    )

    # allow for second dive depending interval and depth
    updateCheckboxInput(session, "ghost_sec", "ghost second dive",
      value = !(input$depth1 > 60 & interv < 720)
    )
    cat("\n-----------------------\n\n") # consol debug help
    ################ SLIDER T2 conditions ################
    timet <- input$time2
    if (interv <= 15) {
      cat("\nconsec")
      spendt <- max(dive1$dtcurve$times) + interv # already spent time
      maxt2 <- max_depth_t(max(input$depth1, input$depth2)) - spendt
      maxt2 <- floor(maxt2)
    } else if (interv > 720) {
      cat("\ndiff")
      maxt2 <- max_depth_t(input$depth2)
    } else {
      cat("\nsuccess")
      cat("\nghost check ", input$ghost_sec)
      if (input$depth1 > 60 & interv < 720) {
        cat(" impossible")
        maxt2 <- -1
      } else {
        maj <- majoration(
          depth = input$depth2, inter = interv,
          group = dive1$palier$group
        )
        cat(" maj : ", maj)
        maxt2 <- max_depth_t(input$depth2) - maj
      }
    }
    tmp <- input$time2

    updateCheckboxInput(session, "time_sec", "ghost second dive",
      value = (maxt2 > 0)
    )
    updateCheckboxInput(session, "sec_plot", "ghost second dive",
                        value = input$sec & input$ghost_sec & input$time_sec
    )

    cat("\nmaj done\n") # consol debug help
    cat("inter ", interv, "time2 ", input$time2) # consol debug help
    cat("\ndepth2 ", input$depth2, "group ", dive1$palier$group)
    cat("\ntimet ", timet, "maxt2 ", maxt2, "\n")
    cat(input$ghost_sec, "ghost check\n")
    cat(input$sec, "sec\n")
    cat(input$time_sec, "time check\n")
    cat(input$sec_plot, "sec plot")
    ################ SLIDER T2 UPDATE ################
    if (timet > maxt2 | !input$time_sec) {
      cat("\n\n update slider")
      updateSliderInput(session, "time2", value = maxt2, min = 1, max = maxt2)
      if (input$depth1 > 60) {
        output$dive2 <- renderText({
          i18n$t("A second dive is not possible in less than 12h")
        })
      } else {
        output$dive2 <- renderText({
          i18n$t("A second dive is not possible at this depth")
        })
      }
      # if (input$ghost_sec | input$time_sec) return()
      if (input$time_sec) {
        return()
      }
    } else {
      updateSliderInput(session, "time2", value = tmp, min = 1, max = maxt2)
    }
    ################ Plot and compute dives ################
    # if (input$type == 'sqr'){}
    cat("\n\n    dives compute")
    if (!input$sec_plot ) {
      cat("    single")
      # Plot the dive
      output$divePlot <- renderPlot({
        plot(dive1, ylab = i18n$t("Depth (m)"), xlab = i18n$t("Time (min)"))
      })
      # Dive summary
      output$dive <- summarise_dive(dive1)
    } else {
      cat("    multiples")
      # compute the dive
      mult_dive <- ndive(dive1,
        dive(
          depth = input$depth2, time = input$time2,
          secu = input$secu, vup = input$vup
        ),
        inter = interv
      )
      # Plot the dive
      output$divePlot <- renderPlot({
        plot(mult_dive, ylab = i18n$t("Depth (m)"), xlab = i18n$t("Time (min)"))
      })
      # Dive summary
      output$dive <- summarise_dive(mult_dive$dive1)
      if (mult_dive$type != "solo") {
        output$dive2 <- summarise_dive(mult_dive$dive2)
      }
    }
    cat("\n    end")
    
    ################ Consumption ################
    if (input$sec_plot){ # select the dive
      conso_dive <- switch(input$conso_selec,
                           first = mult_dive$dive1,
                           second = mult_dive$dive2)
    } else {
      conso_dive <- dive1
    }
    # comput consumption
    dt_conso <- conso(dive = conso_dive, bloc = bloc(input$volume, input$press), 
                      cons = input$cons, 
                      mid = input$rule1_press, reserve = input$rule2_press)
    # plot consuption
    output$plot_conso <- renderPlot({
      plot(conso_dive, ylab = i18n$t("Depth (m)"), xlab = i18n$t("Time (min)"))
      lines(dt_conso$dtcurve$time, -dt_conso$vpress/ 12, col = 'blue')
      abline(v= dt_conso$time_mid[2], col = "orange")
      abline(v= dt_conso$time_reserve[2], col = "red")
    })
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
