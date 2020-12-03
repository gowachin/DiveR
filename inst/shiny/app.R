# Load packages
library(shiny)
library(mn90)
# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("MN90"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(position = "right",
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      titlePanel("First dive"),
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "depth1",
                  label = "Depth (meter):",
                  min = 6,
                  max = 65,
                  value = 20),
      
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "time1",
                  label = "Time (minutes):",
                  min = 1,
                  max = 180,
                  value = 40),
      checkboxInput("secu1", "Security stop", TRUE),
      
      checkboxInput("sec", "Second Dive"),
      
      conditionalPanel(
        condition = "input.sec == true",
        
        sliderInput(inputId = "depth2",
                    label = "Depth (meter):",
                    min = 6,
                    max = 65,
                    value = 20),
        
        sliderInput(inputId = "time2",
                    label = "Time (minutes):",
                    min = 1,
                    max = 180,
                    value = 40)
      )
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      h2('This is a dive with a square profile and stops given from tables (MN90 tables from the FFESSM.'),
      
      textOutput("dive1"),
      # Output: Histogram ----
      plotOutput(outputId = "divePlot"),
      
      conditionalPanel(
        condition = "input.sec == true",
        
        textOutput("dive2")
      )
      
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output, session) {
  observe({
    maxt1 <- max_depth_t(input$depth1)
    updateSliderInput(session, "time1", value = 1,
                      min = 0, max = maxt1)
  })
  
  output$divePlot <- renderPlot({
    dive <- dive(depth = input$depth1, time = input$time1, secu = input$secu1, vup = 10)
    plot(dive)
  })
  
  output$dive1 <- renderText({ 
    paste("You have selected", input$depth1, 'for', input$time1)
  })
  
  output$dive2 <- renderText({ 
    paste("You have selected", input$depth2, 'for', input$time2)
  })
}

# Create Shiny app ----
if (interactive()) {
  shinyApp(ui = ui, server = server)
}
