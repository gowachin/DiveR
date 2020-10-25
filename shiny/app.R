# Load packages
library(shiny)
# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("MN90"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
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
      checkboxInput("secu1", "Security stop", TRUE) #,
      # titlePanel("Second dive"),
      # sliderInput(inputId = "int",
      #             label = "intervalles (minutes):",
      #             min = 6,
      #             max = 720,
      #             value = 15),
      # # Input: Slider for the number of bins ----
      # sliderInput(inputId = "depth2",
      #             label = "Depth (meter):",
      #             min = 6,
      #             max = 65,
      #             value = 20),
      # 
      # # Input: Slider for the number of bins ----
      # sliderInput(inputId = "time2",
      #             label = "Time (minutes):",
      #             min = 1,
      #             max = 180,
      #             value = 40)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "divePlot")
      
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  output$divePlot <- renderPlot({
    dive <- setdive(depth = input$depth1, time = input$time1, secu = input$secu1, vup = 10)
    plot(dive)
  })
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
