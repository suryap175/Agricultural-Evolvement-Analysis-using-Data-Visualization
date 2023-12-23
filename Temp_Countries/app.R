#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Load the required packages
library(shiny)
library(leaflet)

# Read the CSV file
#data <-read.csv('C:/Users/surps/OneDrive/Desktop/MS-DAE/Spring 23/Com Viz/Homework and Assignments/Project 1/Temp_Countries/temp_top_variations_countries.csv', header=T, na.strings = "")
data <- read.csv('temp_top_variations_countries.csv', header=T, na.strings = "")
data

# Define the UI
# Define UI
ui <- fluidPage(
  titlePanel("Interactive Scatterplot"),
  sidebarLayout(
    sidebarPanel(
      h4("Select x and y variables:"),
      selectInput("x_var", "X-axis variable:", choices = c("country_name", "total_temp_changes")),
      selectInput("y_var", "Y-axis variable:", choices = c("country_name", "total_temp_changes"))
    ),
    mainPanel(
      plotOutput("scatterplot")
    )
  )
)

# Define server
server <- function(input, output) {
  output$scatterplot <- renderPlot({
    plot(data[[input$x_var]], data[[input$y_var]], 
         xlab = input$x_var, ylab = input$y_var,
         main = "Scatterplot")
  })
}

# Run the application
shinyApp(ui = ui, server = server)
