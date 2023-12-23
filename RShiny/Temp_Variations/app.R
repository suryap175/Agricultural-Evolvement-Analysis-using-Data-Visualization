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
data <- read.csv('temp_top_variations_countries.csv', header=T, na.strings = "")
data

# Define the UI
ui <- fluidPage(
  leafletOutput("map")
)

# Define the server
server <- function(input, output) {
  
  # Create the leaflet map
  output$map <- renderLeaflet({
    
    # Set the center and zoom level of the map
    leaflet() %>%
      setView(0, 20, zoom = 2) %>%
      
      # Add the tile layer
      addTiles() %>%
      
      # Add the choropleth layer
      addProviderTiles("CartoDB.Positron") %>%
      addChoropleth(
        data = data,
        valueProperty = "total_temp_changes",
        fillColor = colorQuantile("YlOrRd", data$total_temp_changes)(data$total_temp_changes),
        highlight = highlightOptions(
          weight = 2,
          color = "#666",
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        label = labels,
        legend = legend
      )
  })
}

# Run the app
shinyApp(ui, server)
