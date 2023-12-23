# Load required packages
library(shiny)
library(dplyr)
library(plotly)

# Load data
top_8_emiss <- read.csv('sort.csv', header=T, na.strings = "")
top_8_emiss


# Define UI
ui <- fluidPage(
  titlePanel("Top 8 Food Items for Total Emissions of CO₂"),
  
  mainPanel(
    plotlyOutput("emissions_plot")
  )
)

# Define server
server <- function(input, output) {
  output$emissions_plot <- renderPlotly({
    top_8_emiss
    
    
    fig <- plot_ly(
      top_8_emiss,
      x = ~food_product,
      y = ~total_emissions,
      marker = list(color = c("#053623", "#208067", "#70bb7a", "#Ba7700", "#E8b53c", "#Ffe993", "#Ffffbf", "#Efe6cf")),
      type = 'bar',
      hovertemplate = paste(
        "<b>Food Product:</b> %{x}<br>",
        "<b>Total Emissions:</b> %{y:.2f} kgCO\u2082/kg<br>"
      )
    ) %>%
      layout(
        xaxis = list(title = "Food Product", tickangle = 45),
        yaxis = list(title = "Emissions (kgCO₂ per kg of product)"),
        title = list(text = "Top 8 Food Items for Total Emissions of CO₂",
                     x = 0.5, y = 0.95, xanchor = "center", yanchor = "top")
      )
    
    fig
  })
}

# Run the app
shinyApp(ui, server)
