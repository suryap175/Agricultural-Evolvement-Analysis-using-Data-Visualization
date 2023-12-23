
library(shiny)
library(dplyr)
library(plotly)

df_temp <- read.csv('C:/Users/surps/OneDrive/Desktop/MS-DAE/Spring 23/Com Viz/Homework and Assignments/Project 1/Choropleth/temp_variations.csv', header=T, na.strings = "")
df_temp

# Define UI
ui <- fluidPage(
  plotlyOutput("choro_map")
)

# Define server
library(shiny)
library(plotly)

server <- function(input, output){ 
  
  year_variations <- df_temp %>% 
    group_by(country_code, country_name, years) %>% 
    summarise(temp_changes = mean(temp_changes, na.rm = TRUE)) %>% 
    ungroup() %>% 
    filter(!is.na(country_code)) %>% 
    arrange(years, country_name)
  
  output$choro_map <- renderPlotly({
    fig <- plot_ly(year_variations, type = "choropleth", locations = ~country_code, animation_frame = ~years, 
                   color = ~temp_changes, colors = "RdBu", zmin = -2, zmax = 2.5, 
                   hover_name = ~country_name, hover_data = ~country_code) %>% 
      layout(title = "Worldwide Temperature Change by Countries from 1961 to 2019", title_x = 0.5, title_y = 0.95, 
             title_xanchor = "center", title_yanchor = "top", dragmode = FALSE, width = 1000, height = 600, 
             updatemenus = list(list(buttons = list(list(args = list(list(frame = list(duration = 250, redraw = FALSE), 
                                                                          transition = list(duration = 80))), 
                                                         label = "Play", method = "animate", 
                                                         args = list(list(frame = list(duration = 250, redraw = FALSE), 
                                                                          fromcurrent = TRUE, 
                                                                          transition = list(duration = 80), 
                                                                          mode = "immediate"), 
                                                                     list(frame = list(duration = 250, redraw = FALSE), 
                                                                          fromcurrent = TRUE, 
                                                                          transition = list(duration = 80), 
                                                                          mode = "immediate"))), 
                                                    showactive = FALSE, type = "buttons", x = 0.1, xanchor = "right", 
                                                    y = 0, yanchor = "top", direction = "left", pad = list("t" = 80, "r" = 10)))) %>% 
               colorbar(title = "Temperature Change (°C)", len = 0.7, yanchor = "bottom", y = 0.05, tickvals = seq(-2, 2.5, by = 0.5), 
                        ticktext = c("-2.0", "-1.5", "-1.0", "-0.5", "0.0", "0.5", "1.0", "1.5", "2.0", "2.5"), 
                        tickfont = list(color = "black", size = 12), thickness = 20))
    
    fig
  })
}


shinyApp(ui, server)


