library(shiny)
library(plotly)

# Load the data
df_emiss <- read.csv('C:/Users/surps/OneDrive/Desktop/MS-DAE/Spring 23/Com Viz/Homework and Assignments/Project 1/Violin_Bar/emiss.csv', header=T, na.strings = "")
df_emiss
df_emiss$total_emissions <- as.numeric(df_emiss$total_emissions)
str(df_emiss)

library(shiny)
library(dplyr)
library(plotly)

ui <- fluidPage(
  titlePanel("Total Emissions Histogram"),
  sidebarLayout(
    sidebarPanel(),
    mainPanel(plotlyOutput("emission_hist"))
  )
)

server <- function(input, output) {
  df_emiss <- read.csv('C:/Users/surps/OneDrive/Desktop/MS-DAE/Spring 23/Com Viz/Homework and Assignments/Project 1/Violin_Bar/emiss.csv', header=T, na.strings = "")
  df_emiss
  
  
  hist(    
    data_frame=df_emiss,
    x='farm',
    marginal='violin')
  
  fig.update_layout(
    width=750,
    height=500,
    bargap=0.2,
    
    title='Distribution of elements for Total Emissions',
    title_x = 0.5,
    title_y = 0.95,
    title_xanchor='center',
    title_yanchor='top',
    
    xaxis = dict(title='Total Emissions (kgCO₂ per kg of product)'),
    yaxis = dict(title='Frequencies'))
  
  output$emission_hist <- renderPlotly(fig)
}

shinyApp(ui, server)

