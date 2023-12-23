library(ggplot2)
library(dplyr)
library(tidyr)
library(shiny)
library(rsconnect)


df_student <- read.csv("student_health.csv")


# UI
ui <- fluidPage(
  
  titlePanel("Effect of Student Mental Health on CGPA"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("year", "Year", 1, 4, value = c(1, 4),
                  sep = ""),
      selectizeInput(inputId = "gender",
                     label = "Gender",
                     choices = c("All",unique(df_student[, "Gender"]))),
      selectizeInput(inputId = "major",
                     label = "Major",
                     choices = c("All",unique(df_student[, "Major"]))),
      selectizeInput(inputId = "married",
                     label = "Marital Status",
                     choices = c("All",unique(df_student[, "Married"])))
    ),
    
    mainPanel(
      
      plotOutput("deprPlot"),
      plotOutput("anxPlot"),
      plotOutput("panicPlot"),
      plotOutput("TxPlot"),
      
      tabPanel("num_students", "Students selected based on given inputs:", 
               textOutput("n_students")
      )
      
    )
  )
)



# SERVER
server <- function(input, output) {
  
  mental_health <- reactive({
    df_student %>%
      filter(
        if (input$married != "All"){
          Married==input$married} else {
            TRUE},
        
        if (input$major != "All"){
          Major==input$major} else {
            TRUE},
        
        if (input$gender != "All"){
          Gender==input$gender} else {
            TRUE},
        Year >= as.numeric(input$year[1]),
        Year <= as.numeric(input$year[2])
      ) 
  })
  
  output$n_students <- renderText({ 
    nrow(mental_health()) 
  })
  
  output$deprPlot <- renderPlot({
    ggplot(data = mental_health()) + 
      geom_bar(aes(x = CGPA, y = (stat(count)/sum(stat(count)))*100, fill = Depression)) + 
      labs(x = "CGPA", y = "Percentage", title = "Depression vs CGPA") +
      coord_flip()
  })
  
  output$anxPlot <- renderPlot({
    ggplot(data = mental_health()) + 
      geom_bar(aes(x = CGPA, y = (stat(count)/sum(stat(count)))*100, fill = Anxiety)) + 
      labs(x = "CGPA", y = "Percentage", title = "Anxiety vs CGPA") +
      coord_flip()
  })  
  
  output$panicPlot <- renderPlot({
    ggplot(data = mental_health()) + 
      geom_bar(aes(x = CGPA, y = (stat(count)/sum(stat(count)))*100, fill = Panic_Attack)) + 
      labs(x = "CGPA", y = "Percentage", title = "Panic Attacks vs CGPA") +
      coord_flip()
  })  
  
  output$TxPlot <- renderPlot({
    ggplot(data = mental_health()) + 
      geom_bar(aes(x = CGPA, y = (stat(count)/sum(stat(count)))*100, fill = Treatment)) + 
      labs(x = "CGPA", y = "Percentage", title = "Treatment vs CGPA") +
      coord_flip()
  })
  
}  


shinyApp(ui, server)

