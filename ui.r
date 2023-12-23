
library(shiny) 
library(shinydashboard)
library(plotly)

# Define the user interface for the dashboard
shinyUI( fluidPage(
  
  # Set the title of the dashboard
  titlePanel(h1("University Comparison Dashboard", align="center")),
  
  # Sidebar panel for selecting universities
  
    flowLayout(
      # Select input for the first university
      selectInput("uni1", label = "Select First University", choices = university_data$Institution.Name,
                  selected = 'Northeastern University'),
      
      # Select input for the second university
      selectInput("uni2", label = "Select Second University", choices = university_data$Institution.Name,
                  selected = 'Massachusetts Institute of Technology'),
      align="center"),
    
    # Main panel for displaying visualizations
  splitLayout(
      
      fluidRow(
        fluidRow(h4("University 1")),
        fluidRow("Acceptance Rate", verbatimTextOutput("acceptance_rate_1")),
        fluidRow("Tuition Fees", verbatimTextOutput("tuition_fee_1")),
        fluidRow("Total Students Enrolled", verbatimTextOutput("enrolled_students_1")),
        fluidRow("Average Federal Financial Aid", verbatimTextOutput("financial_aid_1")),
        fluidRow("Graduation Rate", verbatimTextOutput("graduation_rate_1")),
        align="center"),
      
      fluidRow(
        fluidRow(h4("University 2")),
        fluidRow("Acceptance Rate", verbatimTextOutput("acceptance_rate_2")),
        fluidRow("Tuition Fees", verbatimTextOutput("tuition_fee_2")),
        fluidRow("Total Students Enrolled", verbatimTextOutput("enrolled_students_2")),
        fluidRow("Average Federal Financial Aid", verbatimTextOutput("financial_aid_2")),
        fluidRow("Graduation Rate", verbatimTextOutput("graduation_rate_2")),
        align="center")
),
   
      # Bar chart comparing admissions data
      fluidRow(h2("Admissions Data"),
      splitLayout(plotlyOutput("fig1", height = "400px", width = "80%"),
                  plotlyOutput("fig2", height = "400px", width = "80%")),
      align="center"),

      #SAT score Indicator
      fluidRow(h2("SAT Score"),
         splitLayout(plotlyOutput("SATscore1", height = "400px", width = "80%"),
                     plotlyOutput("SATscore2", height = "400px", width = "80%")),
         align="center"),

      # bar chart comparing fees data
      
      fluidRow(h2("Fees Data"),
      splitLayout(plotOutput("fees_plot1", width = "80%"), align="center",
                  plotOutput("fees_plot2", width = "80%"), align="center"),
      align="center")
       
)
)

