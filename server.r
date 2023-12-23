
library(shiny)
library(ggplot2)
library(magrittr)
library(dplyr)
library(tidyr)
library(shinydashboard)
library(plotly)


# Define the server for the dashboard
shinyServer(
  function(input, output) {
  
  # Compare the selected universities
  university_data <- reactive({read.csv("university_database1.csv")
  })
    
  selected_universities <- reactive({
    req(c(input$uni1, input$uni2))
    
    data.frame(
      Institution.Name = c(input$uni1, input$uni2),
      acceptance_rate = university_data()$acceptance_rate[university_data()$Institution.Name %in% c(input$uni1, input$uni2)],
      tuition_fee = university_data()$Published.out.of.state.tuition.and.fees.2021.22..IC2021_AY.[university_data()$Institution.Name %in% c(input$uni1, input$uni2)],
      enrolled_students = university_data()$Enrolled.total..ADM2021.[university_data()$Institution.Name %in% c(input$uni1, input$uni2)],
      financial_aid = university_data()$Average.amount.of.federal.grant.aid.awarded.to.full.time.first.time.undergraduates..SFA2021.[university_data()$Institution.Name %in% c(input$uni1, input$uni2)],
      graduation_rate = university_data()$Graduation.rate..total.cohort..DRVGR2021.[university_data()$Institution.Name %in% c(input$uni1, input$uni2)]
    )
    
  })
  
  # Display the big bold font comparison of acceptance rate, tuition fee, enrolled students, financial aid, and graduation rate
  output$acceptance_rate_1 <- renderText({   
    selected_universities()$acceptance_rate[1]
  })
  output$tuition_fee_1 <- renderText({
    selected_universities()$tuition_fee[1]
  })
  output$enrolled_students_1 <- renderText({
    selected_universities()$enrolled_students[1]
  })
  output$financial_aid_1 <- renderText({
    selected_universities()$financial_aid[1]
  })
  output$graduation_rate_1 <- renderText({
    selected_universities()$graduation_rate[1]
  })
  
  output$acceptance_rate_2 <- renderText({
    selected_universities()$acceptance_rate[2]
  })
  output$tuition_fee_2 <- renderText({
    selected_universities()$tuition_fee[2]
  })
  output$enrolled_students_2 <- renderText({
    selected_universities()$enrolled_students[2]
  })
  output$financial_aid_2 <- renderText({
    selected_universities()$financial_aid[2]
  })
  output$graduation_rate_2 <- renderText({
    selected_universities()$graduation_rate[2]
  })

#plotfunnel
  
    admissions_data2 <- reactive({
    req(input$uni1)
    tmp <- subset(university_data(), university_data()$Institution.Name %in% input$uni1)
    tmp %>%
      dplyr::select(Institution.Name, Applicants.total..ADM2021., Admissions.total..ADM2021., Enrolled.total..ADM2021.) %>% 
      tidyr::pivot_longer(names_to = "admissions_type", values_to = "admissions_value", -Institution.Name)
  })
  
    output$fig1 <- renderPlotly({
    temp <- plot_ly() %>%
      add_trace(type = "funnel",
                x = admissions_data2()$admissions_value,
                y = admissions_data2()$admissions_type,
                textposition = "outside",
                textinfo = "value+percent initial",
                opacity = 0.65,
                marker = list(color = c("deepskyblue", "lightsalmon", "tan"),
                              line = list(width = c(2, 2, 5), color = c("wheat", "wheat", "blue"))),
                connector = list(line = list(color = "royalblue", dash = "dot", width = 3))) %>%
      layout(yaxis = list(categoryarray = c("Number of Applicants", "Number of Admissions", "Number of Enrolled Students")))
  })
  
    admissions_data3 <- reactive({
    req(input$uni2)
    tmp <- subset(university_data(), university_data()$Institution.Name %in% input$uni2)
    tmp %>%
      dplyr::select(Institution.Name, Applicants.total..ADM2021., Admissions.total..ADM2021., Enrolled.total..ADM2021.) %>% 
      tidyr::pivot_longer(names_to = "admissions_type", values_to = "admissions_value", -Institution.Name)
  })
  
    output$fig2 <- renderPlotly({
    temp <- plot_ly() %>%
      add_trace(type = "funnel",
                x = admissions_data3()$admissions_value,
                y = admissions_data3()$admissions_type,
                textposition = "outside",
                textinfo = "value+percent initial",
                opacity = 0.65,
                marker = list(color = c("deepskyblue", "lightsalmon", "tan"),
                              line = list(width = c(2, 2, 5), color = c("wheat", "wheat", "blue"))),
                connector = list(line = list(color = "royalblue", dash = "dot", width = 3))) %>%
      layout(yaxis = list(categoryarray = c("Number of Applicants", "Number of Admissions", "Number of Enrolled Students")))
  })
  
  # Select the relevant columns from the university data
    fees_data1 <- reactive({
    
    req(c(input$uni1))
    
    tmp3 <- university_data() %>%
    filter(Institution.Name %in% c(input$uni1)) %>%
    select(Institution.Name, Published.in.district.tuition.and.fees.2021.22..IC2021_AY., Published.in.state.tuition.and.fees.2021.22..IC2021_AY., Published.out.of.state.tuition.and.fees.2021.22..IC2021_AY., On.campus..room.and.board.2021.22..IC2021_AY., On.campus..other.expenses.2021.22..IC2021_AY., Off.campus..not.with.family...room.and.board.2021.22..IC2021_AY.,Off.campus..not.with.family...other.expenses.2021.22..IC2021_AY.)
   
    tmp4 <- tmp3 %>%
    pivot_longer(names_to = "fee_type", values_to = "fee_amount", -Institution.Name, values_drop_na = TRUE)
  })
  
    fees_data2 <- reactive({
    
    req(c(input$uni2))
    
    tmp5 <- university_data() %>%
      filter(Institution.Name %in% c(input$uni2)) %>%
      select(Institution.Name, Published.in.district.tuition.and.fees.2021.22..IC2021_AY., Published.in.state.tuition.and.fees.2021.22..IC2021_AY., Published.out.of.state.tuition.and.fees.2021.22..IC2021_AY., On.campus..room.and.board.2021.22..IC2021_AY., On.campus..other.expenses.2021.22..IC2021_AY., Off.campus..not.with.family...room.and.board.2021.22..IC2021_AY.,Off.campus..not.with.family...other.expenses.2021.22..IC2021_AY.)
    
    tmp6 <- tmp5 %>%
      pivot_longer(names_to = "fee_type", values_to = "fee_amount", -Institution.Name, values_drop_na = TRUE)
  })
  
    # Create the bar chart
    output$fees_plot1<- renderPlot({ggplot(fees_data1(), aes(x = fee_type, y = fee_amount )) +
    geom_bar(stat = "identity") +
    labs(x = "Fee Type", y = "Fee Amount", fill = "") +
    ggtitle("Fees Data Comparison")
  })
    
    output$fees_plot2<- renderPlot({ggplot(fees_data2(), aes(x = fee_type, y = fee_amount )) +
        geom_bar(stat = "identity") +
        labs(x = "Fee Type", y = "Fee Amount", fill = "") +
        ggtitle("Fees Data Comparison")
    })
    
    #SAT score 
    
    SATscoredata1 <- reactive({
      req(input$uni1)
      tmp <- subset(university_data(), university_data()$Institution.Name %in% input$uni1)
      tmp %>%
        dplyr::select(Institution.Name, Percent.of.first.time.degree.certificate.seeking.students.submitting.SAT.scores..ADM2021.)
    })
    
    output$SATscore1 <- renderPlotly ({
      
      SATscore01<- plot_ly(
      domain = list(x = c(0, 1), y = c(0, 1)),
      value = SATscoredata1()$Percent.of.first.time.degree.certificate.seeking.students.submitting.SAT.scores..ADM2021.,
      title = list(text = "SAT score Submission Percentage"),
      type = "indicator",
      mode = "gauge+number",
      gauge = list(
        axis = list(range = list(NULL, 100)),
        tickvals = seq(0,100, by = 20),
        ticksuffix = "%")
      ) 
      SATscore01 %>%
        layout(margin = list(l=20,r=30),
               width= "70%",
               height= "70%")
    })
    
    SATscoredata2 <- reactive({
      req(input$uni2)
      tmp <- subset(university_data(), university_data()$Institution.Name %in% input$uni2)
      tmp %>%
        dplyr::select(Institution.Name, Percent.of.first.time.degree.certificate.seeking.students.submitting.SAT.scores..ADM2021.)
    })
    
    output$SATscore2 <- renderPlotly ({
      
      SATscore02<- plot_ly(
      domain = list(x = c(0, 1), y = c(0, 1)),
      value = SATscoredata2()$Percent.of.first.time.degree.certificate.seeking.students.submitting.SAT.scores..ADM2021.,
      title = list(text = "SAT score Submission Percentage"),
      type = "indicator",
      mode = "gauge+number",
      gauge = list(
        axis = list(range = list(NULL, 100)))
      )
      SATscore02 %>%
      layout(margin = list(l=20,r=30),
             width= "100px",
             height= "100px")
    })
  })


