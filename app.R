library(shiny)
library(tidyverse)
library(ggplot2)
library(gridExtra)

data <- read.csv("data/cleaned_survey_data.csv", stringsAsFactors = FALSE)

ui <- fluidPage(
  titlePanel("Mental Health in Tech Analyzer",
             windowTitle = "Mental Health in Tech Analyzer"),
  sidebarLayout(
     sidebarPanel(
       selectInput("countryInput", "Select country", 
                   choices = c("All", "Australia", "Canada", 
                               "France", "Germany", "Ireland", 
                               "Netherlands", "United Kingdom", "United States"), 
                   selected = "All"),
       radioButtons("genderInput", "Select Gender",
                    choices = c("All", "Male", "Female"), 
                    selected = "All"), 
       selectInput("ageInput", "Select age group", 
                   choices = c("All" = 1, 
                                  "< 30" = 2, 
                                  "30~40" = 3, 
                                  "40~50" = 4, 
                                  ">50" = 5), 
                   selected = 1), 
       selectInput("companyInput", "Select company size", 
                   choices = c("All", "Less than 100", "100-500", "500-1000", "More than 1000"), 
                   selected = 1)
     ),
     mainPanel(
       fluidRow(column(6, plotOutput('plot1')),
                column(6, plotOutput('plot2'))),
       fluidRow(column(6, plotOutput('plot3')),
                column(6, plotOutput('plot4'))),
       fluidRow(column(6, plotOutput('plot5')),
                column(6, plotOutput('plot6')))
       # plotOutput('plotgraph'),
       # plotOutput('plot6')
     )
   )
)

server <- function(input, output) {
  
  dataFilter <- reactive({
    # first layer of filter 
    # filter data based on 'countryInput', which represents selected country 
    if(input$countryInput == 'All') {
      filteredData <- data
    } else {
      filteredData <- data %>%
        filter(Country == input$countryInput)
    }
    
    # second layer of filter 
    # filter data based on 'genderInput', which represents selected gender
    if(input$genderInput == 'All') {
      filteredData <- filteredData
    } else {
      filteredData <- filteredData %>%
        filter(Gender == input$genderInput)
    }
    
    # third layer of filter 
    # filter data based on 'ageInput', which represents selected age group
    if(input$ageInput == '1') { 
      filteredData <- filteredData
    } else {
      if(input$ageInput == '2') {
        filteredData <- filteredData %>% 
          filter(Age <= 30)
      } else {
        if(input$ageInput == '3') {
          filteredData <- filteredData %>% 
            filter(Age > 30, Age <= 40)
        } else {
          if(input$ageInput == '4') {
            filteredData <- filteredData %>% 
              filter(Age > 40, Age <= 50)
          } else {
            filteredData <- filteredData %>% 
              filter(Age > 50)
          }
        }
      }
    }
    
    # last layer of filter 
    # filter data based on 'companyInput', which represents selected company size 
    if(input$companyInput == 'All') { 
      filteredData <- filteredData
    } else {
      filteredData <- filteredData %>%
        filter(no_employees == input$companyInput)
    }
  })

  # plot the first bar chart based on variable 'mental_health_consequence'
  output$plot1 <- renderPlot({
    dataFilter() %>% 
      ggplot(aes(mental_health_consequence, fill=mental_health_consequence)) + 
      geom_bar() + 
      ggtitle('Do you think that discussing a mental \nhealth issue with your employer would \nhave negative consequences?') + 
      theme(legend.position="none")
  })
  
  # plot the second bar chart based on variable 'coworkers' 
  output$plot2 <- renderPlot({
    dataFilter() %>% 
      ggplot(aes(coworkers, fill=coworkers)) + 
      geom_bar() + 
      ggtitle('Would you be willing to discuss a mental \nhealth issue with your coworkers?') + 
      theme(legend.position="none")
  })

  # plot the third bar chart based on variable 'supervisor' 
  output$plot3 <- renderPlot({
    dataFilter() %>% 
      ggplot(aes(supervisor, fill=supervisor)) + 
      geom_bar() + 
      ggtitle('Would you be willing to discuss a mental \nhealth issue with your direct supervisor(s)?') + 
      theme(legend.position="none")
  })
  
  # plot the fourth bar chart based on variable 'mental_health_interview' 
  output$plot4 <- renderPlot({
    dataFilter() %>% 
      ggplot(aes(mental_health_interview, fill=mental_health_interview)) + 
      geom_bar() + 
      ggtitle('Would you bring up a mental health issue \nwith a potential employer in an interview?') + 
      theme(legend.position="none")
  })
  
  # plot the fifth bar chart based on variable 'care_options' 
  output$plot5 <- renderPlot({
    dataFilter() %>% 
      ggplot(aes(care_options, fill=care_options)) + 
      geom_bar() + 
      ggtitle('Do you know the options for mental health \ncare your employer provides?') + 
      theme(legend.position="none")
  })
  
  # plot the sixth bar chart based on variable 'wellness_program' 
  output$plot6 <- renderPlot({
    dataFilter() %>% 
      ggplot(aes(wellness_program, fill=wellness_program)) + 
      geom_bar() + 
      ggtitle('Has your employer ever discussed mental \nhealth as part of an employee wellness program?') + 
      theme(legend.position="none")
  })
  
}

shinyApp(ui = ui, server = server)

