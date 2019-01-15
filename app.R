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
                               "France", "Germany", "Irland", 
                               "Netherlands", "United Kingdom", "United States"), 
                   selected = "All"),
       radioButtons("genderInput", "Select Gender",
                    choices = c("All", "Male", "Female"), 
                    selected = "Male"), 
       selectInput("ageInput", "Select age group", 
                   choices = c("All" = 1, 
                                  "< 30" = 2, 
                                  "30~40" = 3, 
                                  "40~50" = 4, 
                                  ">50" = 5), selected = 1), 
       selectInput("companyInput", h3("Select company size"), 
                   choices = c("All" = 1, 
                                  "< 100" = 2, 
                                  "100~500" = 3, 
                                  "500~1000" = 4, 
                                  ">1000" = 5), selected = 1)
     ),
     mainPanel(
       plotOutput('plot1'),
       plotOutput('plot2'),
       plotOutput('plot3'),
       plotOutput('plot4'),
       plotOutput('plot5'),
       plotOutput('plot6')
     )
   )
)

server <- function(input, output) {
  
  countryFilter <- reactive({
    if(input$countryInput == 'All') {
      data
    } else {
      data %>%
        filter(Country == input$countryInput)
    }
  })

  genderFilter <- reactive({
    if(input$genderInput == 'All') {
      countryFilter()
    } else {
      countryFilter() %>%
        filter(Gender == input$genderInput)
    }
  })

  output$plot1 <- renderPlot({
    genderFilter() %>% 
      ggplot(aes(mental_health_consequence, fill=mental_health_consequence)) + 
      geom_bar() + 
      ggtitle('Do you think that discussing a mental \nhealth issue with your employer would \nhave negative consequences?') + 
      theme(legend.position="none")
  })
  
  output$plot2 <- renderPlot({
    genderFilter() %>% 
      ggplot(aes(coworkers, fill=coworkers)) + 
      geom_bar() + 
      ggtitle('Would you be willing to discuss a mental \nhealth issue with your coworkers?') + 
      theme(legend.position="none")
  })

  output$plot3 <- renderPlot({
    genderFilter() %>% 
      ggplot(aes(supervisor, fill=supervisor)) + 
      geom_bar() + 
      ggtitle('Would you be willing to discuss a mental \nhealth issue with your direct supervisor(s)?') + 
      theme(legend.position="none")
  })
  
  output$plot4 <- renderPlot({
    genderFilter() %>% 
      ggplot(aes(mental_health_interview, fill=mental_health_interview)) + 
      geom_bar() + 
      ggtitle('Would you bring up a mental health issue \nwith a potential employer in an interview?') + 
      theme(legend.position="none")
  })
  
  output$plot5 <- renderPlot({
    genderFilter() %>% 
      ggplot(aes(care_options, fill=care_options)) + 
      geom_bar() + 
      ggtitle('Do you know the options for mental health \ncare your employer provides?') + 
      theme(legend.position="none")
  })
  
  output$plot6 <- renderPlot({
    genderFilter() %>% 
      ggplot(aes(wellness_program, fill=wellness_program)) + 
      geom_bar() + 
      ggtitle('Has your employer ever discussed mental \nhealth as part of an employee wellness program?') + 
      theme(legend.position="none")
  })
}

shinyApp(ui = ui, server = server)

