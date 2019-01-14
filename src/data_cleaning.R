library(tidyverse) 

data <- read.csv('../data/tech_mental-health/survey.csv') 
head(data)
cleanedData <- data %>% 
  filter(tech_company == 'Yes') %>% 
  select(Country, Gender, Age, no_employees, mental_health_consequence, coworkers, supervisor, mental_health_interview, care_options, wellness_program) %>% 
  group_by(Country) %>% 
  mutate(n = n()) %>% 
  filter(n >= 10) %>% 
  select(-n) %>% 
  ungroup()

write.csv(cleanedData, file = "data/tech_mental-health/cleanedData.csv")
