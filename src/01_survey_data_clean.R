# !/usr/bin/env Rscript 
# 
# 01_survey_data_clean.R
# 
# Jingyun Chen, Gilbert Lei - Jan, 2019
#
# Purpose: clean mental health survey data

# load libraries
library(tidyverse) 

# load data
survey_data <- read.csv("../data/survey.csv")

# select 10 features for this project
selected_feat <- c("Country", "Gender", "Age", "no_employees", "mental_health_consequence", "coworkers", "supervisor", "mental_health_interview", "care_options", "wellness_program")
# only consider tech companies
# only consider countries that have more than 10 respondents in this survey
survey_data <- survey_data %>% 
  filter(tech_company == "Yes") %>% 
  select(selected_feat) %>% 
  group_by(Country) %>% 
  mutate(n = n()) %>% 
  filter(n >= 10) %>% 
  select(-n) %>% 
  ungroup()

# encoding gender
male_sub <- c("cis male", "Cis Male", "Cis Man", "m", "M", "Mail", "maile", "Make", "Mal", "male", "Male", "Male ", "Male (CIS)", "Malr", "Man", "msle")
female_sub <- c("cis-female/femme", "Cis Female", "f", "F", "femail", "Femake", "female", "Female", "Female ", "Female (cis)", "Female (trans)", "Trans-female", "Trans woman", "woman", "Woman")

survey_data <- survey_data %>% 
  mutate(Gender = ifelse((survey_data$Gender %in% male_sub), "Male",
                         ifelse((survey_data$Gender %in% female_sub), "Female", "Others"))) %>% 
  mutate(Gender = as.factor(Gender))

# encoding age
# there are 4 outliers (age < 18 or age > 80), we only consider employee whose age is from 18 to 80
survey_data <- survey_data %>% 
  filter(Age>= 18 & Age <=80) %>% 
  mutate(Age = as.integer(Age))

# encoding company size
# For company which no_employees is either "1-5", "6-25" or, "26-100" is encoded as "Less than 100"
survey_data <- survey_data %>%
  mutate(no_employees = as.character(no_employees)) %>%
  mutate(no_employees = ifelse(no_employees %in% c("1-5","6-25","26-100"), "Less than 100", no_employees)) %>%
  mutate(no_employees = as.factor(no_employees))

# store cleaned data
write.csv(survey_data, file = "../data/cleaned_survey_data.csv")
