rm(list = ls())

library(pacman)
p_load(rvest, tidyverse, ggplot2, rio, skimr, caret, stargazer,expss, boot)

# Set working directory
setwd("/Users/nataliajaramillo/Documents/GitHub/Taller_2/stores")


#LOAD DATA
#Load training data
train <- read.csv("train.csv")

#Load test data
test <- read.csv("test.csv")

#DESCRIPTIVE STATISTICS
#Understand the training data
glimpse(train)
head(train[1:15])

#Check for missing values
train %>%
  is.na()%>%
  sum()

# Check the levels of each variable
variable_levels <- sapply(train, function(x) length(unique(x)))
variable_levels

# Identify variables with only one level
single_level_vars <- names(variable_levels[variable_levels == 1])
single_level_vars

#Predicting prices
lm_model<- lm(price ~ surface_total + surface_covered + rooms + bedrooms + bathrooms + property_type, data = train)
summary(lm_model)

