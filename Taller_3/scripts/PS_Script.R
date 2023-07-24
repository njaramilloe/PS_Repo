#' *****************************************************************************
#' ******************BIG DATA AND ML:PROBLEM SET 3******************************
#' *****************************************************************************


#----------------------prepare the environment----------------------------------
#clean global environment
rm(list = ls())

#load packages
require("pacman")

p_load(stringi, #manipulate string/text data in the cleaning process
       stringr, #functions designed to work with strings operations
       tokenizers, #convert natural language text into tokens
       stopwords, #low value commonly used words cleaning
       SnowballC, #apply stemming to each word reducing them to their root forms
       wordcloud, #takes words frequencies to create a cloud visualization
       tidyverse, #data manipulation and visualization
       tm, #text mining and natural language processing task
       rio, #import/export data file formats
       skimr, #summary data
       stargazer, #generate publication-quality tables
       RColorBrewer, #color palettes for thematic maps
       xlsx, #read and write format excel files
       purrr, #converts list variables to string
       dplyr, #data manipulation
       tidytext, #text mining tasks 
       doParallel #deliver tasks to each of the pc's cores
)

# set working directory
path_script <- rstudioapi::getActiveDocumentContext()$path
path_folder <- dirname(path_script)
setwd(path_folder)
setwd("../stores")


#----------------------loading data---------------------------------------------
# training houses
train_hogares <- read.csv("train_hogares.csv")
#test houses
test_hogares <- read.csv("test_hogares.csv")
#training persons
train_personas <- read.csv("train_personas.csv")
#test persons
test_personas <- read.csv("test_personas.csv")

#----------------------merging data---------------------------------------------
#train data
train_data <- train_hogares %>%
  left_join(train_personas, by = c("id"))
#test data
test_data <- test_hogares %>%
  left_join(test_personas, by = c("id"))

#----------------------merging data---------------------------------------------