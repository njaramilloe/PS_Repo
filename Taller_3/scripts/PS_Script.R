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
       doParallel, #deliver tasks to each of the pc's cores
       ggplot2, #data visualization
       expss, #functions from spreadsheets and SPSS Statistics software
       plyr, #round_any function
       glmnet, #para spatial correlation
       ranger, #para realizar bosques
       bst, #para realizar bosques con boosting
       parallel,
       doParallel,
       xtable, #to export to latex
       forecast
)

#Assign Cores
detectCores() #8
registerDoParallel(6)

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
train <- train_hogares %>%
  left_join(train_personas, by = c("id"))
#test data
test <- test_hogares %>%
  left_join(test_personas, by = c("id"))

#-----------------------export the data base------------------------------------
#train data
write.csv(train, file = "train.csv", row.names = FALSE )

#test data
write.csv(train, file = "test.csv", row.names = FALSE )

#----------------------understanding data---------------------------------------
#Glimpse into the data bases
glimpse(test) # Lp : Línea de pobreza
head(test[1:15])

glimpse(train) # Ingtot : Ingreso Total
head(train[1:15])

train<-tolower(train)

















? #Generate new variable that identifies the sample
#test<-test %>% mutate(sample="test")  # test 79 variables
#train<-train %>% mutate(sample="train")   # train 158 variables

? #Bind together both databases
#total_table<-rbind(test,train) 
#table(total_table$sample) #test 10286 | train 38644

#----------------------understanding data---------------------------------------
#Glimpse into the data base
head(total_table)
table(total_table$sample) #test 10286 | train 38644
colnames(total_table) <- tolower(colnames(total_table))
names(total_table)

#Check for string inconsistencies
unique(total_table$property_type)

#Check for missing values
colSums(is.na(total_table))/nrow(total_table)*100 

# Check the levels of each variable
variable_levels <- sapply(total_table, function(x) length(unique(x)))
variable_levels

# Identify variables with only one level
single_level_vars <- names(variable_levels[variable_levels == 1])
single_level_vars

#Vtable statistics
total_table_selected<- total_table %>% select(property_id, bedrooms, bathrooms, surface_covered)
sumtable(total_table_selected, out = "return")








