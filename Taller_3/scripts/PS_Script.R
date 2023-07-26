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
       forecast,
       AER,
       MLmetrics,
       tidymodels,
       themis,
       smotefamily,
       caret, 
       vtable
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

#P6040 edad
#P6020 sexo 1  hombre, 2 mujer
#Ingtot : Ingreso Total

#PRIMER INTENTO-----------------------------------------------------------------
#Keep only relevant variables
train_n <- train[, c("id", "Clase.x", "Li", "Lp", "Pobre", "Indigente", "P6020", "P6040", "Pet",
                     "Oc", "Des", "Ina", "Ingtot")]
test_n <- test[, c("id", "Clase.x", "Li", "Lp", "P6020", "P6040", "Pet", "Oc", "Des", "Ina")] #Pobre, Indigente, and Ingtot don't exist.

#Create the missing variables in the test database with NA values in all rows of the data frame
test_n$Pobre <- NA
test_n$Indigente <- NA
test_n$Ingtot <- NA


glimpse(test_n) #13 variables
glimpse(train_n) #13 variables



#Replace missings with mode in Ingtot (17.57% of observations are missings)
colSums(is.na(train_n))/nrow(train_n)*100 

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

train_n$Ingtot[is.na(train_n$Ingtot)] <- Mode(train_n$Ingtot) 

#Normalice database for continuous variables
numericas_relevantes <- c("P6040", "Ingtot", "Li", "Lp")

escalador <- preProcess(train_n[, numericas_relevantes],
                        method = c("center","scale"))

train_n[, numericas_relevantes] <- predict(escalador, train_n[, numericas_relevantes])
test_n[, numericas_relevantes] <- predict(escalador, test_n[, numericas_relevantes])

##Bind both databases together -------------------------------------------------
#Generate new variable that identifies the sample
test_n<-test_n %>% mutate(sample="test")
train_n<-train_n %>% mutate(sample="train")

#Bind together both databases
total_table<-rbind(test_n,train_n)
table(total_table$sample) #test 219644 | train 543109


##Clean the database------------------------------------------------------------
#Change P6020 = sex from women 2 to 0
total_table$P6020 <- ifelse(total_table$P6020 == 2, 0, total_table$P6020)
glimpse(total_table) 

#Change Clase.x from 2 to 0 resto
total_table$Clase.x <- ifelse(total_table$Clase.x == 2, 0, total_table$Clase.x)
glimpse(total_table) 


#Ajustar las variables para que sean factores
total_table <- total_table %>%  mutate(Clase.x = as.factor(Clase.x),
                                       P6020 = as.factor(P6020), #sexo 1: hombre 0: mujer
                                       Pet = as.factor(Pet), #Población en edad de trabajar 1: sí 0: no
                                       Oc = as.factor(Oc), # Ocupado 1: sí
                                       Des = as.factor(Des), # Desocupado 1: sí
                                       Ina = as.factor(Ina), # Inactivo 1: sí
                                       Pobre = as.factor(Pobre), # Pobre=1 No pobre=0
                                       Indigente = as.factor(Indigente)) # Indigente=1 No indigente=0 
glimpse(total_table) 

#Check for imbalance in Pobre 
prop.table(table(total_table$Pobre)) #74.86% de la muestra  no es pobre, 25.13% lo  es
#Desbalance no es extremo pues el 25% de la muestra es la clase minoritaria, por lo que se podría decir que hay un desbalance moderado

#Understanding data-------------------------------------------------------------
#Glimpse into the data base
head(total_table)
table(total_table$sample) #test 219.644 | train 543.109
colnames(total_table) <- tolower(colnames(total_table))
names(total_table)

#Check for missing values
colSums(is.na(total_table))/nrow(total_table)*100 

# Check the levels of each variable
variable_levels <- sapply(total_table, function(x) length(unique(x)))
variable_levels

#Vtable statistics
total_table_selected<- total_table %>% select(li, lp, p6020, p6040, pet, oc, des, ina, pobre, indigente, ingtot)
sumtable(total_table_selected, out = "return")
#export to latex
sum <- xtable(sum)
# Export the table to a LaTeX file
#print.xtable(sum, file = "/Users/nataliajaramillo/Documents/GitHub/PS_Repo/Taller_3/stores/sumtable.tex", floating = FALSE)


##Convert to tibble to make it go faster
total_table <- as.tibble(total_table)



