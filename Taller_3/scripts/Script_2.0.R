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
       ggplot2, #data visualization
       expss, #functions from spreadsheets and SPSS Statistics software
       plyr, #round_any function
       glmnet, #para spatial correlation
       ranger, #random forests or recursive partitioning
       bst, #boosting trees for classification and regression
       parallel, #support for parallel computation in R
       doParallel, #deliver tasks to each of the pc's cores
       xtable, #to export to latex
       forecast, 
       AER, #applied econometrics wiht R
       MLmetrics, #evaluation metrics  regression and classification performance
       tidymodels, #collection of packages for modeling and statistical analysis
       themis, #extra recipes steps for dealing with unbalanced data
       smotefamily, #detailed description of what the package does
       caret, #classification and regression training package
       vtable, #output a descriptive variable table while working with data
       adabag #For adaboost
)

# set and register cores to parallel processing
num_cores <- detectCores() #8
print(num_cores)
cl <- makeCluster(num_cores-2) #6
registerDoParallel(cl) 
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
#training personas
train_personas <- read.csv("train_personas.csv")
#test personas
test_personas <- read.csv("test_personas.csv")
#-------collapsing data from individual-level to household-level data-----------
#TRAIN PERSONAS: Check the number of unique values in 'id' column
n_distinct(train_personas$id)
n_distinct(train_hogares$id)
#TRAIN PERSONAS: Using the aggregate() function alculate aggregated household income using train_personas databases
train_household_sum <- aggregate(cbind(Ingtot) ~ id, data = train_personas, FUN = sum)
train_household_max <- aggregate(cbind(Orden) ~ id, data = train_personas, FUN = max)
family_head_train <- train_personas %>%
  filter(Orden == 1) %>%
  select(id, P6020, P6040)
#Bind together
train_household <- train_household_sum %>%
  left_join(train_household_max, by = c("id"))
train_household <- train_household %>%
  left_join(family_head_train, by = c("id"))
#Divide Ingtotal household into ingtotal per person to compare to Lp and Li
train_household <- train_household %>% mutate(Ingtot = (Ingtot / Orden))
#TEST PERSONAS: Check the number of unique values in 'id' column
n_distinct(test_personas$id)
n_distinct(test_hogares$id)
#TEST PERSONAS: Using the aggregate() function alculate aggregated household income using train_personas databases
#test_household_mean <- aggregate(cbind(P6040, P6020) ~ id, data = test_personas, FUN = mean)
test_household_max <- aggregate(cbind(Orden) ~ id, data = test_personas, FUN = max)
family_head_test <- test_personas %>%
  filter(Orden == 1) %>%
  select(id, P6020, P6040)
#Bind together
test_household <- family_head_test %>%
  left_join(test_household_max, by = c("id"))
#----------------------merging data---------------------------------------------
#train data
train <- train_hogares %>%
  left_join(train_household, by = c("id"))
#test data
test <- test_hogares %>%
  left_join(test_household, by = c("id"))
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
train_n <- train[, c("id", "Clase", "Li", "Lp", "Pobre", "Indigente", "P6020", "P6040", "Ingtot")] 
test_n <- test[, c("id", "Clase", "Li", "Lp", "P6020", "P6040")] #Pobre, Indigente, and Ingtot don't exist.
#Create the missing variables in the test database with NA values in all rows of the data frame
test_n$Pobre <- NA
test_n$Indigente <- NA
test_n$Ingtot <- NA
glimpse(test_n) #9 variables
glimpse(train_n) #9 variables
#Replace missings with mode in Ingtot (17.57% of observations are missings)
colSums(is.na(train_n))/nrow(train_n)*100 
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
train_n$Ingtot[is.na(train_n$Ingtot)] <- Mode(train_n$Ingtot) 
##Bind both databases together -------------------------------------------------
#Generate new variable that identifies the sample
test_n<-test_n %>% mutate(sample="test")
train_n<-train_n %>% mutate(sample="train")
#Bind together both databases
total_table<-rbind(test_n,train_n)
table(total_table$sample) #test 66168 | train 164960
#Export the data base
write.csv(total_table, file = "total_table.csv", row.names = FALSE )
##Clean the database------------------------------------------------------------
#Change P6020 = sex from women 2 to 0
total_table$P6020 <- ifelse(total_table$P6020 == 2, 0, total_table$P6020)
glimpse(total_table) 
#Change Clase from 2 to 0 resto
total_table$Clase <- ifelse(total_table$Clase == 2, 0, total_table$Clase)
glimpse(total_table) 
#Ajustar las variables para que sean factores
total_table <- total_table %>%  mutate(Clase = as.factor(Clase),
                                       P6020 = as.factor(P6020), #sexo 1: hombre 0: mujer
                                       Pobre = as.factor(Pobre), # Pobre=1 No pobre=0
                                       Indigente = as.factor(Indigente)) # Indigente=1 No indigente=0 
glimpse(total_table) 
total_table %>%
  filter(sample == "train") %>%
  glimpse()
#Check for imbalance in Pobre 
prop.table(table(total_table$Pobre)) #79.98% de la muestra  no es pobre, 20% lo  es
#Desbalance no es extremo pues el 20% de la muestra es la clase minoritaria, por lo que se podría decir que hay un desbalance moderado
#Understanding data-------------------------------------------------------------
#Glimpse into the data base
head(total_table)
table(total_table$sample) #test 66.168 | train 164.960
colnames(total_table) <- tolower(colnames(total_table))
names(total_table)
#Check for missing values
colSums(is.na(total_table))/nrow(total_table)*100 
#Check the levels of each variable
variable_levels <- sapply(total_table, function(x) length(unique(x)))
variable_levels
#Vtable statistics
total_table_selected<- total_table %>% select(li, lp, p6020, p6040, pobre, indigente, ingtot)
sumtable(total_table_selected, out = "return")
#export to latex
sum <- xtable(sum)
# Export the table to a LaTeX file
#print.xtable(sum, file = "/Users/nataliajaramillo/Documents/GitHub/PS_Repo/Taller_3/stores/sumtable.tex", floating = FALSE)
##Convert to tibble to make it go faster
total_table <- as.tibble(total_table)
## Modelo 1 Logit --------------------------------------------------------------
#Divide the total data to keep only the wanted training data variables (total income, age, sex)
train_data <- total_table  %>% filter(sample=="train")  %>% select(ingtot , p6020, p6040, id)  %>% na.omit()
# Calculate the percentage of zeros in the ingtot column
percentage_zeros <- 100 * mean(train_data$ingtot == 0, na.rm = TRUE)
print(percentage_zeros) # <1% are zeros. We will drop those observations
#Drop zeros in ingtot
train_data <- subset(train_data, ingtot != 0)
colSums(is.na(train_data))/nrow(train_data)*100 
#Logit regression
set.seed(123)
#Train the model with logit regression
logit <- train(
  ingtot ~ p6020 + p6040 + (p6040*p6040),
  data = train_data,
  method = "glmnet",
  preProcess = NULL
)
#Construct the test data frame
test_data <- total_table  %>% filter(sample=="test")  
#Predict total income with logit
test_data$ingtot <- predict(logit, test_data)
head(test_data %>% select(id,ingtot))
#test_data$ingtot <- round(test_data$ingtot, digits = -2)    #digits = -2 indicates rounding to the nearest 100 (10^2)
#Construct the dummy variables pobre & indigente
test_data$pobre <- ifelse(test_data$lp > test_data$ingtot, 1, 0)
test_data$indigente <- ifelse(test_data$li > test_data$ingtot, 1, 0)
#Create the submission document by selecting only the variables required and renaming them to adjust to instructions
submit<-test_data  %>% select(id,pobre)
write.csv(submit,"Modelo1.csv",row.names=FALSE)


## Modelo 2 Ada Boost ----------------------------------------------------------

#Divide the total data to keep only the wanted training data variables (total income, age, sex)
train_data <- total_table  %>% filter(sample=="train")  %>% select(ingtot , p6020, p6040, id, pobre, indigente)  %>% na.omit()

# Calculate the percentage of zeros in the ingtot column
percentage_zeros <- 100 * mean(train_data$ingtot == 0, na.rm = TRUE)
print(percentage_zeros) # <1% are zeros. We will drop those observations
#Drop zeros in ingtot
train_data <- subset(train_data, ingtot != 0)
colSums(is.na(train_data))/nrow(train_data)*100 

#Ada Boost
set.seed(123)

#Train the model with ada boost
  ada_boost1 <- train(
    pobre ~ p6020 + p6040 + (p6040*p6040),
    data = train_data,
    method = "AdaBoost.M1",
    trControl = ctrl,
    tuneGrid  = expand.grid(
      mfinal = c(50,100,150), #VER PARAMETROS MAS GRANDES
      maxdepth = c(1,2,3),
      coeflearn = c('Breiman', 'Freund'))
  )
  
  ada_boost1
  
  #Construct the test data frame
  test_data <- total_table  %>% filter(sample=="test")  
  

  #Predict poverty with ada boost
  predictTest_ada <- data.frame(
    obs = test_data$pobre,
    predict(ada_boost1, newdata = test_data,  type = "prob"),
    pred = predict(ada_boost1, newdata = test_data, type = "raw"))
  
 
  head(test_data %>% select(id,pobre))
  
  #Accuracy
  mean(predictTest_ada$obs == predictTest_ada$pred)

  
  #Create the submission document by selecting only the variables required and renaming them to adjust to instructions
  submit<-test_data  %>% select(id,pobre)
  write.csv(submit,"Modelo2.csv",row.names=FALSE)
  
  
  
  ## Modelo 2 Spatial Block Cost Complexity Prunning - Bagging -------------------
  fitControl <- trainControl(method = "cv",
                             number = 10)
  #Train the model with ingtot
  tree_ranger <- train(
    ingtot ~ p6020 + p6040 + (p6040*p6040),
    data = train_data,
    method = "ranger",
    trControl = fitControl,
    metric = "MAE",
    tuneGrid = expand.grid(
      mtry = c(1,2,3),  #número de predictores que va a sacar aleatoriamente. En este caso en cada bootstrap saca 1 predictor de la regresión
      splitrule = "variance", #Regla de partición
      min.node.size = c(5,10,15)) #Cantidad de observaciones en el nodo. Default 5 para regresiones
  )
  tree_ranger
  tree_ranger$bestTune
  train_data$pred_tree<-predict(tree_ranger,train_data)
  #Construct the test data frame
  test_data<-total_table  %>% filter(sample=="test")  
  #Predict the tree with test data
  test_data$pred_tree<-predict(tree_ranger,test_data)
  head(test_data %>% select(property_id,pred_tree))
  #Drop the variable geometry and return Log(prices) into Price
  test_data <- test_data   %>% st_drop_geometry()  %>% mutate(pred_tree=exp(pred_tree))
  test_data$price <- round(test_data$pred_tree, digits = -7)    #Indicates rounding to the nearest 10.000.000 (10^7)
  head(test_data  %>% select(property_id, pred_tree, price))
  #Create the submission document by selecting only the variables required and renaming them to adjust to instructions
  submit<-test_data  %>% select(property_id,price)
  write.csv(submit,"Tree_v14.csv",row.names=FALSE)
  #MAE and MAPE train 
  MAE(train_data$pred_tree, train_data$price)
  MAPE(train_data$pred_tree, train_data$price)
  #MAE
  MAE(test_data$pred_tree, test_data$price)
  #MAE V14: 2.496.469
  MAPE(test_data$pred_tree, test_data$price)
  ### Model Comparison------------------------------------------------------------
  # Create a dataframe with the model information
  modelcomparison <- data.frame(
    Model = c("Model 14", "Model 10", "Model 9", "Model 3Rounded"),
    Method = c("Random Forest", "Elastic Net Regularization", "Recursive Partitioning and Regression Tree", "Recursive Partitioning and Regression Tree"),
    Variables = c("15", "15", "14", "4"),
    MAE = c("2.496.469", "2.496.152", "2.653.858", "3.176.430")
  )
  # Create the chart using ggplot2
  table <- stargazer(modelcomparison, 
                     title = "Model Comparison", 
                     column.labels = c("Model", "Method", "Variables", "MAE"),
                     label = "tab:model_comparison",
                     align = TRUE,
                     header = FALSE,
                     summary = FALSE)
  # Display the chart
  cat(table, sep = "")
  # stop the cores cluster on parallel processing
  stopCluster(cl)
  