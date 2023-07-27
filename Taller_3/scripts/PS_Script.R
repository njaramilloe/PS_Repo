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
       vtable #output a descriptive variable table while working with data
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

#'calculate aggregated household income using train_personas databases
# aggregate individual-level data by house id and counting family size
household_data_train <- train_personas %>%
  group_by(id) %>%
  summarize(household_income = sum(Ingtot),
            total_person = max(Orden))

# calculate household income for each member
train_personas <- train_personas %>%
  group_by(id) %>%
  mutate(total_income = sum(Ingtot)) %>%
  ungroup() %>%
  mutate(household_income_participation = Ingtot / total_income)

train_personas$total_income
train_personas$household_income_participation

# filter the data to include only family members up to number 13
household_income_df <- train_personas %>%
  filter(Orden <= 13)

# Plot the average participation of each member of the family (up to 13)
average_participation_plot <- household_income_df %>%
  group_by(Orden) %>%
  summarize(average_participation = mean(household_income_participation, na.rm = TRUE)) %>%
  ggplot(aes(x = factor(Orden), y = average_participation, fill = factor(Orden))) +
  geom_bar(stat = "identity", fill = "blue", width = 0.6) +
  labs(x = "Family Member Order", y = "Average Income Participation") +
  ggtitle("Average Household Income Participation by Family Member Order (Up to 13)")

print(average_participation_plot)

# plot safe in views file
ggsave("../views/average_participation.png", plot = average_participation_plot)

#erase the database used to make the graphic
rm(list = "household_income_df", "average_participation_plot")

#retrieve data from de household head
family_head_train <- train_personas %>%
  filter(order == 1) %>%
  select(id, P6040, P6020, P6040, P6050, P6090, P6100, P6210, P6210s1, P6240, 
         Oficio, P6426, P6430)


#podemos borrar esta parte 
'colnames(train_hogares) #La columna id identifica el hogar y orden es la identificación de persona
colnames(train_personas)[1:2]'

'sum_ingresos <- train_personas %>% 
  group_by(id) %>% 
  summarize(Ingtot_hogar=sum(train_personas$Ingtot,na.rm = TRUE)) 
summary(sum_ingresos)'


#----------------------merging data---------------------------------------------
#train data
train <- train_hogares %>%
  left_join(train_personas, by = c("id"))
#test data
test <- test_hogares %>%
  left_join(test_personas, by = c("id"))

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

#(?) Normalice database for continuous variables
#numericas_relevantes <- c("P6040", "Ingtot", "Li", "Lp")

#escalador <- preProcess(train_n[, numericas_relevantes],
                        method = c("center","scale"))

#train_n[, numericas_relevantes] <- predict(escalador, train_n[, numericas_relevantes])
#test_n[, numericas_relevantes] <- predict(escalador, test_n[, numericas_relevantes])

##Bind both databases together -------------------------------------------------
#Generate new variable that identifies the sample
test_n<-test_n %>% mutate(sample="test")
train_n<-train_n %>% mutate(sample="train")

#Bind together both databases
total_table<-rbind(test_n,train_n)
table(total_table$sample) #test 219644 | train 543109

#Export the data base
write.csv(total_table, file = "total_table.csv", row.names = FALSE )

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

#Check the levels of each variable
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

## Modelo 1 Logit --------------------------------------------------------------
#Divide the total data to keep only the wanted training data variables (total income, age, sex)
train_data <- total_table  %>% filter(sample=="train")  %>% select(ingtot , p6020, p6040, id)  %>% na.omit()
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
test_data<-total_table  %>% filter(sample=="test")  

#Predict total income with logit
test_data$ingtot <- predict(logit, test_data)

head(test_data %>% select(id,ingtot))

#Construct the dummy variables pobre & indigente
test_data$pobre <- ifelse(test_data$lp > test_data$ingtot, 1, 0)

test_data$indigente <- ifelse(test_data$li > test_data$ingtot, 1, 0)

#Create the submission document by selecting only the variables required and renaming them to adjust to instructions
submit<-test_data  %>% select(id,pobre)
write.csv(submit,"Modelo1.csv",row.names=FALSE)







#Spatial Block Cost Complexity Prunning - Bagging
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



