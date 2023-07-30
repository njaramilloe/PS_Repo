#' *****************************************************************************
#' ******************BIG DATA AND ML:PROBLEM SET 3******************************
#' *****************************************************************************


#----------------------prepare the environment----------------------------------
#clean global environment
rm(list = ls())

#load packages
require("pacman")

#install.packages("rgl")
#install.packages("adabag")

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
       robustbase,
       caret, #classification and regression training package
       vtable, #output a descriptive variable table while working with data
       adabag, #For adaboost
       rpart.plot #To visualize trees
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

#TRAIN PERSONAS: Using the aggregate() function calculate aggregated household income using train_personas databases
train_household_sum <- aggregate(cbind(Ingtot) ~ id, data = train_personas, FUN = sum)
train_household_max <- aggregate(cbind(Orden) ~ id, data = train_personas, FUN = max)

family_head_train <- train_personas %>%
  filter(Orden == 1) %>%
  select(id, P6020, P6040, P6210)

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

#TEST PERSONAS: Using the aggregate() function calculate aggregated household income using train_personas databases
test_household_max <- aggregate(cbind(Orden) ~ id, data = test_personas, FUN = max)

family_head_test <- test_personas %>%
  filter(Orden == 1) %>%
  select(id, P6020, P6040, P6210)

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

#Keep only relevant variables---------------------------------------------------
train_n <- train[, c("id", "Clase", "Li", "Lp", "Pobre", "Indigente", "P6020", "P6040", "Ingtot", "P5090","Nper", "P6210", "Depto","P5130", "P5140", "P5090", "P5000", "P5010")] 

train_n <- train_n %>%
  mutate(P5130 = ifelse(is.na(P5130), P5140, P5130)) %>%
  select(-P5140) %>%
  select(-P5090.1)

test_n <- test[, c("id", "Clase", "Li", "Lp", "P6020", "P6040", "P5090","Nper", "P6210", "Depto","P5130", "P5140", "P5090", "P5000", "P5010")] #Pobre, Indigente, and Ingtot don't exist.

test_n <- test_n %>%
  mutate(P5130 = ifelse(is.na(P5130), P5140, P5130)) %>%
  select(-P5140) %>%
  select(-P5090.1)

#Create the missing variables in the test database with NA values in all rows of the data frame
test_n$Pobre <- NA
test_n$Indigente <- NA
test_n$Ingtot <- NA

glimpse(test_n) #16 variables
glimpse(train_n) #16 variables

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
                                       P5090 = as.factor(P5090), #a. Propia, totalmente pagada b. Propia, la están pagando c. En arriendo o subarriendo d. En usufructo e. Posesión sin titulo (ocupante f. Otra
                                       P6210 = as.factor(P6210), #a. Ninguno b. Preescolar c. Básica primaria (1o - 5o) d. Básica secundaria (6o - 9o) e. Media (10o - 13o) f. Superior o universitaria g. No sabe, no informa
                                       Depto = as.factor(Depto),
                                       P5000 = as.factor(P5000),
                                       P5010 = as.factor(P5010),
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
total_table_selected<- total_table %>% select(li, lp, p6020, p6040, pobre, indigente, ingtot, p5090, p6210, p5130, p5000, p5010)
sumtable(total_table_selected, out = "return")
#export to latex
#sum <- xtable(sum)
# Export the table to a LaTeX file
#print.xtable(sum, file = "/Users/nataliajaramillo/Documents/GitHub/PS_Repo/Taller_3/stores/sumtable.tex", floating = FALSE)

#Dummificador
#dumificador<-dummyVars(formula = ~ . + I(p6040^2) - 1,
#                       data = total_table, 
#                       fullRank = T)

#total_table <- predict(dumificador, newdata = total_table)

##Convert to tibble to make it go faster
total_table <- as.tibble(total_table)

## Modelo 1 Logit --------------------------------------------------------------
#Divide the total data to keep only the wanted training data variables (total income, age, sex)
train_data <- total_table  %>% filter(sample=="train")  %>% select(li, lp, p6020, p6040, p5090, nper, p6210, depto, p5130, p5000, p5010, pobre, indigente, ingtot)  %>% na.omit()

variables_numericas <- c("p6040", "p5130", "li", "lp", "ingtot")
escalador <- preProcess(train_data[, variables_numericas],
                        method = c("center", "scale"))

train_data[, variables_numericas] <- predict(escalador, train_data[, variables_numericas])

train_data <- train_data  %>% mutate(p6020=factor(p6020,levels=c(0,1),labels=c("Woman","Man")))

# Calculate the percentage of zeros in the ingtot column
percentage_zeros <- 100 * mean(train_data$ingtot == 0, na.rm = TRUE)
print(percentage_zeros) # <1% are zeros. We will drop those observations

#Drop zeros in ingtot
train_data <- subset(train_data, ingtot != 0)
colSums(is.na(train_data))/nrow(train_data)*100 

#Logit regression
set.seed(123)

modelo6 <- train(
  pobre ~ p6020 + p6040 + I(p6040^2) + p5090 + p6210 + depto + p5130 + p5000 + p5010 + ingtot,
  data = train_data,
  preProcess = NULL,
  method = "glmnet"
)

modelo6_imp<-varImp(modelo6)
modelo6_imp<-rownames_to_column(modelo6_imp$importance, var = "variable")

ggplot(modelo6_imp, aes(x = Overall, y = reorder(variable, Overall))) +
  geom_col(fill = "darkblue")

y_hat_insample6 <- predict(modelo6, train_data)

#Construct the test data frame
test_data <- total_table  %>% filter(sample=="test")  

test_data[, variables_numericas] <- predict(escalador, test_data[, variables_numericas])

test_data <- test_data %>% select(li, lp, p6020, p6040, p5090, nper, p6210, depto, p5130, p5000, p5010, pobre, indigente, ingtot) 

#test_data <- test_data  %>% mutate(p6020=factor(p6020,levels=c(0,1),labels=c("Woman","Man")))

y_hat_outsample6 <- predict(modelo6, test_data)

#Accuracy
acc_insample6 <- Accuracy(y_pred = y_hat_insample6,
                           y_true = train_data$pobre)

acc_outsample6 <- Accuracy(y_pred = y_hat_outsample6,
                           y_true = test_data$pobre)

#Recall
rec_insample6 <- Recall(y_pred = y_hat_insample6,
                           y_true = train_data$pobre)

rec_outsample6 <- Recall(y_pred = y_hat_outsample6,
                            y_true = test_data$pobre)


#Precision
pre_insample6 <- Precision(y_pred = y_hat_insample6,
                        y_true = train_data$pobre)

pre_outsample6 <- Precision(y_pred = y_hat_outsample6,
                         y_true = test_data$pobre)

#F1
f_insample6 <- F1_Score(y_pred = y_hat_insample6,
                           y_true = train_data$pobre)

f_outsample6 <- F1_Score(y_pred = y_hat_outsample6,
                            y_true = test_data$pobre)

acc_insample6
acc_outsample6
rec_insample6
rec_outsample6
pre_insample6
pre_outsample6
f_insample6
f_outsample6

#Predict total income with logit
test_data$pobre <- predict(modelo6, test_data)

head(test_data %>% select(id,pobre))

#Create the submission document by selecting only the variables required and renaming them to adjust to instructions
submit<-test_data  %>% select(id,pobre)
write.csv(submit,"Modelo1.csv",row.names=FALSE)


#Upsample
train_data2<-upSample(x=select(train_data, -pobre),
                      y= as.factor(train_data$pobre),
                      list = F,
                      yname = "pobre")

nrow(train_data2) - nrow(train_data)
prop.table(table(train_data2$pobre))

modelo7 <- train(
  pobre ~ p6020 + p6040 + I(p6040^2) + p5090 + p6210 + depto + p5130 + p5000 + p5010 + ingtot,
  data = train_data2,
  preProcess = NULL,
  method = "glmnet"
)

modelo7_imp<-varImp(modelo7)
modelo7_imp<-rownames_to_column(modelo7_imp$importance, var = "variable")

ggplot(modelo7_imp,  aes(x=Overall, y=reorder(variable, Overall)) +
         geom_col(fill = "darkblue")) 

y_hat_insample7 <- predict(modelo7, train_data2)
y_hat_outsample7 <- predict(modelo7, test_data)

#Accuracy
acc_insample7 <- Accuraccy(y_pred = y_hat_insample7,
                           y_true = train_data2$pobre)

acc_outsample7 <- Accuraccy(y_pred = y_hat_outsample7,
                            y_true = test_data$pobre)

#Recall
rec_insample7 <- Recall(y_pred = y_hat_insample7,
                        y_true = train_data2$pobre)

rec_outsample7 <- Recall(y_pred = y_hat_outsample7,
                         y_true = test_data$pobre)


#Precision
pre_insample7 <- Precision(y_pred = y_hat_insample7,
                           y_true = train_data2$pobre)

pre_outsample7 <- Precision(y_pred = y_hat_outsample7,
                            y_true = test_data$pobre)

#F1
f_insample7 <- F1_Score(y_pred = y_hat_insample7,
                        y_true = train_data2$pobre)

f_outsample7 <- F1_Score(y_pred = y_hat_outsample7,
                         y_true = test_data$pobre)


rec_insample7
rec_outsample7
pre_insample7
pre_outsample7
f_insample7
f_outsample7



#----------------------------

#Train the model with logit regression
logit <- train(
  ingtot ~ p6020 + p6040 + (p6040*p6040),
  data = train_data,
  method = "glmnet",
  preProcess = NULL
)

#Construct the test data frame
test_data <- total_table  %>% filter(sample=="test")  

test_data[, variables_numericas] <- predict(escalador, test_data[, variables_numericas])

test_data <- test_data  %>% mutate(p6020=factor(p6020,levels=c(0,1),labels=c("Woman","Man")))


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

## Modelo 2 Logit classification -----------------------------------------------
#Divide the total data to keep only the wanted training data variables (total income, age, sex)
train_data <- total_table  %>% filter(sample=="train")  %>% select(ingtot , p6020, p6040, id, pobre, indigente)  %>% na.omit()

train_data <- train_data  %>% mutate(p6020 = factor(p6020,levels=c(0,1),labels=c("Woman","Men")),
                                     pobre = factor(pobre,levels=c(0,1),labels=c("No","Si")),
                                     indigente = factor(indigente,levels=c(0,1),labels=c("No","Si")))

#Logit regression
set.seed(123)

#Train the model with logit regression
logit <- train(
  pobre ~ p6020 + p6040 + (p6040*p6040),
  data = train_data,
  method = "glmnet",
  preProcess = NULL
)

#Construct the test data frame
test_data <- total_table  %>% filter(sample=="test") 

test_data <- test_data  %>% mutate(p6020 = factor(p6020,levels=c(0,1),labels=c("Woman","Men")),
                                     pobre = factor(pobre,levels=c(0,1),labels=c("No","Si")),
                                     indigente = factor(indigente,levels=c(0,1),labels=c("No","Si")))


#Predict total income with logit
pobre_insample <- predict(logit, train_data)
pobre_outsample <- predict(logit, test_data)

#For submission in kaggle
test_data<- test_data  %>% mutate(prob_hat=predict(logit,newdata = test_data, type = "prob")) #type = "prob" gives the predicted probabilities.
head(test_data  %>% select(id,prob_hat))

#Classification
rule <- 1/2 # Bayes Rule
test_data <-  test_data  %>% mutate(pobre_hat=ifelse(prob_hat>rule,1,0))    ## predicted class labels

head(test_data  %>% select(id,prob_hat,pobre_hat))

#Create the submission document by selecting only the variables required and renaming them to adjust to instructions
submit<-test_data  %>% select(id,pobre_hat)
submit <- submit  %>% rename(pobre_hat=pobre)
write.csv(submit,"Modelo1.csv",row.names=FALSE)

#Probabilities to calculate AUC
probs_insample <- predict(logit, train_data, type="prob")[, "Si", drop = T] #Nos interesa los que son pobres
probs_outsample <- predict(logit, test_data, type="prob")[, "Si", drop = T] #Nos interesa los que son pobres

#Accuracy
acc_insample <- Accuracy(y_pred = pobre_insample,       #Accuracy fuera de muestra 0.7998
                         y_true = train_data$pobre)

acc_outsample <- Accuracy(y_pred = pobre_outsample,     #Accueracy dentro de muestra NA
                         y_true = test_data$pobre)

#Precision
Precision(y_pred = pobre_insample, 
          y_true = as.factor(train_data$pobre),
          positive = "Si")      #NaN

Precision(y_pred = pobre_outsample, 
          y_true = as.factor(test_data$pobre),
          positive = "Si")      #NaN

#Recall
Recall(y_pred = pobre_insample, 
          y_true = as.factor(train_data$pobre),
          positive = "Si")      #0

Recall(y_pred = pobre_outsample, 
          y_true = as.factor(test_data$pobre),
          positive = "Si")      #NaN


## Modelo 3 Ada Boost ----------------------------------------------------------
#Divide the total data to keep only the wanted training data variables (total income, age, sex)
train_data <- total_table  %>% filter(sample=="train")  %>% select(ingtot , p6020, p6040, id, pobre, indigente)  %>% na.omit()

train_data <- train_data  %>% mutate(p6020 = factor(p6020,levels=c(0,1),labels=c("Woman","Men")),
                                     pobre = factor(pobre,levels=c(0,1),labels=c("No","Si")),
                                     indigente = factor(indigente,levels=c(0,1),labels=c("No","Si")))


#Ada Boost
set.seed(123)

ctrl<- trainControl(method = "cv",
                    number = 5,
                    classProbs = TRUE,
                    verbose=FALSE,
                    savePredictions = T)

# Check the class levels of 'pobre' variable
class_levels <- unique(train_data$pobre)

#Train the model with ada boost
ada_boost1 <- train(
  pobre ~ p6020 + p6040 + (p6040*p6040),
  data = train_data,
  method = "AdaBoost.M1",
  trControl = ctrl,
  tuneGrid  = expand.grid(
    mfinal = c(50, 100, 150),   #VER PARAMETROS MAS GRANDES
    maxdepth = c(1, 2, 3),
    coeflearn = c('Breiman', 'Freund'))
)

ada_boost1

#Construct the test data frame
test_data <- total_table  %>% filter(sample=="test")  

test_data <- test_data  %>% mutate(p6020 = factor(p6020,levels=c(0,1),labels=c("Woman","Men")),
                                   pobre = factor(pobre,levels=c(0,1),labels=c("No","Si")),
                                   indigente = factor(indigente,levels=c(0,1),labels=c("No","Si")))

#Predict poverty with ada boost
predictTest_ada <- data.frame(
  id = test_data$id,
  predict(ada_boost1, newdata = test_data,  type = "prob"),
  pobre = predict(ada_boost1, newdata = test_data, type = "raw"))

head(predictTest_ada %>% select(id, pobre))

#Accuracy
mean(predictTest_ada$id == predictTest_ada$pobre)


#Create the submission document by selecting only the variables required and renaming them to adjust to instructions
submit<-predictTest_ada  %>% select(id,pobre)
write.csv(submit,"Modelo3.csv",row.names=FALSE)


##Modelo 4: Árboles -------------------------------------------------------------
#Divide the total data to keep only the wanted training data variables (total income, age, sex)
train_data <- total_table  %>% filter(sample=="train")  %>% select(ingtot , p6020, p6040, id, pobre, indigente)  %>% na.omit()

train_data <- train_data  %>% mutate(p6020 = factor(p6020,levels=c(0,1),labels=c("Woman","Men")),
                                     pobre = factor(pobre,levels=c(0,1),labels=c("No","Si")),
                                     indigente = factor(indigente,levels=c(0,1),labels=c("No","Si")))


ctrl<- trainControl(method = "cv",
                    number = 5,
                    classProbs = TRUE,
                    verbose=FALSE,
                    savePredictions = T)

set.seed(123)

class_arboles <- train(pobre ~ p6020 + p6040 + (p6040*p6040),
                       data = train_data, 
                       method = "rpart",
                       trControl = ctrl,
                       tuneLength=100)

class_arboles

#Construct the test data frame
test_data <- total_table  %>% filter(sample=="test")  

test_data <- test_data  %>% mutate(p6020 = factor(p6020,levels=c(0,1),labels=c("Woman","Men")),
                                   pobre = factor(pobre,levels=c(0,1),labels=c("No","Si")),
                                   indigente = factor(indigente,levels=c(0,1),labels=c("No","Si")))

predictTest_arbol <- data.frame(
  id = test_data$id,                                              ## observed class labels
  predict(class_arboles, newdata = test_data, type = "prob"),         ## predicted class probabilities
  pobre = predict(class_arboles, newdata = test_data, type = "raw")    ## predicted class labels
)

head(predictTest_arbol)

#Accuracy
mean(predictTest_arbol$obs == predictTest_arbol$pred)

#Create the submission document by selecting only the variables required and renaming them to adjust to instructions
submit<-predictTest_arbol  %>% select(id,pobre)
write.csv(submit,"Modelo4.csv",row.names=FALSE)

## Modelo 5 Spatial Block Cost Complexity Prunning - Bagging -------------------
train_data <- total_table  %>% filter(sample=="train")  %>% select(ingtot , p6020, p6040, id, pobre, indigente)  %>% na.omit()

train_data <- train_data  %>% mutate(p6020 = factor(p6020,levels=c(0,1),labels=c("Woman","Men")),
                                     pobre = factor(pobre,levels=c(0,1),labels=c("No","Si")),
                                     indigente = factor(indigente,levels=c(0,1),labels=c("No","Si")))

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

test_data <- test_data  %>% mutate(p6020 = factor(p6020,levels=c(0,1),labels=c("Woman","Men")),
                                   pobre = factor(pobre,levels=c(0,1),labels=c("No","Si")),
                                   indigente = factor(indigente,levels=c(0,1),labels=c("No","Si")))

#Predict the tree with test data
test_data$pred_tree<-predict(tree_ranger,test_data)

head(test_data %>% select(id,pred_tree))

#Construct the dummy variables pobre & indigente
test_data$pobre <- ifelse(test_data$lp > test_data$pred_tree, 1, 0)

test_data$indigente <- ifelse(test_data$li > test_data$pred_tree, 1, 0)

head(test_data %>% select(id,pred_tree,pobre,indigente))

#Create the submission document by selecting only the variables required and renaming them to adjust to instructions
submit<-test_data  %>% select(id,pobre)
write.csv(submit,"Modelo5.csv",row.names=FALSE)

#MAE and MAPE train 
MAE(train_data$pred_tree, train_data$price)
MAPE(train_data$pred_tree, train_data$price)

#MAE
MAE(test_data$pred_tree, test_data$price)
#MAE V14: 2.496.469
MAPE(test_data$pred_tree, test_data$price)

### Model Comparison
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


## Modelo 6 Ada Boost ----------------------------------------------------------
#Divide the total data to keep only the wanted training data variables (total income, age, sex)
train_data <- total_table  %>% filter(sample=="train")  %>% select(ingtot , p6020, p6040, id, pobre, indigente)  %>% na.omit()

train_data <- train_data  %>% mutate(p6020 = factor(p6020,levels=c(0,1),labels=c("Woman","Men")),
                                     pobre = factor(pobre,levels=c(0,1),labels=c("No","Si")),
                                     indigente = factor(indigente,levels=c(0,1),labels=c("No","Si")))


#Ada Boost
set.seed(123)

ctrl<- trainControl(method = "cv",
                    number = 5,
                    classProbs = TRUE,
                    verbose=FALSE,
                    savePredictions = T)

# Check the class levels of 'pobre' variable
class_levels <- unique(train_data$pobre)

#Train the model with ada boost
ada_boost2 <- train(
  pobre ~ p6020 + p6040 + I(p6040^2),
  data = train_data,
  method = "AdaBoost.M1",
  trControl = ctrl,
  tuneGrid  = expand.grid(
    mfinal = c(50, 100, 150),   #VER PARAMETROS MAS GRANDES
    maxdepth = c(1, 2, 3),
    coeflearn = c('Breiman', 'Freund'))
)

ada_boost2

train_data$pobre<-predict(ada_boost2,train_data)

#Construct the test data frame
test_data<-total_table  %>% filter(sample=="test") 

test_data <- test_data  %>% mutate(p6020 = factor(p6020,levels=c(0,1),labels=c("Woman","Men")),
                                   pobre = factor(pobre,levels=c(0,1),labels=c("No","Si")),
                                   indigente = factor(indigente,levels=c(0,1),labels=c("No","Si")))

#Predict the tree with test data
test_data$pobre<-predict(ada_boost2,test_data)

head(test_data %>% select(id,pobre))

test_data <- test_data %>%
  mutate(pobre = as.character(pobre)) %>%
  mutate(pobre = case_when(
    pobre == "No" ~ "0",
    pobre == "Si" ~ "1",
    TRUE ~ pobre  # Keep the original value if it's not "No" or "Si"
  )) %>%
  mutate(pobre = as.factor(pobre))


#Create the submission document by selecting only the variables required and renaming them to adjust to instructions
submit<-test_data  %>% select(id,pobre)
write.csv(submit,"Modelo6.1.csv",row.names=FALSE)


#Modelo 7: Bosque 1 -------------------------------------------------------------
#Divide the total data to keep only the wanted training data variables (total income, age, sex)
train_data <- total_table  %>% filter(sample=="train")  %>% select(ingtot , p6020, p6040, id, pobre, indigente)  %>% na.omit()

train_data <- train_data  %>% mutate(p6020 = factor(p6020,levels=c(0,1),labels=c("Woman","Men")),
                                     pobre = factor(pobre,levels=c(0,1),labels=c("No","Si")),
                                     indigente = factor(indigente,levels=c(0,1),labels=c("No","Si")))

ctrl<- trainControl(method = "cv",
                    number = 5,
                    classProbs = TRUE,
                    verbose=FALSE,
                    savePredictions = T)


set.seed(123)

class_bosques <- train(
  pobre ~ p6020 + p6040 + (p6040*p6040),
  data=train_data,
  method = "ranger",
  trControl = ctrl,
  tuneGrid=expand.grid(
    mtry = c(1,2,3,4,5,6,7,8),
    splitrule = "gini",
    min.node.size = c(15,30,45,60))
)

class_bosques

#Construct the test data frame
test_data <- total_table  %>% filter(sample=="test")  

test_data <- test_data  %>% mutate(p6020 = factor(p6020,levels=c(0,1),labels=c("Woman","Men")),
                                   pobre = factor(pobre,levels=c(0,1),labels=c("No","Si")),
                                   indigente = factor(indigente,levels=c(0,1),labels=c("No","Si")))

predictTest_bosque <- data.frame(
  obs = test_data$pobre,                                    ## observed class labels
  predict(class_bosques, newdata = test_data, type = "prob"),         ## predicted class probabilities
  pred = predict(class_bosques, newdata = test_data, type = "raw")    ## predicted class labels
)

#Accuracy
mean(predictTest_arbol$obs == predictTest_arbol$pred)





