################################################################################
# Problem Set 1 - Question 5
# Authors: Luis Carlos Ricaurte
################################################################################

# 0. Set up --------------------------------------------------------------------
#Load packages, and define the working directory.

# 0.1 Clean workspace
rm(list = ls())

# 0.2 Load packages 
require(pacman)
p_load(rio, # import/export data
       tidyverse, # tidy-data
       skimr, # summary data
       caret, # Classification And REgression Training
       rvest, 
       tidyverse, 
       dplyr, 
       ggplot2,
       rstudioapi, 
       stargazer,
       parallel,
       doSNOW)

# 0.3 Define directories.
path_script <- rstudioapi::getActiveDocumentContext()$path
path_folder <- dirname(path_script)
setwd(path_folder)

# 0.4. Load Data------------------------------------------------------------------
#'Load GEIH 2018. 

geih_bog18_filtered <- read.delim("../stores/geihbog18_filtered.txt",
                                  header = TRUE, 
                                  sep = ";", 
                                  dec = ".")
names(geih_bog18_filtered)

#5.a Split the sample into two--------------------------------------------------
#Make this example reproducible
set.seed(10101)

#use 70% of the dataset as a training set and 30% as a test set
sample <- sample(c(TRUE, FALSE), 
                 nrow(geih_bog18_filtered), 
                 replace=TRUE, 
                 prob=c(0.7,0.3))
head(sample)

#generate the partition
training_data  <- geih_bog18_filtered[sample, ] #train sample 
test_data   <- geih_bog18_filtered[!sample, ] #test sample 

# Calculating the proportion of each sample 
dim(training_data)/dim(geih_bog18_filtered)
dim(test_data)/dim(geih_bog18_filtered)

--------------------------------------------------------------------------------
# Set the number of cores to use to running the models
n_cores <- detectCores()
cl <- makeCluster(ceiling(n_cores/2)) 

# Register the parallel backend. Here you can use "registerDoParallel(cl)"
registerDoSNOW(cl)

#' WARNING: Another way of speed up the process is using doParallel package and 
#' "registerDoParallel()" command to specify the number of cores 
#' for parallel processing. But doParallel package is only available for the 
#' latest version of Rstudio. So update it!!

--------------------------------------------------------------------------------

#' Define your models: Linear moldel, generalized linear models
#' random forest and support vector machine

models <- c("lm", "glm", "rf", "svm")


# Define the variables to iterate over
variables <- c("age",	"sex",	"oficio",	"relab",	"college",	"ocu",	"p6210s1",	
               "clase",	"cotPension",	"cuentaPropia",	"depto",	"directorio",	
               "dominio",	"dsi",	"estrato1",	"formal",	"inac",	"informal",	
               "maxEducLevel",	"mes",	"pea", "pet", "sizeFirm",	"wap",	
               "regSalud",	"age2")

# Specify the maximum degree of polynomial terms
max_degree <- 5  

# Create a data frame to store the results
results <- data.frame(Model = character(),
                      Variable = character(),
                      RMSE = numeric(),
                      stringsAsFactors = FALSE)

?trainControl


# Iterate over the models, variables, and polynomial degrees
for (model in models) {
  
  # Create the model specification using the set approach
  model_spec <- regsubsets(ln_wage ~ age + age2 + sex,
                           data = geih_bog18_filtered,
                           subset = training_data,
                           method = model)
}

  # Extract the model performance metrics (e.g., RMSE)
  performance <- model_spec$results$RMSE[1]
  
  # Store the results in the data frame
  results <- rbind(results, data.frame(Model = model, 
                                       Variable = variable, 
                                       Degree = degree, 
                                       RMSE = performance))  

--------------------------------------------------------------------------------
  # Set up the control parameters for model training
  ctrl <- trainControl(method = "cv",       # Cross-validation method
                       number = 10,         # Number of folds
                       verboseIter = TRUE,  # Print progress during model training
                       savePredictions = TRUE)  # Save predictions for each resample

# Define the formula for the model
formula <- ln_wage ~ "age"

# Train the model using the defined control parameters
model <- train(formula,
               data = geih_bog18_filtered,
               method = "lm",            # Random Forest model
               trControl = ctrl)

# View the results
print(model)
--------------------------------------------------------------------------------

# Iterate over the models, variables, and polynomial degrees
for (model in models) {
  for (variable in variables) {
    for (degree in 1:max_degree) {
      # Create the polynomial terms
      poly_terms <- paste(variable, "^", degree, sep = "")
      
      # Create the formula with polynomial terms and interactions
      formula <- paste("outcome ~ (", paste(variables, collapse = " + "), ")^", 
                       degree, sep = "")
      
      # Set up the control parameters for model training
      ctrl <- trainControl(method = "repeatedcv",  # Cross-validation method
                           number = 5,               # Number of folds
                           repeats = 3,              # Number of repetitions
                           verboseIter = TRUE,
                           allowParallel = TRUE)     # Enable parallel processing
      
      # Create the model specification using the set approach
      model_spec <- train(formula,
                          data = your_data,  # Update with your data
                          method = model,
                          trControl = ctrl)
      
      # Extract the model performance metrics (e.g., RMSE)
      performance <- model_spec$results$RMSE[1]
      
      # Store the results in the data frame
      results <- rbind(results, data.frame(Model = model, 
                                           Variable = variable, 
                                           Degree = degree, 
                                           RMSE = performance))
    }
  }
}

# Stop the parallel backend
stopCluster(cl)

# View the results
print(results)

------------------------------------------------------------------------------

  
# Estimate the model with training sample
model1<-lm(ln_wage ~ age + age2, data=train)
summary(model1)


# Evaluate the models by predicting the wages using testing sample
test$model1<-predict(model1, newdata = test)



inTrain <- createDataPartition(
  y = matchdata$price,## La variable dependiente u objetivo
  p = .7, ## Usamos 70%  de los datos en el conjunto de entrenamiento 
  list = FALSE)

training <- matchdata[ inTrain,]
testing  <- matchdata[-inTrain,]

nrow(training)


