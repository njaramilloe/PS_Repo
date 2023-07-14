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
       doSNOW, 
       leaps)


# Loading package
library(Ecdat)

# Structure of dataset in package
str(Hedonic)


#Make this example reproducible
set.seed(10101)

#use 70% of the dataset as a training set and 30% as a test set
sample <- sample(c(TRUE, FALSE), 
                 nrow(Hedonic), 
                 replace=TRUE, 
                 prob=c(0.7,0.3))
#head(sample)

#generate the partition
training_data  <- Hedonic[sample, ] #train sample 
testing_data   <- Hedonic[!sample, ] #test sample 

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

# Define your models
models <- c("lm", "glm", "rf", "svm")

# Define the variables to iterate over
variables <- c("mv", "crim", "zn", "indus", "chas", "nox", "rm", "tax", "dis", 
               "rad", "ptratio", "blacks", "lstat")  # Update with your variable names

# Iterate over the models, variables, and polynomial degrees
model_spec <- lm(age~., data=training_data)
testing_data$model_spec<-predict(model_spec, newdata = testing_data)
RMS <- with(testing_data,mean((age-model_spec)^2))


# Create a data frame to store the results
results <- data.frame(Model = character(),
                      Variables = character(),
                      RMSE = numeric(),
                      stringsAsFactors = FALSE)
--------------------------------------------------------------------------------
  # Set up the control parameters for model training
  ctrl <- trainControl(method = "cv",       # Cross-validation method
                       number = 10,         # Number of folds
                       verboseIter = TRUE,  # Print progress during model training
                       savePredictions = TRUE)  # Save predictions for each resample

# Define the formula for the model
formula <- Species ~ .

# Train the model using the defined control parameters
model <- train(formula,
               data = your_data,
               method = "rf",            # Random Forest model
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
      formula <- as.formula(paste("age ~ (", paste(variables, collapse = " + "), ")^", degree, sep = ""))
      print(formula)
      
      ctrl <- train(formula, data = training_data, method = "leapBackward")
    }
  }
}

# Set up the control parameters for model training
ctrl <- trainControl(method = "cv",  # Cross-validation method
                     number = 1,               # Number of folds
                     verboseIter = TRUE,
                     allowParallel = TRUE)     # Enable parallel processing

warnings()
      # Create the model specification using the set approach
      model_spec <- train(formula,
                          data = Hedonic,  # Update with your data
                          method = "leapBackward",
                          trControl = ctrl,
                          tuneGrid = data.frame(nvmax = degree))
      
      # Extract the selected variables from the subset selection
      selected_vars <- colnames(coef(model_spec))
      
      # Update the formula with selected variables
      updated_formula <- as.formula(paste("age ~", paste(selected_vars, collapse = " + "), sep = ""))
      
      # Create the final model using the selected variables
      final_model <- train(updated_formula,
                           data = Hedonic,  # Update with your data
                           method = model,
                           trControl = ctrl)
      
      # Extract the model performance metrics (e.g., RMSE)
      performance <- final_model$results$RMSE[1]
      
      # Store the results in the data frame
      results <- rbind(results, data.frame(Model = model, Variables = selected_vars, RMSE = performance))


# Stop the parallel backend
stopCluster(cl)

# View the results
print(results)
