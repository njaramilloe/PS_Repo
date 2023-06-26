################################################################################
# Problem Set 1 - Question 4
# Authors: Mariana Bonet De Vivero
################################################################################

# 0. Set up --------------------------------------------------------------------
#'This section aims to clean the workspace, load packages, and define the 
#'directories needed for running the code.

# 0.1 Clean workspace
rm(list = ls())

# 0.2 Load packages 
library(pacman)
library(ggplot2) #estos son necesarios si ya estan en el p_load? 
library(dplyr)
p_load(rvest, tidyverse, ggplot2, rio, skimr, caret, rstudioapi, stargazer)

# 0.3 Define directories
path_script <- rstudioapi::getActiveDocumentContext()$path
path_folder <- dirname(path_script)
setwd(path_folder)
print(path_folder)
# 1. Load Data------------------------------------------------------------------
#'We call our store dataframe that has been previously prepped and cleaned. This
#'dataframe has the information of the GEIH 2018. 
geihbog18_filtered <- read.delim(file="C:/Users/maria/OneDrive - Universidad de los andes/MECA/BIG DATA/Repositorios/PS_Repo/stores/geihbog18_filtered.txt", header=TRUE, sep = ";")

#View variables in our dataset
str(geihbog18_filtered)

#Understand whether variables classes are numerical or categorical
skim(geihbog18_filtered)

# 2. Estimate the unconditional wage gap (4A) ----------------------------------
#' In this section, we estimate the unconditional wage gap, which is 
#' log(w) = β1 + β2Female + u , where Female is an indicator that takes the value
#' of one if the individual in the sample identifies as female.

  #2.1 First, we create a variable that converts the wage variable to log(wage)
  geihbog18_filtered <- geihbog18_filtered  %>% 
  mutate(ln_wage = log(y_total_m_ha)) %>%
  mutate (gender = ifelse(geihbog18_filtered$sex == 0, 1, 0))

  #2.2 Now, we can estimate the unconditional wage gap
  reg4<- lm(geihbog18_filtered$ln_wage  ~ geihbog18_filtered$gender, geihbog18_filtered)
  summary(reg4)
  
  #2.3 Generate the LaTeX code using the stargazer function and store it in a variable
  regression_table <- stargazer(reg4, title = "Unconditional wage gap regression results", align = TRUE, omit.stat = c("ser", "f", "adj.rsq"))
  
  #2.4  
  gender_colors <- c("Female"= "blue", "Male" = "red")
  geihbog18_filtered$gender_label <- ifelse(geihbog18_filtered$gender == 0, "Male", "Female")
  ggplot(geihbog18_filtered) +
    geom_density(aes(x = ln_wage, color = factor(gender_label)), alpha = 0.5) +
    scale_color_manual(values = gender_colors) +
    labs(x = "Income", y = "Density", fill = "Gender") +
    ggtitle("Income Distribution by Gender") +
    theme_bw()
     
  ?ggplot       
# 3. Estimate the conditional wage gap (4B) ------------------------------------
#' In this section, we will estimate the conditional wage gap which we have 
#' defined as log(w) = β1 + β2Female + β3formal + β4relab + β5college 
#' + β7oficio + u , where 
        #' Female is an indicator that takes the value of one if the individual 
        #' in the sample identifies as female.
        #' formal is an indicator that takes the value of one if the individual 
        #' in the sample is a formal worker
        #'
        #'
        #'
  
  # 3.1 We first estimate the conditional regression using standard lm
  standardreg <- lm(ln_wage ~ gender + formal + relab + college + oficio, data 
                     = geihbog18_filtered, x = TRUE)
  residuals_sr <- residuals(standardreg)
  
  #3.2 Now, we will estimate using FWL. We regress gender on the rest of the 
  #'variables and store the residuals. Next, we regress ln wage on the rest of 
  #'the variables, excluding gender, and store the residuals. Finally, regress 
  #'the residuals of the second regression on the residuals of the first 
  #'regression. (4b.i)
  geihbog18_filtered<-geihbog18_filtered %>% mutate(genderResidF=lm(gender ~ formal + relab + college + oficio,geihbog18_filtered)$residuals) #Residuals of gender~rest of xi
  geihbog18_filtered<-geihbog18_filtered %>% mutate(wageResidF=lm(ln_wage ~ formal + relab + college + oficio,geihbog18_filtered)$residuals) #Residuals of gender~rest of xi
  regFWL<-lm(wageResidF~genderResidF,geihbog18_filtered)
  
  #3.3 We compare the results of the standard regression and the regression 
  #using the FWL theorem. 
  stargazer(standardreg,regFWL,type="text",digits=7)
  
  #3.4 We take a look at SS residuals and the standard errors, after correcting
  #the degrees of freedom of the FWL regression.
  sum(resid(standardreg)^2)
  sum(resid(regFWL)^2)
  
 # sqrt(diag(vcov(standardreg))*(13513/13517))[2]
 # sqrt(diag(vcov(regFWL)))[4]
  
  #3.5 We will proceed to estimate the conditional wage gap using FWL and 
  #'bootstrap.We create function to be used during the boot command.  
  
  fwl_regression <- function(geihbog18_filtered, indices) {
    # Subset the data based on the bootstrap indices
    bootstrap_data <- geihbog18_filtered[indices, ]
    # Estimate the residuals of gender~rest of xi
    gender_resid <- residuals(lm(gender ~ formal + relab + college + oficio, 
                                 data = bootstrap_data))
    # Estimate the residuals of ln_wage~rest of xi
    wage_resid <- residuals(lm(ln_wage ~ formal + relab + college + oficio, 
                               data = bootstrap_data))
    # Perform the FWL regression using the residuals
    fwl_model <- lm(wage_resid ~ gender_resid, data = bootstrap_data)
    # Return the estimated coefficient for the model
    return(coef(fwl_model))
  }
  #We make sure the function works
  fwl_regression(geihbog18_filtered, 1:13519)
 
 #3.6 We perform the bootstrap estimation based on the FWL specification 
  #'(4b.ii). We store the results of the coefficients and calculate the standard
  #' errors manually. Finally, we specify and obtain the confidence intervals. 
 
  boot_results <- boot(data = geihbog18_filtered, statistic = fwl_regression, R = 1000)
  boot_results
  estimated_coefs <- boot_results$t0
  estimated_coefs
  sqrt(var(estimated_coefs))
  
  confidence_intervals <- boot.ci(boot_results, type = "perc")
  confidence_intervals_95 <- confidence_intervals$percent[, 4]
  confidence_intervals
  confidence_intervals_95
  
  #3.7 We will export the results of the three regressions in this point.
  boot_model <- lm(estimated_coefs ~ 1)
  coef_names <- c("Intercept", "genderResidF")
  stargazer(boot_results$t0*, type="text")
  stargazer(standardreg,regFWL,boot_model,type="text",digits=7, coef.var=coef_names)
  regression_table <- stargazer(standardreg,regFWL,estimated_coefs, title = "Unconditional and conditional wage gap regression results", align = TRUE, omit.stat = c("ser", "f", "adj.rsq"))
  
#################################################################
  # Extract the coefficient estimates from the models
  coef_standardreg <- coef(standardreg)
  coef_regFWL <- coef(regFWL)
  coef_boot_model <-estimated_coefs
  
  # Create a matrix with the coefficient estimates
  coef_matrix <- matrix(c(coef_standardreg, coef_regFWL, coef_boot_model), nrow = 3, byrow = TRUE)
  coef(regFWL)
  # Create a character vector with the row names
  row_names <- c("Standard Regression", "FWL Regression", "Bootstrap")
  
  # Create a data frame with the coefficient matrix and row names
  coef_df <- data.frame(coef_matrix, row.names = row_names)
  
  # Rename the columns
  colnames(coef_df) <- coef_names
  
  # Print the coefficient table
  print(coef_df)
  _______
  
  class(standardreg)
  class(regFWL)
  class(boot_results)
  
#4 Age & gender wage analysis ##################################################
#' Now, we will plot the predicted age-wage profile and estimate the implied 
#' “peak ages” with the respective confidence intervals by gender.
