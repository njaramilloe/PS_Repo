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

table(geih_bog18_filtered$formal, geih_bog18_filtered$informal)

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



