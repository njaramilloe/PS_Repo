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
p_load(rvest, tidyverse, ggplot2, rio, skimr, caret, rstudioapi)

# 0.3 Define directories
path_script <- rstudioapi::getActiveDocumentContext()$path
path_folder <- dirname(path_script)
setwd(path_folder)

# 1. Load Data------------------------------------------------------------------
#'We call our store dataframe that has been previously prepped and cleaned. This
#'dataframe has the information of the GEIH 2018. 

#Posibles nombres de las df
geih_bog18
tabla_loop
GEIH_BOG18

#'Comando utilizado por Lucas en clase, revisar si con un archivo local se puede
#'utilizar.

df <- import("https://github.com/ignaciomsarmiento/datasets/blob/main/GEIH_sample1.Rds?raw=true")

#Plan B
load("path/to/your/file.RData")

#Plan C = el más probable que termine usando
genddata <- readRDS("path/to/your/file.rds")

#View variables in our dataset
str(genddata)

#Understand whether variables classes are numerical or categorical
skim(genddata)

# 2. Estimate the unconditional wage gap----------------------------------------
#' In this section, we estimate the unconditional wage gap, which is 
#' log(w) = β1 + β2Female + u , where Female is an indicator that takes the value
#' of one if the individual in the sample identifies as female.

  #Simulando 01_ps_script
      reg4<- lm(log(genddata$ingtot)  ~ genddata$female, genddata)
      summary(reg4)
      
  #Simulando Cuaderno del Modulo 2
      
    # 2.1 We first select the variables used in the analysis (X)
      gen1.explanatory = c('female')
      x <- genddata %>% select(gen1.explanatory)
      
    # 2.2 We then select the dependent variable
      y <- select(ingtot) %>%
            mutate() %>%
            rename(y=ingtot)
    
    # 2.3 Next, we construct the database and check variables classes
      dat = cbind(x, y)
      skim(dat)
     
    # 2.4 
      reg4 <- lm(y ~ ., data = dat, x = TRUE)
      summary(reg4)
#para que sirve x=TRUE
      
      



