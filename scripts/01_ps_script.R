rm(list = ls())


install.packages("DT")
install.packages("VIM")
library(pacman)
library(ggplot2)
library(dplyr)
library(VIM)
p_load(rvest, tidyverse, ggplot2, rio, skimr, caret, stargazer,expss)




# CARGAR LOS DATOS ---------------------------------------------------- 

# Realizar un loop para adjuntar los 10 archivos en los cuales se encuentra 
# divida la base de datos GEIH de Bogotá 2018"

geih_bog18 <- data.frame()

for (i in 1:10) {
   url <- paste0("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_",i,".html")
   data_chunk <- read_html(url)
   tabla <- data_chunk %>%
     html_table()
   geih_bog18 <- bind_rows(tabla, geih_bog18)
   print("BD", i)
}
geihbog18<-geih_bog18 %>% filter(age > 17)

view(geihbog18)

str(geihbog18)

## Mantener la memoria limpia en caso de requerir capacidad para el procesamiento de datos
rm(list = "url", i, "tabla", data_chunk)

#Estadísticas descriptivas para determinar la y
as.datatable_widget(geihbog18 %>%
                      tab_cells(y_total_m_ha, y_ingLab_m_ha) %>%
                      tab_cols(total(), cuentaPropia) %>%
                      tab_stat_mean(label = "Media") %>%
                      tab_stat_sd(label = "Desviación") %>% 
                      tab_pivot())

#REVISAR POR CEROS
lapply(geihbog18, function(x){ length(which(x==0))/length(x)})

#REVISAR POR MISSING VALUES ----------------------------------------------------
#Seleccionamos algunas variables relevantes para hacer más fácil la interpretación de las gráficas

# Keep a couple  of predictors
geihbog18_selected <- geihbog18  %>% select(y_total_m_ha, 
                                            hoursWorkUsual,
                                            age,
                                            sex,
                                            oficio,
                                            relab,
                                            college,
                                            ocu,
                                            p6210s1)

#Ahora si, revisar por missing values
missing_values <- is.na(geihbog18_selected)

mice_plot <- aggr(geihbog18_selected, col=c('navyblue','yellow'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(geihbog18_selected), cex.axis=.7,
                  gap=3, ylab=c("Missing data","Pattern"))

#Eliminar missings de la columna de horas trabajadas por semana
geihbog18_filtered <- na.omit(geihbog18, cols="hoursWorkUsual")
geihbog18_selected <- na.omit(geihbog18, cols="hoursWorkUsual")

#Generar nuevas variables
##Create the age square and the log(wage) variables
geihbog18_filtered <- geihbog18_filtered  %>% mutate(age2=age^2)
geihbog18_filtered <- geihbog18_filtered  %>% mutate(ln_wage = log(y_total_m_ha))


#Exportar base de datos
write.table(geihbog18_filtered, file = "/Users/nataliajaramillo/Documents/GitHub/PS_Repo/stores/geihbog18_filtered.txt", sep = ";",
            row.names = TRUE)




#ESTADÍSTICAS DESCRIPTIVAS ----------------------------------------------------
#Descriptive statistics
summary_table <- stargazer(data.frame(geihbog18_selected), title = "Variables Included in the Selected Data Set", align = TRUE, omit.stat = c("n"))

#Export descriptive analysis of selected variables in latex
writeLines(summary_table, "/Users/nataliajaramillo/Documents/GitHub/PS_Repo/stores/summary_table.tex")








#REGRESIÓN : log(wage) = b1 + b2(age) + b3(age)^2 + u -------------------------
#Regress
reg_age <- lm(ln_wage ~ age + age2, geihbog18_filtered)

#Generate the LaTeX code using the stargazer function and store it in a variable
regression_table <- stargazer(reg_age, title = "Regression Results", align = TRUE, omit.stat = c("ser", "f", "adj.rsq"))




#BOOTSTRAP to construct the confidence intervals -------------------------------
p_load("boot")

#Define a function that will extract the coefficients from the model based on bootstrap samples
get_coefficients <- function(geihbog18_filtered, indices) {
  fit <- lm(ln_wage ~ age + age2, data = geihbog18_filtered[indices, ])
  return(coef(fit))
}

#Use the boot function to perform the bootstrap procedure and calculate the confidence intervals
##Set stype = "i" to obtain the percentile intervals
boot_results <- boot(data = geihbog18_filtered, statistic = get_coefficients, R = 1000)
confidence_intervals <- boot.ci(boot_results, type = "perc")

#Obtain the confidence intervals
confidence_intervals_95 <- confidence_intervals$percent[, 4]

#Plot of the estimated age-earnings profile
geihbog18_filtered <- geihbog18_filtered  %>% mutate(yhat=predict(reg_age))

summ = geihbog18_filtered %>%  
  group_by(
    age, age2
  ) %>%  
  summarize(
    mean_y = mean(ln_wage),
    yhat_reg = mean(yhat), .groups="drop"
  ) 


ggplot(summ) + 
  geom_line(
    aes(x = age, y = yhat_reg), 
    color = "green", size = 1.5
  ) + 
  labs(
    title = "log Wages by Age in the GEIH 2018",
    x = "Age",
    y = "log Wages"
  ) +
  theme_bw()


#ALTERNATIVA: Bind Manual ---------------------------------------------------- 
url1 <- "https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_1.html"
browseURL(url1)

data_chunk_1 <- read_html(url1)
data_chunk_1

tabla_1 <- data_chunk_1 %>%
  html_table() %>% .[[1]]


url2 <- "https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_2.html"

data_chunk_2 <- read_html(url2)
data_chunk_2

tabla_2 <- data_chunk_2 %>%
  html_table() %>% .[[1]]


url3 <- "https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_3.html"

data_chunk_3 <- read_html(url3)
data_chunk_3

tabla_3 <- data_chunk_3 %>%
  html_table() %>% .[[1]]


url4 <- "https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_4.html"

data_chunk_4 <- read_html(url4)
data_chunk_4

tabla_4 <- data_chunk_4 %>%
  html_table() %>% .[[1]]



url5 <- "https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_5.html"

data_chunk_5 <- read_html(url5)
data_chunk_5

tabla_5 <- data_chunk_5 %>%
  html_table() %>% .[[1]]



url6 <- "https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_6.html"

data_chunk_6 <- read_html(url6)
data_chunk_6

tabla_6 <- data_chunk_6 %>%
  html_table() %>% .[[1]]



url7 <- "https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_7.html"

data_chunk_7 <- read_html(url7)
data_chunk_7

tabla_7 <- data_chunk_7 %>%
  html_table() %>% .[[1]]


url8 <- "https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_3.html"

data_chunk_8 <- read_html(url8)
data_chunk_8

tabla_8 <- data_chunk_8 %>%
  html_table() %>% .[[1]]



url9 <- "https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_9.html"

data_chunk_9 <- read_html(url9)
data_chunk_9

tabla_9 <- data_chunk_9 %>%
  html_table() %>% .[[1]]



url10 <- "https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_3.html"

data_chunk_10 <- read_html(url10)
data_chunk_10

tabla_10 <- data_chunk_10 %>%
  html_table() %>% .[[1]]

tabla <- bind_rows(bind_rows(bind_rows(bind_rows(bind_rows(bind_rows(bind_rows(bind_rows(tabla_1, tabla_2), tabla_3), tabla_5), tabla_6), tabla_7), tabla_8), tabla_9), tabla_10)

#Filtrar por edad
tabla_limpia <-  tabla %>% filter(age > 17)

