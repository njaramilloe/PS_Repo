rm(list = ls())

library(pacman)
library(ggplot2)
library(dplyr)
p_load(rvest, tidyverse, ggplot2, rio, skimr, caret)


# PASO 1: CARGAR LOS DATOS ---------------------------------------------------- 

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
geihbog18<-  geih_bog18 %>% filter(age > 17)

## Mantener la memoria limpia en caso de requerir capacidad para el procesamiento de datos
rm(list = "url", i, "tabla", data_chunk)



#Bind Manual
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

#Estadísticas descriptivas
p_load(stargazer)

sum <- summary(tabla_limpia)
stargazer(sum, type = "text", omit.stat = c("ser", "f", "adj.rsq")) #NO FUNCIONOA

est_des  <- skim(tabla_limpia)
stargazer(est_des, type = "text", omit.stat = c("ser", "f", "adj.rsq")) #SALE VACIA
stargazer(tabla_limpia, type = "latex", summary = T, omit.stat = c("ser", "f", "adj.rsq")) #SALE VACIA

#Regresión : log(wage) = b1 + b2(age) + b3(age)^2 + u
tabla_limpia$age2 <- tabla_limpia$age^2
reg1 <- lm(log(ingtot)  ~ age + age2, tabla_limpia)

?lm

reg1 <- lm(log(tabla_limpia$ingtot)  ~ tabla_limpia$age + tabla_limpia$age2)
summary(reg1)


#Loop (REVISAR) 
## Lo que Ignacio nos envió
readHTML<-function(page_numb){
  x<-read_html(paste0("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_",page_numb,".html")) 
  #Faltan Pasos para obtener la tabla (completar ustedes)
  x %>% html_table() %>% as.data.frame()
}

db_list<-lapply(1:10,readHTML)  #itera sobre las páginas y retorna una lista con 10 elementos
db<-do.call(rbind,db_list) #une todo en un data.frame

## Mi intento



tabla_loop <-  data.frame()
for (PS1 in seq(from = 1, to = 10, by = 1)){
  url <- paste0("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_",PS1,".html")
  total_data <- read_html(url)
}

tabla_loop =  rbind(tabla_loop, data.frame())

print(paste("Data_Chunk:",PS1))

view(tabla_loop)
view(GEIH_BOG18)