rm(list = ls())

library(pacman)
library(stringr)
library(stringi)
p_load(rvest, tidyverse, stringr, stringi, sf, leaflet, tmaptools, ggplot2, rio, skimr, caret, stargazer,expss, boot)

# Set working directory
setwd("/Users/nataliajaramillo/Documents/GitHub/Taller_2/stores")


#LOAD DATA --------------------------------------------------------------------------------------------------------------------------------------
#Load training data
train <- read.csv("train.csv")

#Load test data
test <- read.csv("test.csv")

#Glimpse into the data bases
head(test)
head(train)

test<-test %>% mutate(sample="test")
train<-train %>% mutate(sample="train")

total_table<-rbind(test,train)
table(total_table$sample) #test 10286 | train 38644

#Understand the data ----------------------------------------------------------------------------------------------------------------------------
#DESCRIPTIVE STATISTICS
glimpse(total_table)
head(total_table[1:15])

#Check for string inconsistencies
unique(total_table$property_type)

#Check for missing values
road(colSums(is.na(total_table))/nrow(total_table)*100,1) #REVISAR VIDEO DE LUCAS

# Check the levels of each variable
variable_levels <- sapply(total_table, function(x) length(unique(x)))
variable_levels

# Identify variables with only one level
single_level_vars <- names(variable_levels[variable_levels == 1])
single_level_vars

#Load geographical data -------------------------------------------------------------------------------------------------------------------------
total_table <- st_as_sf(
  total_table, 
  coords = c("lon","lat"), # "coords" is in x/y order -- so longitude goes first
  crs = 4326  # Set our coordinate reference system to EPSG:4326,
              # the standard WGS84 geodetic coordinate reference system
)


palette <- colorFactor(
  palette = c('red', 'green'),
  domain = total_table$sample 
)

map<-leaflet() %>%
  addTiles() %>% #capa base
  addCircles(data=total_table,col=~pal(sample)) #capa casas
map


## Distance to CBD -------------------------------------------------------------------------------------------------------------------------------
cbd <- geocode_OSM("Centro Internacional, Bogotá", as.sf=T)
cbd

total_table$DCBD <- st_distance(x = total_table, y=cbd)

head(total_table$DCBD)

total_table %>% st_drop_geometry() %>% group_by(sample) %>% summarize(mean(DCBD))

train_data<-total_table  %>% filter(sample=="train")  %>% select(price,DCBD)  %>% na.omit()

set.seed(123)
tree <- train(
  log(price) ~    DCBD,
  data=train_data,
  method = "rpart",
  trControl = fitControl,
  tuneLength=10
)

test_data<-total_table  %>% filter(sample=="test")  
test_data$pred_tree<-predict(tree,test_data)

head(test_data  %>% select(property_id,pred_tree))

test_data <- test_data   %>% st_drop_geometry()  %>% mutate(pred_tree=exp(pred_tree))
head(test_data  %>% select(property_id,pred_tree))

submit<-test_data  %>% select(property_id,pred_tree)
submit <- submit  %>% rename(price=pred_tree)
write.csv(submit,"Tree_v1.csv",row.names=FALSE)


#Predicting prices ------------------------------------------------------------------------------------------------------------------------------
lm_model<- lm(price ~ surface_total + surface_covered + rooms + bedrooms + bathrooms + property_type, data = train)
summary(lm_model)


#LOAD DATA ABOUT LOCALITIES AND NEIGHBORHOODS OF BOGOTA -----------------------------------------------------------------------------------------
localidades_url<-"https://es.wikipedia.org/wiki/Anexo:Barrios_de_Bogot%C3%A1"
localidades<-read_html(localidades_url)
localidades <- localidades %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div[1]/table') %>%
  html_table %>%
  as.data.frame()

#Create new column with extracted values
localidades$id_localidad<-substr(localidades$Localidad,1,2)
localidades$localidad<-str_sub(string = localidades$Localidad, start=4)

#Drop column Localidad
localidades <- select(localidades, -Localidad)

#Convert to lowercase
localidades<-tolower(stri_trans_general(localidades,"Latin-ASCII")) #se daña



