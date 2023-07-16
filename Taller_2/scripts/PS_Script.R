rm(list = ls())

library(pacman)

p_load(rvest, 
       tidyverse, #data manipulation and visualization
       stringr, #manipulate and analyze character data
       stringi, #manipulate string/text data in the cleaning process
       sf, #analyze spacial data
       leaflet, #create interactive web maps and visualizations
       tmaptools, #support spatial data manipulation and analysis
       rgeos, #to calculate the polygons
       tmaptools, #to extract information from open streetmaps
       osmdata, #to extract information from open streetmaps
       caret, #Classification And REgression Training 
       vtable, #output a descriptive variable table
       spatialsample, #spatial resampling
       ggplot2, #data visualization
       rio, #import/export data file formats
       skimr, #summary data
       stargazer,#generate publication-quality tables
       expss, #functions from spreadsheets and SPSS Statistics software
       plyr, #round_any function
       glmnet, #para spatial correlation
       ranger, #para realizar bosques
       bst, #para realizar bosques con boosting
       parallel,
       doParallel,
       xtable, #to export to latex
       forecast, 
)

#Assign Cores
detectCores() #8
registerDoParallel(6)


# set working directory
path_script <- rstudioapi::getActiveDocumentContext()$path
path_folder <- dirname(path_script)
setwd(path_folder)
setwd("../stores")


#LOAD DATA --------------------------------------------------------------------------------------------------------------------------------------
#Load training data
total_table <- read.csv("properties_joined.csv")
#total_table<- load("merged_data.RData")
#Glimpse into the data base
head(total_table)
table(total_table$sample) #test 10286 | train 38644
colnames(total_table) <- tolower(colnames(total_table))
names(total_table)
"geometry.1"<-NULL
#Understand the data ----------------------------------------------------------------------------------------------------------------------------
#DESCRIPTIVE STATISTICS
glimpse(total_table)
head(total_table[1:15])

#Check for string inconsistencies
unique(total_table$property_type)
unique(total_table$operation_type)

#Check for missing values
missings<-data.frame(colSums(is.na(total_table))/nrow(total_table)*100)

#export to latex
?xtable
missings <- xtable(missings, caption = "Percentage of missing values per variable", colnames = c("Variable","Percentage of Missing Values"))
# Export the table to a LaTeX file
#print.xtable(missings, file = "/Users/nataliajaramillo/Documents/GitHub/PS_Repo/Taller_2/stores/missings.tex", floating = FALSE)

# Check the levels of each variable
variable_levels <- sapply(total_table, function(x) length(unique(x)))
variable_levels

# Identify variables with only one level
single_level_vars <- names(variable_levels[variable_levels == 1])
single_level_vars

#Replace missings with mode in bathroom (25.67% of observations are missings)
Mode <- function(x) {
ux <- unique(x)
ux[which.max(tabulate(match(x, ux)))]
}

total_table$bathrooms[is.na(total_table$bathrooms)] <- Mode(total_table$bathrooms) 

#Vtable statistics
total_table_selected<- total_table %>% select(bedrooms, bathrooms, surface_covered, neighborhood, price, neighborhood)
sum<-data.frame(sumtable(total_table_selected, out = "return"))

#export to latex
sum <- xtable(sum)
# Export the table to a LaTeX file
#print.xtable(sum, file = "/Users/nataliajaramillo/Documents/GitHub/PS_Repo/Taller_2/stores/sumtable.tex", floating = FALSE)


#Drop anormal values
# Create a data frame
total_table_cleaned <- total_table[total_table$surface_covered < 1206 & total_table$surface_covered >= 30, ]
rownames(total_table_cleaned) <- NULL
total_table_cleaned <- total_table_cleaned[complete.cases(total_table_cleaned$property_id), ]

view(total_table_cleaned)

#Vtable statistics
total_table_cleaned_selected<- total_table_cleaned %>% select(bedrooms, bathrooms, surface_covered, neighborhood, price)
sum_cleaned<-data.frame(sumtable(total_table_cleaned_selected, out = "return"))
#export to latex
sum_cleaned <- xtable(sum_cleaned)
# Export the table to a LaTeX file
print.xtable(sum_cleaned, file = "/Users/nataliajaramillo/Documents/GitHub/PS_Repo/Taller_2/stores/sumtable_cleaned.tex", floating = FALSE)

#Load the total sample as geographical data --------------------------------------------------------------------------------------------------------
#Adjust for spatial dependence
total_table <- st_as_sf(
  total_table, 
  coords = c("lon","lat"), # "coords" is in x/y order -- so longitude goes first
  crs = 4326  # Set our coordinate reference system to EPSG:4326,
  # the standard WGS84 geodetic coordinate reference system
)

#Interactive visualization
palette <- colorFactor(
  palette = c('red', 'green'),
  domain = total_table$sample 
)

map<-leaflet() %>%
  addTiles() %>% #capa base
  addCircles(data=total_table,col=~palette(sample)) #capa casas
map


##Add geographical variables
# Call the Centro Comercial Andino location
cc_andino <- geocode_OSM("Centro Comercial Andino, Bogotá", as.sf=T)
cc_andino

#Calculate the distances from the observations to the Point of Interest
total_table$cc_andino <- st_distance(x = total_table, y=cc_andino)

head(total_table$cc_andino)

#Check if effectively the distance between the test observations and  the Interest Point is different from those  in the training observations 
total_table %>% st_drop_geometry() %>% group_by(sample) %>% summarize(mean(cc_andino))



#Call the 93 Park location
parque_93 <- geocode_OSM("93 Park, Bogotá", as.sf=T)
parque_93

#Calculate the distances from the observations to the Point of Interest
total_table$parque_93 <- st_distance(x = total_table, y=parque_93)

head(total_table$parque_93)

#Check if effectively the distance between the test observations and  the Interest Point is different from those  in the training observations 
total_table %>% st_drop_geometry() %>% group_by(sample) %>% summarize(mean(parque_93))



#Call the Park El Virrey location
parque_el_virrey <- geocode_OSM("Parque El Virrey, Bogotá", as.sf=T)
parque_el_virrey

#Calculate the distances from the observations to the Point of Interest
total_table$parque_el_virrey <- st_distance(x = total_table, y=parque_el_virrey)

head(total_table$parque_el_virrey)

#Check if effectively the distance between the test observations and  the Interest Point is different from those  in the training observations 
total_table %>% st_drop_geometry() %>% group_by(sample) %>% summarize(mean(parque_el_virrey))



#Call the Universidad Javeriana location
universidad_javeriana <- geocode_OSM("Pontificia Universidad Javeriana, Bogotá", as.sf=T)
universidad_javeriana

#Calculate the distances from the observations to the Point of Interest
total_table$universidad_javeriana <- st_distance(x = total_table, y=universidad_javeriana)

head(total_table$universidad_javeriana)

#Check if effectively the distance between the test observations and  the Interest Point is different from those  in the training observations 
total_table %>% st_drop_geometry() %>% group_by(sample) %>% summarize(mean(universidad_javeriana))



#Call the Zona G proxy location - Four Seasons Hotel Casa Medina
zona_g <- geocode_OSM("Four Seasons Hotel Casa Medina Bogota, Bogotá", as.sf=T)
zona_g

#Calculate the distances from the observations to the Point of Interest
total_table$zona_g <- st_distance(x = total_table, y=zona_g)

head(total_table$zona_g)

#Check if effectively the distance between the test observations and  the Interest Point is different from those  in the training observations 
total_table %>% st_drop_geometry() %>% group_by(sample) %>% summarize(mean(zona_g))



#Call the Museo del Chicó location
museo_chico <- geocode_OSM("Museo del Chicó, Bogotá", as.sf=T)
museo_chico

#Calculate the distances from the observations to the Point of Interest
total_table$museo_chico <- st_distance(x = total_table, y=museo_chico)

head(total_table$museo_chico)

#Check if effectively the distance between the test observations and  the Interest Point is different from those  in the training observations 
total_table %>% st_drop_geometry() %>% group_by(sample) %>% summarize(mean(museo_chico))



#Call the Parque de los Hippies location
parque_hippies <- geocode_OSM("Parque de los Hippies o Sucre, Bogotá", as.sf=T)
parque_hippies

#Calculate the distances from the observations to the Point of Interest
total_table$parque_hippies <- st_distance(x = total_table, y=parque_hippies)

head(total_table$parque_hippies)

#Check if effectively the distance between the test observations and  the Interest Point is different from those  in the training observations 
total_table %>% st_drop_geometry() %>% group_by(sample) %>% summarize(mean(parque_hippies))




#Call the Club El Nogal location
club_el_nogal <- geocode_OSM("Club El Nogal, Bogotá", as.sf=T)
club_el_nogal

#Calculate the distances from the observations to the Point of Interest
total_table$club_el_nogal <- st_distance(x = total_table, y=club_el_nogal)

head(total_table$club_el_nogal)

#Check if effectively the distance between the test observations and  the Interest Point is different from those  in the training observations 
total_table %>% st_drop_geometry() %>% group_by(sample) %>% summarize(mean(club_el_nogal))




#Get information from supermarkets in Chapinero, Bogotá
# ?available_tags()
available_tags("shop")
supermarket <- opq(bbox = getbb("UPZs Localidad Chapinero")) %>%
  add_osm_feature(key = "shop", value = "supermarket")

#Convert to sf format
supermarket <- osmdata_sf(supermarket)

#Extract polygons
supermarket_geometria <- supermarket$osm_polygons %>%
  select(osm_id, name)

#Visualize supermarkets
leaflet() %>%
  addTiles() %>%
  addPolygons(data=supermarket_geometria, col = "red", opacity = 0.8, popup = supermarket_geometria$name)

#Distance from the properties to the nearest supermarket
centroides <- gCentroid(as(supermarket_geometria$geometry, "Spatial"), byid = T)
centroides <- st_as_sf(centroides, coords = c("x","y"))

dist_matrix <- st_distance(x=total_table, y=centroides) #Distance matrix
dim(dist_matrix) #dimensions. In the columns supermarkets, and in the rows the properties

total_table$min_distance_supermarket <- apply(dist_matrix, 1, min) #min distance

##Initial view of which variables might be important
#Predicting prices via a Linear Model
lm_model<- lm(log(price) ~ bedrooms + property_type + bathrooms + depot + parking + balcony + penthouse + gym + patio + lounge + 
                zona_g + universidad_javeriana + min_distance_supermarket + parque_hippies + parque_el_virrey + parque_93 + museo_chico + club_el_nogal + cc_andino + neighborhood, data = total_table)
summary(lm_model)
stargazer(lm_model, type = "text")

# V1 - Distance to Main Interest Points in Bogotá --------------------------------------------------------------------------
#Call the Centro Internacional de Bogotá location
cib <- geocode_OSM("Centro Internacional, Bogotá", as.sf=T)
cib

#Calculate the distances from the observations to the Point of Interest
total_table$DCIB <- st_distance(x = total_table, y=cib)

head(total_table$DCIB)

#Check if effectively the distance between the test observations and  the Interest Point is different from those  in the training observations 
total_table %>% st_drop_geometry() %>% group_by(sample) %>% summarize(mean(DCIB))

#Divide the total data to keep only the training data variables Price and Distance to the Interest Point
train_data <- total_table  %>% filter(sample=="train")  %>% select(property_id, price,DCIB,bedrooms)  %>% na.omit()

#Tell caret we want to use cross-validation 5 times #OJOOOOOO AJUSTAR PARA DATOS ESPACIALES. VER VIDEO ANTERIOR
fitControl<-trainControl(method = "cv",
                         number=5)

#Predicting prices with a tree 
#Train the model with Log(price)
set.seed(123)
tree <- train(
  log(price) ~ DCIB + bedrooms ,
  data = train_data,
  method = "rpart",
  trControl = fitControl,
  metric = "MAE",
  tuneLength = 300 #300 valores del alfa - cost complexity parameter
)

#Construct the test data frame
test_data<-total_table  %>% filter(sample=="test")  

#Predict the tree with test data
test_data$pred_tree<-predict(tree,test_data)

head(test_data  %>% select(property_id,pred_tree))

#Drop the variable geometry and return Log(prices) into Price
test_data <- test_data   %>% st_drop_geometry()  %>% mutate(pred_tree=exp(pred_tree))
head(test_data  %>% select(property_id,pred_tree))

#Create the submission document by selecting only the variables required and renaming them to adjust to instructions
submit<-test_data  %>% select(property_id,pred_tree)
write.csv(submit,"Tree_v1.csv",row.names=FALSE)

#MAE & MAPE
MAE(test_data$pred_tree, test_data$price)
#MAE V14: 2.496.469
MAPE(test_data$pred_tree, test_data$price)


# V2 - Predicting prices with Andino cross-validation -------------------------------------------------------------------------------------------------------------
#Tell caret we want to use cross-validation 5 times 
fitControl<-trainControl(method = "cv",
                         number=5)

#Train the model with Log(price)
set.seed(123)
tree <- train(
  log(price) ~ cc_andino + bedrooms ,
  data = train_data,
  method = "rpart",
  trControl = fitControl,
  metric = "MAE",
  tuneLength = 300 #300 valores del alfa - cost complexity parameter
)

#Construct the test data frame
test_data<-total_table  %>% filter(sample=="test")  

#Predict the tree with test data
test_data$pred_tree<-predict(tree,test_data)

head(test_data  %>% select(property_id,pred_tree))

#Drop the variable geometry and return Log(prices) into Price
test_data <- test_data   %>% st_drop_geometry()  %>% mutate(pred_tree=exp(pred_tree))
head(test_data  %>% select(property_id,pred_tree))

#Create the submission document by selecting only the variables required and renaming them to adjust to instructions
submit<-test_data  %>% select(property_id,pred_tree)
submit <- submit  %>% rename(price=pred_tree)
write.csv(submit,"Tree_v2.csv",row.names=FALSE)


# V3 - Predicting prices with Andino and Park 93 cross-validation --------------------------------------------------------------------------------------------------------------
#Divide the total data to keep only the wanted training data variables
train_data <- total_table  %>% filter(sample=="train")  %>% select(price,cc_andino, parque_93, bedrooms)  %>% na.omit()

#Tell caret we want to use cross-validation 5 times
fitControl<-trainControl(method = "cv",
                         number=5)

#Train the model with Log(price)
set.seed(123)
tree <- train(
  log(price) ~ cc_andino + parque_93 + bedrooms ,
  data = train_data,
  method = "rpart",
  trControl = fitControl,
  metric = "MAE",
  tuneLength = 300 #300 valores del alfa - cost complexity parameter
)

#Construct the test data frame
test_data<-total_table  %>% filter(sample=="test")  

#Predict the tree with test data
test_data$pred_tree<-predict(tree,test_data)

head(test_data  %>% select(property_id,pred_tree))

#Drop the variable geometry, return Log(prices) into Price and round to the nearest 100th
test_data <- test_data   %>% st_drop_geometry()  %>% mutate(pred_tree=exp(pred_tree))
head(test_data  %>% select(property_id,pred_tree, pred_tree))

#Create the submission document by selecting only the variables required and renaming them to adjust to instructions
submit<-test_data  %>% select(property_id,pred_tree)
submit <- submit  %>% rename(price=pred_tree)
write.csv(submit,"Tree_v3.csv",row.names=FALSE)

# V4 - Predicting prices with Andino, Park 93, bathrooms and property_type cross-validation --------------------------------------------------------------------------------------------------------------
#Divide the total data to keep only the wanted training data variables
train_data <- total_table  %>% filter(sample=="train")  %>% select(price,cc_andino, parque_93, bedrooms, bathrooms, property_type)  %>% na.omit()

#Tell caret we want to use cross-validation 5 times
fitControl<-trainControl(method = "cv",
                         number=5)

#Train the model with Log(price)
set.seed(123)
tree <- train(
  log(price) ~ cc_andino + parque_93 + bedrooms + bathrooms,
  data = train_data,
  method = "rpart",
  trControl = fitControl,
  metric = "MAE",
  tuneLength = 300 #300 valores del alfa - cost complexity parameter
)

#Construct the test data frame
test_data<-total_table  %>% filter(sample=="test")  

#Predict the tree with test data
test_data$pred_tree<-predict(tree,test_data)

head(test_data %>% select(property_id,pred_tree))

#Drop the variable geometry and return Log(prices) into Price
test_data <- test_data   %>% st_drop_geometry()  %>% mutate(pred_tree=exp(pred_tree))
head(test_data  %>% select(property_id,pred_tree))

#Create the submission document by selecting only the variables required and renaming them to adjust to instructions
submit<-test_data  %>% select(property_id,pred_tree)
submit <- submit  %>% rename(price=pred_tree)
write.csv(submit,"Tree_v4.csv",row.names=FALSE)


# V5 - Predicting prices with Andino, Park 93 and other parks in Chapinero cross-validation --------------------------------------------------------------------------------------------------------------
#Get information from parks in Chapinero, Bogotá
# ?available_tags()
available_tags("leisure")
parques <- opq(bbox = getbb("UPZs Localidad Chapinero")) %>%
  add_osm_feature(key = "leisure", value = "park")

#Convert to sf format
parques <- osmdata_sf(parques)

#Extract polygons
parques_geometria <- parques$osm_polygons %>%
  select(osm_id, name)

#Visualize parks
leaflet() %>%
  addTiles() %>%
  addPolygons(data=parques_geometria, col = "green", opacity = 0.8, popup = parques_geometria$name)

#Distance from the properties to the nearest park
centroides <- gCentroid(as(parques_geometria$geometry, "Spatial"), byid = T)
centroides <- st_as_sf(centroides, coords = c("x","y"))

dist_matrix <- st_distance(x=total_table, y=centroides) #Distance matrix
dim(dist_matrix) #dimensions. In the columns parks, and in the rows the properties

total_table$min_distance_parks <- apply(dist_matrix, 1, min) #min distance

#Divide the total data to keep only the wanted training data variables
train_data <- total_table  %>% filter(sample=="train")  %>% select(price,cc_andino, parque_93, min_distance_parks, bedrooms)  %>% na.omit()

#Tell caret we want to use cross-validation 5 times
fitControl<-trainControl(method = "cv",
                         number=5)

#Train the model with Log(price)
set.seed(123)
tree <- train(
  log(price) ~ cc_andino + parque_93 + min_distance_parks + bedrooms ,
  data = train_data,
  method = "rpart",
  trControl = fitControl,
  metric = "MAE",
  tuneLength = 300 #300 valores del alfa - cost complexity parameter
)

#Construct the test data frame
test_data<-total_table  %>% filter(sample=="test")  

#Predict the tree with test data
test_data$pred_tree<-predict(tree,test_data)

head(test_data  %>% select(property_id,pred_tree))

#Drop the variable geometry and return Log(prices) into Price
test_data <- test_data   %>% st_drop_geometry()  %>% mutate(pred_tree=exp(pred_tree))
head(test_data  %>% select(property_id,pred_tree))

#Create the submission document by selecting only the variables required and renaming them to adjust to instructions
submit<-test_data  %>% select(property_id,pred_tree)
submit <- submit  %>% rename(price = pred_tree)
write.csv(submit,"Tree_v5.csv",row.names=FALSE)


# V6 - Predicting prices with Andino, Park 93 and SuperMarkets cross-validation --------------------------------------------------------------------------------------------------------------
#Divide the total data to keep only the wanted training data variables
train_data <- total_table  %>% filter(sample=="train")  %>% select(price,cc_andino, parque_93, min_distance_supermarket, bedrooms)  %>% na.omit()

#Tell caret we want to use cross-validation 5 times
fitControl<-trainControl(method = "cv",
                         number=5)

#Train the model with Log(price)
set.seed(123)
tree <- train(
  log(price) ~ cc_andino + parque_93 + min_distance_supermarket + bedrooms ,
  data = train_data,
  method = "rpart",
  trControl = fitControl,
  metric = "MAE",
  tuneLength = 300 #300 valores del alfa - cost complexity parameter
)

#Construct the test data frame
test_data<-total_table  %>% filter(sample=="test")  

#Predict the tree with test data
test_data$pred_tree<-predict(tree,test_data)

head(test_data  %>% select(property_id,pred_tree))

#Drop the variable geometry and return Log(prices) into Price
test_data <- test_data   %>% st_drop_geometry()  %>% mutate(pred_tree=exp(pred_tree))
head(test_data  %>% select(property_id,pred_tree))

#Create the submission document by selecting only the variables required and renaming them to adjust to instructions
submit<-test_data  %>% select(property_id,pred_tree)
submit <- submit  %>% rename(price=pred_tree)
write.csv(submit,"Tree_v6.csv",row.names=FALSE)



# V7 - Predicting prices with Andino, Park 93 and Park El Virrey rounding, cross-validation --------------------------------------------------------------------------------------------------------------
#Divide the total data to keep only the wanted training data variables
train_data <- total_table  %>% filter(sample=="train")  %>% select(price,cc_andino, parque_93, parque_el_virrey, bedrooms)  %>% na.omit()

#Tell caret we want to use cross-validation 5 times
fitControl<-trainControl(method = "cv",
                         number=5)

#Train the model with Log(price)
set.seed(123)
tree <- train(
  log(price) ~ cc_andino + parque_93 + parque_el_virrey + bedrooms ,
  data = train_data,
  method = "rpart",
  trControl = fitControl,
  metric = "MAE",
  tuneLength = 300 #300 valores del alfa - cost complexity parameter
)

#Construct the test data frame
test_data<-total_table  %>% filter(sample=="test")  

#Predict the tree with test data
test_data$pred_tree<-predict(tree,test_data)

head(test_data  %>% select(property_id,pred_tree))

#Drop the variable geometry and return Log(prices) into Price
test_data <- test_data   %>% st_drop_geometry()  %>% mutate(pred_tree=exp(pred_tree))
test_data$price <- round(test_data$pred_tree, digits = -7)    #Indicates rounding to the nearest 10.000.000 (10^7)
head(test_data  %>% select(property_id, pred_tree, price))

#Create the submission document by selecting only the variables required and renaming them to adjust to instructions
submit<-test_data  %>% select(property_id,price)
write.csv(submit,"Tree_v7.csv",row.names=FALSE)



# V8 - Tree V3_rounding - Predicting prices with Andino and Park 93 cross-validation rounding ---------------------------------------------------------------------------
#Divide the total data to keep only the wanted training data variables
train_data <- total_table  %>% filter(sample=="train")  %>% select(price,cc_andino, parque_93, bedrooms)  %>% na.omit()

#Tell caret we want to use cross-validation 5 times
fitControl<-trainControl(method = "cv",
                         number=5)

#Train the model with Log(price)
set.seed(123)
tree <- train(
  log(price) ~ cc_andino + parque_93 + bedrooms ,
  data = train_data,
  method = "rpart",
  trControl = fitControl,
  metric = "MAE",
  tuneLength = 300 #300 valores del alfa - cost complexity parameter
)

#Construct the test data frame
test_data<-total_table  %>% filter(sample=="test")  

#Predict the tree with test data
test_data$pred_tree<-predict(tree,test_data)

head(test_data  %>% select(property_id,pred_tree))

#Drop the variable geometry, return Log(prices) into Price and round to the nearest 100th
test_data <- test_data   %>% st_drop_geometry()  %>% mutate(pred_tree=exp(pred_tree))

test_data$price <- round(test_data$pred_tree, digits = -7)    #Indicates rounding to the nearest 10.000.000 (10^7)

head(test_data  %>% select(property_id,pred_tree, price))

#Create the submission document by selecting only the variables required and renaming them to adjust to instructions
submit<-test_data  %>% select(property_id,price)
write.csv(submit,"Tree_v3_rounding.csv",row.names=FALSE)



# V9 - Predicting prices with Andino, Park 93, Park El Virrey and all dummies, rounding, cross-validation --------------------------------
#Divide the total data to keep only the wanted training data variables
train_data <- total_table  %>% filter(sample=="train")  %>% select(price,cc_andino, parque_93, parque_el_virrey, bedrooms, property_type, bathrooms, depot, parking, balcony, penthouse, gym, patio, lounge)  %>% na.omit()

#Tell caret we want to use cross-validation 10 times
fitControl<-trainControl(method = "cv",
                         number= 10)

#Train the model with Log(price)
set.seed(123)
tree <- train(
  log(price) ~ cc_andino + parque_93 + parque_el_virrey + bedrooms + property_type + bathrooms + depot + parking + balcony + penthouse + gym + patio + lounge,
  data = train_data,
  method = "rpart",
  trControl = fitControl,
  metric = "MAE",
  tuneLength = 300 #300 valores del alfa - cost complexity parameter
)

#Construct the test data frame
test_data<-total_table  %>% filter(sample=="test")  

#Predict the tree with test data
test_data$pred_tree<-predict(tree,test_data)

head(test_data  %>% select(property_id,pred_tree))

#Drop the variable geometry and return Log(prices) into Price
test_data <- test_data   %>% st_drop_geometry()  %>% mutate(pred_tree=exp(pred_tree))
test_data$price <- round(test_data$pred_tree, digits = -7)    #Indicates rounding to the nearest 10.000.000 (10^7)
head(test_data  %>% select(property_id, pred_tree, price))

#Create the submission document by selecting only the variables required and renaming them to adjust to instructions
submit<-test_data  %>% select(property_id,price)
write.csv(submit,"Tree_v9.csv",row.names=FALSE)

# V10 - Predicting prices via spatial blocks cross-validation --------------------------------------------------------------------------------------------------------------
#Divide the total data to keep only the wanted training data variables
train_data <- total_table  %>% filter(sample=="train")  %>% select(price,cc_andino, parque_93, parque_el_virrey, bedrooms, property_type, bathrooms, depot, parking, balcony, penthouse, gym, patio, lounge, neighborhood)  %>% na.omit()

#Spatial location folds
set.seed(123)

location_folds_train <- 
  spatial_leave_location_out_cv(
    train_data,
    group = neighborhood
  )

autoplot(location_folds_train)

folds_train<-list()
for(i in 1:length(location_folds_train$splits)){
  folds_train[[i]]<- location_folds_train$splits[[i]]$in_id
}

fitControl <- trainControl(method = "cv",
                           index = folds_train)

#Train the model with Log(price)
set.seed(123)
tree <- train(
  log(price) ~ cc_andino + parque_93 + parque_el_virrey + bedrooms + property_type + bathrooms + depot + parking + balcony + penthouse + gym + patio + lounge,
  data = train_data,
  method = "glmnet",
  trControl = fitControl,
  metric = "MAE",
  tuneGrid = expand.grid(alpha =seq(0,1,length.out = 20),
                           lambda = seq(0.001,0.2,length.out = 50))
)

tree

tree$bestTune

#Construct the test data frame
test_data<-total_table  %>% filter(sample=="test")  

#Predict the tree with test data
test_data$pred_tree<-predict(tree,test_data)

head(test_data %>% select(property_id,pred_tree))

#Drop the variable geometry and return Log(prices) into Price
test_data <- test_data   %>% st_drop_geometry()  %>% mutate(pred_tree=exp(pred_tree))
test_data$price <- round(test_data$pred_tree, digits = -7)    #Indicates rounding to the nearest 10.000.000 (10^7)
head(test_data  %>% select(property_id, pred_tree, price))

#Create the submission document by selecting only the variables required and renaming them to adjust to instructions
submit<-test_data  %>% select(property_id,price)
write.csv(submit,"Tree_v10.csv",row.names=FALSE)

# V10.2 - Predicting prices via spatial blocks cross validation with all variables ----------------------------------------------------------------
#Divide the total data to keep only the wanted training data variables
train_data <- total_table  %>% filter(sample=="train")  %>% select(price, bedrooms, property_type, bathrooms, depot, parking, balcony, penthouse, gym, patio, lounge, 
                                                                   zona_g, universidad_javeriana, min_distance_supermarket, parque_hippies, parque_el_virrey, parque_93, 
                                                                   museo_chico, club_el_nogal, cc_andino, neighborhood)  %>% na.omit()
#Spatial location folds
set.seed(123)

location_folds_train <- 
  spatial_leave_location_out_cv(
    train_data,
    group = neighborhood
  )

autoplot(location_folds_train)

folds_train<-list()
for(i in 1:length(location_folds_train$splits)){
  folds_train[[i]]<- location_folds_train$splits[[i]]$in_id
}

fitControl <- trainControl(method = "cv",
                           index = folds_train)

#Train the model with Log(price)
tree <- train(
  log(price) ~ bedrooms + property_type + bathrooms + depot + parking + balcony + penthouse + gym + patio + lounge + 
    zona_g + universidad_javeriana + min_distance_supermarket + parque_hippies + parque_el_virrey + parque_93 + museo_chico + 
    club_el_nogal + cc_andino,
  data = train_data,
  method = "glmnet",
  trControl = fitControl,
  metric = "MAE",
  tuneGrid = expand.grid(alpha =seq(0,1,length.out = 20),
                         lambda = seq(0.001,0.2,length.out = 50))
)

tree

tree$bestTune

#Construct the test data frame
test_data<-total_table  %>% filter(sample=="test")  

#Predict the tree with test data
test_data$pred_tree<-predict(tree,test_data)

head(test_data %>% select(property_id,pred_tree))

#Drop the variable geometry and return Log(prices) into Price
test_data <- test_data   %>% st_drop_geometry()  %>% mutate(pred_tree=exp(pred_tree))
test_data$price <- round(test_data$pred_tree, digits = -7)    #Indicates rounding to the nearest 10.000.000 (10^7)
head(test_data  %>% select(property_id, pred_tree, price))

#Create the submission document by selecting only the variables required and renaming them to adjust to instructions
submit<-test_data  %>% select(property_id,price)
write.csv(submit,"Tree_v10_2.csv",row.names=FALSE)




# V10.3 - Predicting prices via spatial blocks cross-validation with positive variables in a linear model --------------------------------------------------------------------------------------------------------------
#Divide the total data to keep only the wanted training data variables
train_data <- total_table  %>% filter(sample=="train")  %>% select(price,cc_andino, museo_chico, min_distance_supermarket, zona_g, bedrooms, property_type, bathrooms, depot, balcony, penthouse, gym, patio, neighborhood)  %>% na.omit()

#Spatial location folds
set.seed(123)

location_folds_train <- 
  spatial_leave_location_out_cv(
    train_data,
    group = neighborhood
  )

autoplot(location_folds_train)

folds_train<-list()
for(i in 1:length(location_folds_train$splits)){
  folds_train[[i]]<- location_folds_train$splits[[i]]$in_id
}

fitControl <- trainControl(method = "cv",
                           index = folds_train)

#Train the model with Log(price)
set.seed(123)
tree <- train(
  log(price) ~ cc_andino + museo_chico + min_distance_supermarket + zona_g + bedrooms + property_type + bathrooms + depot + balcony + penthouse + gym + patio,
  data = train_data,
  method = "glmnet",
  trControl = fitControl,
  metric = "MAE",
  tuneGrid = expand.grid(alpha =seq(0,1,length.out = 20),
                         lambda = seq(0.001,0.2,length.out = 50))
)

tree

tree$bestTune

#Construct the test data frame
test_data<-total_table  %>% filter(sample=="test")  

#Predict the tree with test data
test_data$pred_tree<-predict(tree,test_data)

head(test_data %>% select(property_id,pred_tree))

#Drop the variable geometry and return Log(prices) into Price
test_data <- test_data   %>% st_drop_geometry()  %>% mutate(pred_tree=exp(pred_tree))
test_data$price <- round(test_data$pred_tree, digits = -7)    #Indicates rounding to the nearest 10.000.000 (10^7)
head(test_data  %>% select(property_id, pred_tree, price))

#Create the submission document by selecting only the variables required and renaming them to adjust to instructions
submit<-test_data  %>% select(property_id,price)
write.csv(submit,"Tree_v10_3.csv",row.names=FALSE)

# V11 - Predicting prices via spatial blocks cost complexity prunning ramdom forest--------------------------------------------------------------------------------------------------------------
#Divide the total data to keep only the wanted training data variables
train_data <- total_table  %>% filter(sample=="train")  %>% select(price,cc_andino, parque_93, parque_el_virrey, bedrooms, property_type, bathrooms, depot, parking, balcony, penthouse, gym, patio, lounge, neighborhood)  %>% na.omit()

#Spatial Block Cost Complexity Prunning
set.seed(123)

location_folds_train <- 
  spatial_leave_location_out_cv(
    train_data,
    group = neighborhood
  )

autoplot(location_folds_train)

folds_train<-list()
for(i in 1:length(location_folds_train$splits)){
  folds_train[[i]]<- location_folds_train$splits[[i]]$in_id
}

fitControl <- trainControl(method = "cv",
                           index = folds_train)

#Train the model with Log(price)
set.seed(123)
tree_ranger <- train(
  log(price) ~ cc_andino + parque_93 + parque_el_virrey + bedrooms + property_type + bathrooms + depot + parking + balcony + penthouse + gym + patio + lounge,
  data = train_data,
  method = "ranger",
  trControl = fitControl,
  metric = "MAE",
  tuneGrid = expand.grid(
    mtry = 1,  #número de predictores que va a sacar aleatoriamente. En este caso en cada bootstrap saca 1 predictor de la regresión
    splitrule = "variance", #Regla de partición
    min.node.size = 5) #Cantidad de observaciones en el nodo. Default 5 para regresiones
)

tree_ranger

tree_ranger$bestTune

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
write.csv(submit,"Tree_v11.csv",row.names=FALSE)

# V12 - Predicting prices via spatial blocks cost complexity prunning bagging--------------------------------------------------------------------------------------------------------------
#Divide the total data to keep only the wanted training data variables
train_data <- total_table  %>% filter(sample=="train")  %>% select(price, bedrooms, property_type, bathrooms, depot, parking, balcony, penthouse, gym, patio, lounge, 
                                                                    zona_g, universidad_javeriana, min_distance_supermarket, parque_hippies, parque_el_virrey, parque_93, 
                                                                      museo_chico, club_el_nogal, cc_andino, neighborhood)  %>% na.omit()

#Spatial Block Cost Complexity Prunning - Bagging
set.seed(123)

fitControl <- trainControl(method = "cv",
                           number = 10)

#Train the model with Log(price)
tree_ranger <- train(
  log(price) ~ bedrooms + property_type + bathrooms + depot + parking + balcony + penthouse + gym + patio + lounge + 
    zona_g + universidad_javeriana + min_distance_supermarket + parque_hippies + parque_el_virrey + parque_93 + museo_chico + 
    club_el_nogal + cc_andino,
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
write.csv(submit,"Tree_v12.csv",row.names=FALSE)


# V13 - Predicting prices via spatial blocks cost complexity prunning boosting --------------------------------------------------------------------------------------------------------------
#Divide the total data to keep only the wanted training data variables
train_data <- total_table  %>% filter(sample=="train")  %>% select(price, bedrooms, property_type, bathrooms, depot, parking, balcony, penthouse, gym, patio, lounge, 
                                                                   zona_g, universidad_javeriana, min_distance_supermarket, parque_hippies, parque_el_virrey, parque_93, 
                                                                   museo_chico, club_el_nogal, cc_andino, neighborhood)  %>% na.omit()

#Spatial Block Cost Complexity Prunning - Bagging
set.seed(123)


fitControl <- trainControl(method = "cv",
                           number = 10)

#Train the model with Log(price)
tree_boosted <- train(
  log(price) ~ bedrooms + property_type + bathrooms + depot + parking + balcony + penthouse + gym + patio + lounge + 
    zona_g + universidad_javeriana + min_distance_supermarket + parque_hippies + parque_el_virrey + parque_93 + museo_chico + 
    club_el_nogal + cc_andino,
  data = train_data,
  method = "bstTree",
  trControl = fitControl,
  tuneGrid=expand.grid(
    mstop = c(400,500,600), #Boosting Iterations (M)
    maxdepth = c(1,2,3), # Max Tree Depth (d)
    nu = c(0.01,0.001)) # Shrinkage (lambda)
)

tree_boosted

tree_boosted$bestTune

#Construct the test data frame
test_data<-total_table  %>% filter(sample=="test")  

#Predict the tree with test data
test_data$pred_tree<-predict(tree_boosted,test_data)

head(test_data %>% select(property_id,pred_tree))

#Drop the variable geometry and return Log(prices) into Price
test_data <- test_data   %>% st_drop_geometry()  %>% mutate(pred_tree=exp(pred_tree))
test_data$price <- round(test_data$pred_tree, digits = -7)    #Indicates rounding to the nearest 10.000.000 (10^7)
head(test_data  %>% select(property_id, pred_tree, price))

#Create the submission document by selecting only the variables required and renaming them to adjust to instructions
submit<-test_data  %>% select(property_id,price)
write.csv(submit,"Tree_v13.csv",row.names=FALSE)

#MAE and MAPE test 
MAE(test_data$pred_tree, test_data$price)
## MAE V13: 2.569.115

# V14 - Predicting prices via spatial blocks cost complexity prunning bagging-------------------------------------------------------------------------------------------------------------------------------------------------------
#Divide the total data to keep only the wanted training data variables
train_data <- total_table  %>% filter(sample=="train")  %>% select(price,cc_andino, parque_93, parque_el_virrey, bedrooms, property_type, bathrooms, depot, parking, balcony, penthouse, gym, patio, lounge, neighborhood)  %>% na.omit()

#Spatial Block Cost Complexity Prunning - Bagging
set.seed(123)

fitControl <- trainControl(method = "cv",
                           number = 10)

#Train the model with Log(price)
tree_ranger <- train(
  log(price) ~ cc_andino + parque_93 + parque_el_virrey + bedrooms + property_type + bathrooms + depot + parking + balcony + penthouse + gym + patio + lounge,
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
  Model = c("Model 14", "Model 2", "Model 3", "Model 4", "Model 5"),
  Method = c("Random Forest", "Random Forests", "Bagging", "Boosting", "Other Method"),
  Variables = c("15", "Var1, Var3", "Var2, Var4", "Var1, Var2, Var3", "Var4"),
  MAE = c("2.496.469", "8.5", "12.1", "9.8", "11.3")
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
