rm(list = ls())

library(pacman)

p_load(rvest, 
       tidyverse, #data manipulation and visualization
       stringr, #manipulate and analyze character data
       stringi, #manipulate string/text data in the cleaning process
       sf, #analyze spacial data
       leaflet, #create interactive web maps and visualizations
       tmaptools, #support spatial data manipulation and analysis
       caret, #Classification And REgression Training 
       vtable, #output a descriptive variable table
       spatialsample, #spatial resampling
       ggplot2, #data visualization
       rio, #import/export data file formats
       skimr, #summary data
       stargazer,#generate publication-quality tables
       expss #functions from spreadsheets and SPSS Statistics software
)

# set working directory
path_script <- rstudioapi::getActiveDocumentContext()$path
path_folder <- dirname(path_script)
setwd(path_folder)
setwd("../stores")


#LOAD DATA --------------------------------------------------------------------------------------------------------------------------------------
#Load training data
train <- read.csv("train.csv")

#Load test data
test <- read.csv("test.csv")

#Glimpse into the data bases
head(test)
head(train)

#Generate new variable that identifies the sample
test<-test %>% mutate(sample="test")
train<-train %>% mutate(sample="train")

#Bind together both databases
total_table<-rbind(test,train)
table(total_table$sample) #test 10286 | train 38644

#Understand the data ----------------------------------------------------------------------------------------------------------------------------
#DESCRIPTIVE STATISTICS
glimpse(total_table)
head(total_table[1:15])

#Check for string inconsistencies
unique(total_table$property_type)

#Check for missing values
colSums(is.na(total_table))/nrow(total_table)*100 

# Check the levels of each variable
variable_levels <- sapply(total_table, function(x) length(unique(x)))
variable_levels

# Identify variables with only one level
single_level_vars <- names(variable_levels[variable_levels == 1])
single_level_vars

#Vtable statistics
total_table_selected<- total_table %>% select(property_id, bedrooms, bathrooms, surface_covered)
sumtable(total_table_selected, out = "return")

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

## Distance to Main Interest Points in Bogotá --------------------------------------------------------------------------
#Call the Centro Internacional de Bogotá location
cib <- geocode_OSM("Centro Internacional, Bogotá", as.sf=T)
cib

#Calculate the distances from the observations to the Point of Interest
total_table$DCIB <- st_distance(x = total_table, y=cib)

head(total_table$DCIB)

#Check if effectively the distance between the test observations and  the Interest Point is different from those  in the training observations 
total_table %>% st_drop_geometry() %>% group_by(sample) %>% summarize(mean(DCIB))

#Divide the total data to keep only the training data variables Price and Distance to the Interest Point
train_data <- total_table  %>% filter(sample=="train")  %>% select(price,DCIB,bedrooms)  %>% na.omit()

#Tell caret we want to use cross-validation 5 times #OJOOOOOO AJUSTAR PARA DATOS ESPACIALES. VER VIDEO ANTERIOR
fitControl<-trainControl(method = "cv",
                         number=5)

#V1 - Predicting prices with a tree ----------------------------------------------------------------------------------------------------------------------------
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
head(test_data  %>% select(Property_id,pred_tree))

#Create the submission document by selecting only the variables required and renaming them to adjust to instructions
submit<-test_data  %>% select(Property_id,pred_tree)
submit <- submit  %>% rename(price=pred_tree)
submit <- submit  %>% rename(property_id=Property_id)
write.csv(submit,"Tree_v1.csv",row.names=FALSE)

#V2 - Predicting prices with Andino cross-validation --------------------------------------------------------------------------------------------------------------
#Call the Centro Comercial Andino location
cc_andino <- geocode_OSM("Centro Comercial Andino, Bogotá", as.sf=T)
cc_andino

#Calculate the distances from the observations to the Point of Interest
total_table$cc_andino <- st_distance(x = total_table, y=cc_andino)

head(total_table$cc_andino)

#Check if effectively the distance between the test observations and  the Interest Point is different from those  in the training observations 
total_table %>% st_drop_geometry() %>% group_by(sample) %>% summarize(mean(cc_andino))

#Divide the total data to keep only the wanted training data variables
train_data <- total_table  %>% filter(sample=="train")  %>% select(price,cc_andino,bedrooms)  %>% na.omit()

#Tell caret we want to use cross-validation 5 times #OJOOOOOO AJUSTAR PARA DATOS ESPACIALES. VER VIDEO ANTERIOR
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
#Call the Centro Comercial Andino location
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


#Divide the total data to keep only the wanted training data variables
train_data <- total_table  %>% filter(sample=="train")  %>% select(price,cc_andino, parque_93,bedrooms)  %>% na.omit()

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

#Drop the variable geometry and return Log(prices) into Price
test_data <- test_data   %>% st_drop_geometry()  %>% mutate(pred_tree=exp(pred_tree))
head(test_data  %>% select(property_id,pred_tree))

#Create the submission document by selecting only the variables required and renaming them to adjust to instructions
submit<-test_data  %>% select(property_id,pred_tree)
submit <- submit  %>% rename(price=pred_tree)
write.csv(submit,"Tree_v3.csv",row.names=FALSE)



# V4 - Predicting prices with Andino, Park 93, bathrooms and property_type cross-validation --------------------------------------------------------------------------------------------------------------
#Call the Centro Comercial Andino location
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


#Change NAs in Bathroom to 0
sum(is.na(total_table$bathrooms))
filas_con_na <- is.na(total_table$bathrooms)
total_table[filas_con_na,]%>%
  view()
total_table$bathrooms[is.na(total_table$bathrooms)] <- 0

colSums(is.na(total_table))

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





#Predicting prices via cross-validation with Andino, bathrooms and property_type --------------------------------------------------------------------------------------------------------------
#Call the Centro Comercial Andino location
cc_andino <- geocode_OSM("Centro Comercial Andino, Bogotá", as.sf=T)
cc_andino

#Calculate the distances from the observations to the Point of Interest
total_table$cc_andino <- st_distance(x = total_table, y=cc_andino)

head(total_table$cc_andino)

#Check if effectively the distance between the test observations and  the Interest Point is different from those  in the training observations 
total_table %>% st_drop_geometry() %>% group_by(sample) %>% summarize(mean(cc_andino))

#Divide the total data to keep only the wanted training data variables
train_data <- total_table  %>% filter(sample=="train")  %>% select(price,cc_andino,bedrooms, bathrooms, property_type)  %>% na.omit()

#Tell caret we want to use cross-validation 5 times 
fitControl<-trainControl(method = "cv",
                         number=5)

#Train the model with Log(price)
set.seed(123)
tree <- train(
  log(price) ~ cc_andino + bedrooms + bathrooms + property_type,
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
write.csv(submit,"Tree_v5.csv",row.names=FALSE)






#Predicting prices via spatial blocks cross-validation --------------------------------------------------------------------------------------------------------------
#Call the Centro Comercial Andino location
cc_andino <- geocode_OSM("Centro Comercial Andino, Bogotá", as.sf=T)
cc_andino

#Calculate the distances from the observations to the Point of Interest
total_table$cc_andino <- st_distance(x = total_table, y=cc_andino)

head(total_table$cc_andino)

#Check if effectively the distance between the test observations and  the Interest Point is different from those  in the training observations 
total_table %>% st_drop_geometry() %>% group_by(sample) %>% summarize(mean(cc_andino))

#Divide the total data to keep only the wanted training data variables
train_data <- total_table  %>% filter(sample=="train")  %>% select(price,cc_andino,bedrooms)  %>% na.omit()

#Spatial Blocks
block_folds <- spatial_block_cv(total_table, v = 5) #5 folds
autoplot(block_folds)

#Train the model with Log(price)
set.seed(123)
tree <- train(
  log(price) ~ cc_andino + bedrooms ,
  data = train_data,
  method = "rpart",
  trControl = block_folds,
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








#####HAY QUE GENERAR LAS  LOCALIDADES COMO PENTAGONOS
#Spatial Leave Location Out Cross Validation (LLOCV) 
location_folds <-
  spatial_leave_location_out_cv(
    train_data,
    group = Neighborhood
  )
autoplot(location_folds)

#Predicting prices via a Linear Model ------------------------------------------------------------------------------------------------------------------------------
lm_model<- lm(log(price) ~  DCIB + bedrooms , data = train_data)
summary(lm_model)
stargazer(lm_model, type = "text")


