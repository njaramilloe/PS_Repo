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

#Save it as a data frame
train_data<-as.data.frame(train_data) 

glimpse(train_data)
train_data[,2]
train_data$columna<-as.numeric(train_data[,2,drop =T][,1])
train_data$DCIB<-NULL

#Check for missing values in the training data
colSums(is.na(train_data))
summary(train_data)

names(train_data)

names(train_data) <- c("Price", "Bedrooms", "Geometry",  "DCIB")

sum(train_data$Price == 0)
sum(train_data$Bedrooms == 0)
sum(train_data$DCIB == 0)

train_data <- train_data[train_data$Bedrooms != 0, ]

#Tell caret we want to use cross-validation 5 times #OJOOOOOO AJUSTAR PARA DATOS ESPACIALES. VER VIDEO ANTERIOR
fitControl<-trainControl(method = "cv",
                         number=5)

#Predicting prices with a tree ----------------------------------------------------------------------------------------------------------------------------
#Train the model with Log(price)
set.seed(123)
tree <- train(
  log(Price) ~ DCIB + Bedrooms ,
  data = train_data,
  method = "rpart",
  trControl = fitControl,
  metric = "MAE",
  tuneLength = 300 #300 valores del alfa - cost complexity parameter
)

#Construct the test data frame
test_data<-total_table  %>% filter(sample=="test")  

test_data<-as.data.frame(test_data) 

glimpse(test_data)
test_data[,17]
test_data$columna<-as.numeric(test_data[,17,drop =T][,1])
test_data$DCIB<-NULL

#Check for missing values in the testing data
colSums(is.na(test_data))
summary(test_data)

names(test_data)

names(test_data) <- c("Property_id", "City", "Price", "Month", "Year", "Surface_total", 'Surface_covered', "Rooms", "Bedrooms", "Bathrooms", "Property_type", "Operation_type", "Title", "Description", "Sample", "Geometry",  "DCIB")

#Predict the tree with test data
test_data$pred_tree<-predict(tree,test_data)

head(test_data  %>% select(Property_id,pred_tree))

#Drop the variable geometry and return Log(prices) into Price
test_data <- test_data   %>% st_drop_geometry()  %>% mutate(pred_tree=exp(pred_tree))
head(test_data  %>% select(Property_id,pred_tree))

#Create the submission document by selecting only the variables required and renaming them to adjust to instructions
submit<-test_data  %>% select(Property_id,pred_tree)
submit <- submit  %>% rename(price=pred_tree)
submit <- submit  %>% rename(property_id=Property_id)
write.csv(submit,"Tree_v1.csv",row.names=FALSE)

#Predicting prices via spatial blocks cross-validation --------------------------------------------------------------------------------------------------------------
#Call the Centro Comercial Andino location
cc_andino <- geocode_OSM("Centro Comercial Andino, Bogotá", as.sf=T)
cc_andino

#Calculate the distances from the observations to the Point of Interest
total_table$cc_andino <- st_distance(x = total_table, y=cc_andino)

head(total_table$cc_andino)

#Check if effectively the distance between the test observations and  the Interest Point is different from those  in the training observations 
total_table %>% st_drop_geometry() %>% group_by(sample) %>% summarize(mean(cc_andino))

#Divide the total data to keep only the training data variables Price and Distance to the Interest Point
train_data_2 <- total_table  %>% filter(sample=="train")  %>% select(price,cc_andino,bedrooms, badrooms)  %>% na.omit()

#Save it as a data frame
train_data<-as.data.frame(train_data) 

glimpse(train_data)
train_data[,2]
train_data$columna<-as.numeric(train_data[,2,drop =T][,1])
train_data$DCIB<-NULL

#Check for missing values in the training data
colSums(is.na(train_data))
summary(train_data)

names(train_data)

names(train_data) <- c("Price", "Bedrooms", "Geometry",  "DCIB")

sum(train_data$Price == 0)
sum(train_data$Bedrooms == 0)
sum(train_data$DCIB == 0)

train_data <- train_data[train_data$Bedrooms != 0, ]

##Spatial autocorrelation
set.seed(123)
block_folds <- spatial_block_cv(total_table, v = 5) # 5 folds
autoplot(block_folds)

tree <- train(
  log(Price) ~ DCIB + Bedrooms ,
  data = train_data,
  method = "rpart",
  trControl = fitControl,
  metric = "MAE",
  tuneLength = 300 #300 valores del alfa - cost complexity parameter
)

#Construct the test data frame
test_data<-total_table  %>% filter(sample=="test")  

test_data<-as.data.frame(test_data) 

glimpse(test_data)
test_data[,17]
test_data$columna<-as.numeric(test_data[,17,drop =T][,1])
test_data$DCIB<-NULL

#Check for missing values in the testing data
colSums(is.na(test_data))
summary(test_data)

names(test_data)

names(test_data) <- c("Property_id", "City", "Price", "Month", "Year", "Surface_total", 'Surface_covered', "Rooms", "Bedrooms", "Bathrooms", "Property_type", "Operation_type", "Title", "Description", "Sample", "Geometry",  "DCIB")

#Predict the tree with test data
test_data$pred_tree<-predict(tree,test_data)

head(test_data  %>% select(Property_id,pred_tree))

#Drop the variable geometry and return Log(prices) into Price
test_data <- test_data   %>% st_drop_geometry()  %>% mutate(pred_tree=exp(pred_tree))
head(test_data  %>% select(Property_id,pred_tree))

#Create the submission document by selecting only the variables required and renaming them to adjust to instructions
submit<-test_data  %>% select(Property_id,pred_tree)
submit <- submit  %>% rename(price=pred_tree)
submit <- submit  %>% rename(property_id=Property_id)
write.csv(submit,"Tree_v1.csv",row.names=FALSE)




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



