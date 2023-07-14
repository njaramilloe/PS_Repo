#' *****************************************************************************
#' **********************REGULAR EXPRESSIONS ANALYSIS****************************
#' *****************************************************************************


#----------------------prepare the environment----------------------------------
#clean global environment
rm(list = ls())

#load packages
require("pacman")

p_load(stringi, #manipulate string/text data in the cleaning process
       tokenizers, #convert natural language text into tokens
       stopwords, #low value commonly used words cleaning
       SnowballC, #apply stemming to each word reducing them to their root forms
       wordcloud, #takes words frequencies to create a cloud visualization
       tidyverse, #data manipulation and visualization
       tm, #text mining and natural language processing task
       rio, #import/export data file formats
       skimr, #summary data
       stargazer, #generate publication-quality tables
       RColorBrewer, #color palettes for thematic maps
       xlsx, #read and write format excel files
       purrr #converts list variables to string
)


# set working directory
path_script <- rstudioapi::getActiveDocumentContext()$path
path_folder <- dirname(path_script)
setwd(path_folder)
setwd("../stores")


#----------------------loading data---------------------------------------------
# load training data
train <- read.csv("train.csv")

# load test data
test <- read.csv("test.csv")

# generate new variable that identifies the sample
test<-test %>% mutate(sample="test")
train<-train %>% mutate(sample="train")

# bind together both databases
total_table<-rbind(test,train)
table(total_table$sample) #test 10286 | train 38644


#----------------------data cleaning process------------------------------------
#' converts non-ASCII-range punctuation, symbols, and latin letters 
#' in an ASCII-range equivalent
total_table$title <- stri_trans_general(str = total_table$title, 
                                        id = "Latin-ASCII")

total_table$description <- stri_trans_general(str = total_table$description, 
                                              id = "Latin-ASCII")

# remove not alphanumeric text or spaces
total_table$title <- gsub('[^A-Za-z0-9 ]+', ' ', total_table$title)
total_table$description  <- gsub('[^A-Za-z0-9 ]+', ' ', total_table$description)

# be sure are lowercase letters
total_table$title <- tolower(total_table$title)
total_table$description <- tolower(total_table$description)

# remove multiple consecutive whitespace characters
total_table$title <- gsub('\\s+', ' ', total_table$title)
total_table$description <- gsub('\\s+', ' ', total_table$description)

# erase numbers from the title
total_table$title <- gsub("\\d+", "", total_table$title)
#total_table$description <- gsub("\\d+", "", total_table$description)

#  remove leading and trailing whitespace (spaces, tabs, newlines, etc.)
total_table$title <- trimws(total_table$title)
total_table$description <- trimws(total_table$description)


#-----------------------erase meaningless words---------------------------------
#' use a list of stopwords from two libraries: tm and stopwords
#' this is to make a bigger list of words

#' tm library: 
#' create a vector of stopwords in spanish
tm_stopwords <- tm::stopwords("es")

#' stopwords library
#' create a list to store the stopwords from sources that supports spanish
st_stopwords1 <- stopwords::stopwords(language = "es", source = "snowball")
st_stopwords2 <- stopwords::stopwords(language = "es", source = "nltk")
st_stopwords3 <- stopwords::stopwords(language = "es", source = "stopwords-iso")

# join stopwords coming from different libraries and sources
stopwords <- list(tm_stopwords, st_stopwords1, st_stopwords2, st_stopwords3)
stopwords <- Reduce(union, stopwords)

# erase stopwords from title and description variable
total_table$title <- lapply(total_table$title, 
                            function(title) removeWords(title, stopwords))

total_table$description <- lapply(total_table$description, 
                                  function(description) removeWords(description, 
                                                                    st_stopwords2))

# remove multiple consecutive whitespace characters
total_table$title <- gsub('\\s+', ' ', total_table$title)
total_table$description <- gsub('\\s+', ' ', total_table$description)

#  remove leading and trailing whitespace (spaces, tabs, newlines, etc.)
total_table$title <- trimws(total_table$title)
total_table$description <- trimws(total_table$description)

# create a variable to analyze attributes
total_table$title_attribute <- total_table$title
#----------------------- TITLE ANALYSIS-----------------------------------------
#----------------------- delete repeated words----------------------------------
## delete words without added value for the analysis

# load the r-script file that contents the list of repeated word identified
source("../scripts/list_of_repeated_words_in_title.R")

# remove word without added value
total_table$title <- lapply(total_table$title, 
                            function(title) removeWords(title, repeated_words))

# remove multiple consecutive whitespace characters
total_table$title <- gsub('\\s+', ' ', total_table$title)

# remove leading and trailing whitespace (spaces, tabs, newlines, etc.)
total_table$title <- trimws(total_table$title)
total_table$title_neigbor <- total_table$title

#---------------------------tokenization----------------------------------------
#' tokenize both title and description splitting into individual words
#' each observation has a list of tokens
total_table$tokens_title_neigbor <- tokenize_words(total_table$title_neigbor)
total_table$tokens_descr <- tokenize_words(total_table$description)


#-----------------------stemming words------------------------------------------
## reduce to its roots each tokenized word
#total_table$tokens_title <- wordStem(total_table$tokens_title, "spanish")
#total_table$tokens_descr  <- wordStem(total_table$tokens_descr, "spanish")


#----------------------web-scraping bogota's neighborhood------------------------
#' in an external RStudio script named "web_scraping_bog_neighborhood.R", the 
#' code is written to execute a web-scraping process and retrieve Bogota's 
#' neighborhoods from Wikipedia. At the end, it creates a new variable to store 
#' all the neighborhoods that match the text from the publications title.

source("../scripts/web_scraping_bog_neighborhood.R")  # web-scrapping process


#-----------------------words frequencies---------------------------------------
## make a table with word frequencies

# using publications title
frec_title_neigbor <- total_table$title_neigbor %>%
  table() %>%
  data.frame() %>%
  rename("Word" = ".") %>%
  arrange(desc(Freq))

# using neighborhood that's results from web-scraping matching
frec_matched_neigbor <- total_table$neighborhood %>%
  table() %>%
  data.frame() %>%
  rename("Word" = ".") %>%
  arrange(desc(Freq))

p_load(xlsx)
write.xlsx(frec_title_neigbor, file = "frec_title_results.xlsx")  
write.xlsx(frec_matched_neigbor, file = "frec_matched_neigbor.xlsx")  


#-----------------------wordcloud graphic---------------------------------------
## set a wordclodu for title_neigbor and matched_neigbor

set.seed(666) 
png(filename = "wordcloud_title_neigbor.png", width = 1000, height = 1000)
wordcloud(words = frec_title_neigbor$Word, freq = frec_title_neigbor$Freq, 
          scale=c(16,2), min.freq = 1, max.words = 200, random.order=  FALSE, 
          rot.per = 0.35, colors = brewer.pal(8, "Dark2"))
dev.off()


set.seed(666) 
png(filename = "wordcloud_matched_neigbor.png", width = 1000, height = 1000)
wordcloud(words = frec_title_neigbor$Word, freq = frec_title_neigbor$Freq, 
          scale=c(16,2), min.freq = 1, max.words = 200, random.order=  FALSE, 
          rot.per = 0.35, colors = brewer.pal(8, "Dark2"))
dev.off()
getwd()

#-----------------------export the data base------------------------------------
## export a csv data file with the data to run the models

##show the data structure
str(total_table)

##converts list variables to string variables
total_table$tokens_title_neigbor <- map_chr(total_table$tokens_title_neigbor, 
                                            toString)
total_table$tokens_descr <- map_chr(total_table$tokens_descr 
                                    , toString)

# Remove the desired columns from the data frame
total_table <- total_table[, !(names(total_table) %in% 
                                 c("tokens_title_neigbor", "tokens_descr"))]

## exports the data sets
write.csv(total_table, file = "db_property_bogota.csv", row.names = FALSE )
#----------------------- DESCRIPTION ANALYSIS-----------------------------------
total_table <- read.csv("db_property_bogota.csv")

#'We create dummies for different apartment characteristics to later be used in
#' the models. The information comes from the variable "description". 
sum(grepl("bodega|deposito|bodegas|depositos", total_table$description))
total_table <- total_table %>% 
  mutate(depot = ifelse(grepl("bodega|deposito|bodegas|depositos", total_table$description), 1, 0))
sum(1, total_table$depot)
summary(total_table$depot)

sum(grepl("parqueadero|parqueaderos|garaje|garajes", total_table$description))
total_table <- total_table %>% 
  mutate(parking = ifelse(grepl("parqueadero|parqueaderos|garaje|garajes", total_table$description), 1, 0))
sum(1, total_table$parking)
summary(total_table$parking)

sum(grepl("balcon|balcones", total_table$description))
total_table <- total_table %>% 
  mutate(balcony = ifelse(grepl("balcon|balcones", total_table$description), 1, 0))
sum(1, total_table$balcony)
summary(total_table$balcony)

sum(grepl("penhouse|penthouse", total_table$description))
total_table <- total_table %>% 
  mutate(penthouse = ifelse(grepl("penhouse|penthouse", total_table$description), 1, 0))
sum(1, total_table$penthouse)
summary(total_table$penthouse)

filtered_descriptions <- total_table %>% 
  filter(grepl("gimnasios", description))
print(filtered_descriptions)

sum(grepl("gimnasio|gimnasios", total_table$description))
total_table <- total_table %>% 
  mutate(gym = ifelse(grepl("gimnasio|gimnasios", total_table$description), 1, 0))
sum(1, total_table$gym)
summary(total_table$gym)

sum(grepl("terraza|terrazas|patio|patios", total_table$description))
total_table <- total_table %>% 
  mutate(patio = ifelse(grepl("terraza|terrazas|patio|patios", total_table$description), 1, 0))
sum(1, total_table$patio)
summary(total_table$patio)

sum(grepl("salon privado|salon comunal|salon social|salones privados|salones comunales|salones sociales", total_table$description))
total_table <- total_table %>% 
  mutate(lounge = ifelse(grepl("salon privado|salon comunal|salon social|salones privados|salones comunales|salones sociales", total_table$description), 1, 0))
sum(1, total_table$lounge)
summary(total_table$lounge)

write.csv(total_table, file = "cleandata.csv", row.names = FALSE)



print(total_table$description)
sum(is.na(total_table$rooms))
ftrooms <- total_table %>% 
filter(is.na(rooms), grepl("habitaciones|habitacion|alcoba|alcobas", description))
print(ftrooms)

           
# Function to convert written numbers to digits
convert_to_numeric <- function(number) {
  written_nums <- c("uno", "dos", "tres", "cuatro", "cinco", "seis", "siete", "ocho", "nueve", "diez")
  digit_nums <- 1:10
  ifelse(tolower(number) %in% written_nums, digit_nums[match(tolower(number), written_nums)], number)
}

convert_to_numeric(total_table$description)

# Extract and replace the rooms variable based on description
total_table <- total_table %>%
  mutate(rooms = str_extract(description, "(?i)(\\d+\\s*(?=habitacion|habitaciones|alcoba|alcobas)|(?<=habitacion|habitaciones|alcoba|alcobas)\\s*\\d+)") %>%
             str_extract("\\d+") %>% convert_to_numeric())

# Print the modified total_table
print(total_table)