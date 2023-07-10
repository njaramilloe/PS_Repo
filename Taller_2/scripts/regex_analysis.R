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
       wordcloud, #takes words frequencies as input to create a cloud visualization
       tidyverse, #data manipulation and visualization
       tm, #text mining and natural language processing task
       rio, #import/export data file formats
       skimr, #summary data
       stargazer, #generate publication-quality tables
       RColorBrewer #color palettes for thematic maps
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

head(total_table$title)
head(total_table$description)

#' converts non-ASCII-range punctuation, symbols, and latin letters 
#' in an ASCII-range equivalent
total_table$title <- stri_trans_general(str = total_table$title, id = "Latin-ASCII")
total_table$description <- stri_trans_general(str = total_table$description, id = "Latin-ASCII")

# remove not alphanumeric text or spaces
total_table$title <- gsub('[^A-Za-z0-9 ]+', ' ', total_table$title)
total_table$description  <- gsub('[^A-Za-z0-9 ]+', ' ', total_table$description )

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
                                                                    stopwords))

# remove multiple consecutive whitespace characters
total_table$title <- gsub('\\s+', ' ', total_table$title)
total_table$description <- gsub('\\s+', ' ', total_table$description)

#  remove leading and trailing whitespace (spaces, tabs, newlines, etc.)
total_table$title <- trimws(total_table$title)
total_table$description <- trimws(total_table$description)

#---------------------------tokenization----------------------------------------
#' tokenize both title and description splitting into individual words
#' each observation has a list of tokens
total_table$tokens_title <- tokenize_words(total_table$title)
total_table$tokens_descr <- tokenize_words(total_table$description)


#-----------------------stemming words------------------------------------------
## reduce to its roots each tokenized word
#total_table$tokens_title <- wordStem(total_table$tokens_title, "spanish")
#total_table$tokens_descr  <- wordStem(total_table$tokens_descr, "spanish")
#total_table$tokens_title[1:15]
#total_table$description[1:10]

#-----------------------words frequencies---------------------------------------
## make a table with word frequencies


frec_title <- total_table$title %>%
  table() %>%
  data.frame() %>%
  rename("Word" = ".") %>%
  arrange(desc(Freq))

frec_descr <- total_table$tokens_descr %>%
  table() %>%
  data.frame() %>%
  rename("Word" = ".") %>%
  arrange(desc(Freq))

#-----------------------delete repeated words-----------------------------------
## delete word without added value for the analysis

# load the r-script file that contents the list of repeated word identified
source("../scripts/list_of_repeated_words_in_title.R")

total_table$title <- lapply(total_table$title, 
                            function(title) removeWords(title, repeated_words))

# remove multiple consecutive whitespace characters
total_table$title <- gsub('\\s+', ' ', total_table$title)

# remove leading and trailing whitespace (spaces, tabs, newlines, etc.)
total_table$title <- trimws(total_table$title)

#-----------------------wordcloud graphic---------------------------------------
## Loading required package: RColorBrewer
frec_title <- total_table$title %>%
  table() %>%
  data.frame() %>%
  rename("Word" = ".") %>%
  arrange(desc(Freq))

p_load(xlsx)
write.xlsx(frec_title, file = "frec_results.xlsx")  

frec_descr <- total_table$tokens_descr %>%
  table() %>%
  data.frame() %>%
  rename("Word" = ".") %>%
  arrange(desc(Freq))

set.seed(666) 
png(filename = "wordcloud_title.png", width = 1000, height = 1000)
wordcloud(words = frec_title$Word, freq = frec_title$Freq, scale=c(16,2),
          min.freq = 1, max.words = 200, random.order=  FALSE, rot.per = 0.35,
          colors = brewer.pal(8, "Dark2"))
dev.off()
getwd()
