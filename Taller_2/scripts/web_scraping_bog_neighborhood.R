#' *****************************************************************************
#' *****************WEB-SCRAPPING OF BOGOTA'S NEIGBORHOODS***********************
#' *****************************************************************************

require(pacman)

p_load(tidyverse, # 
       rvest, #
       stringi
)

# specify the URL of the wikipedia page
bog_url <- "https://es.wikipedia.org/wiki/Anexo:Barrios_de_Bogot%C3%A1"

# read the html content of the page
bog_page <- read_html(bog_url)

# extract the table containing neighborhoods
neighborhood_table <- html_table(html_nodes(bog_page, "table")[[1]], fill = TRUE)[[4]]

# Replace "y" with commas, typing and roman numerals
neighborhood_table <- gsub(" y ", ", ", neighborhood_table)
neighborhood_table <- gsub(" yTorca.", ", torca", neighborhood_table)
neighborhood_table <- gsub("\\b(I|II|III|IV|XI|XII|XIX|XVI|XVII|XVIII|XXII|XXV)\\b", 
                           "", neighborhood_table)
neighborhood_table <- gsub(" ,", " ", neighborhood_table)
neighborhood_table <- gsub("Casablanca 32, 33", "Casablanca", 
                           neighborhood_table)

# transforms neighborhoods to lowercase letters
neighborhood_table <- tolower(neighborhood_table)

# cleaning data and separate and unify the neighborhoods from the table
separated_neighborhood <- strsplit(neighborhood_table, ", ")
unified_neighborhood <- unlist(separated_neighborhood)

#' converts non-ASCII-range punctuation, symbols, and latin letters 
#' in an ASCII-range equivalent
unified_neighborhood <- stri_trans_general(str = unified_neighborhood, id = "Latin-ASCII")

# remove not alphanumeric text or spaces
unified_neighborhood <- gsub('[^A-Za-z0-9 ]+', ' ', unified_neighborhood)

# erase stopwords from title and description variable
unified_neighborhood <- lapply(unified_neighborhood, 
                            function(unified_neighborhood) removeWords(unified_neighborhood, stopwords))

# remove multiple consecutive whitespace characters
unified_neighborhood <- gsub('\\s+', ' ', unified_neighborhood)

#  remove leading and trailing whitespace (spaces, tabs, newlines, etc.)
unified_neighborhood <- trimws(unified_neighborhood)

p_load(xlsx)
write.xlsx(unified_neighborhood, file = "lista_barrios.xlsx")  
getwd()

# Create the neighborhood mapping values using apply
neighborhood_mapping <- sapply(unified_neighborhood, function(neighborhood) {
  return(as.character(neighborhood))
}, USE.NAMES = TRUE)

# generate the neighborhood variable
total_table$neighborhood <- character(length(total_table$title))

# iterate through each neighborhood mapping
for (pattern in names(neighborhood_mapping)) {
  # Create a regex pattern using the mapping key
  regex_pattern <- paste0("\\b", pattern, "\\b")
  
  # find matching titles using regex
  matching_indices <- str_detect(total_table$title, regex_pattern)
  
  # assign the corresponding neighborhood value to matching titles
  total_table$neighborhood[matching_indices] <- neighborhood_mapping[pattern]
}

# print the resulting variable
print(total_table$neighborhood)

table(total_table$title, total_table$neighborhood)

