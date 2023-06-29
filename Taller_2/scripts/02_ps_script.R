rm(list = ls())

library(pacman)
p_load(rvest, tidyverse)

# Set working directory
setwd("/Users/nataliajaramillo/Documents/GitHub/Taller_2/stores")

## Mantener la memoria limpia en caso de requerir capacidad para el procesamiento de datos
rm(list = "url", i, "tabla", data_chunk)


## INTENTOS ANTES DE CONVERTIRLOS EN DEFINITIVOS ------------------------------
url_test <- "https://www.properati.com.co/s/chapinero-bogota-d-c/venta"

test_html <- read_html(url_test)

# Scrape the data and stored it in separate vectors. Add nested links and multiples pages
name <- test_html %>% html_nodes("#listings-content .listing-card__title") %>% html_text()
price <- test_html %>% html_nodes("#listings-content .price") %>% html_text()
location <- test_html %>% html_nodes("#listings-content .listing-card__location") %>% html_text()
bedrooms <- test_html %>% html_nodes("#listings-content .card-icon__bedrooms+ span") %>% html_text() #24 observaciones en vez de 30
bathrooms <- test_html %>% html_nodes("#listings-content .card-icon__bathrooms+ span") %>% html_text() #27 observaciones de 30
area <- test_html %>% html_nodes("#listings-content .card-icon__area+ span") %>% html_text() #29 de 30 obs.
agency <- test_html %>% html_nodes("#listings-content .listing-card__agency-name") %>% html_text()

# Determine the length of the longest vector
max_length <- max(length(name), length(price), length(location), length(bedrooms), length(bathrooms), length(area), length(agency))

# Pad the vectors with NA to match the length of the longest vector
name <- rep(c(name, NA), length.out = max_length)
price <- rep(c(price, NA), length.out = max_length)
location <- rep(c(location, NA), length.out = max_length)
bedrooms <- rep(c(bedrooms, NA), length.out = max_length)
bathrooms <- rep(c(bathrooms, NA), length.out = max_length)
area <- rep(c(area, NA), length.out = max_length)
agency <- rep(c(agency, NA), length.out = max_length)

# Create the data frame with the modified vectors
test_listings <- data.frame(name, price, location, bedrooms, bathrooms, area, agency, stringsAsFactors = FALSE)

# Verify the resulting data frame
view(test_listings)

# Save the data frame as a CSV 
write.csv(test_listings, "test_listings.csv")

# AHORA, HACER UN LOOP PARA AUTOMATIZAR

per_listing <- function(property_links){
  property_links <- test_html %>% html_nodes(xpath = '//*[@id="listings-content"]/a[1]') %>%
    html_attr("href") %>% paste("https://www.properati.com.co",., sep="") #Como cambiar el a[1] por a[i] para sacar todos los listings en la página?
  
  property_links <- "https://www.properati.com.co/detalle/14032-32-a85d-443cac917f0b-ff151654-befb-4d08"
  property_page <- read_html(property_links)
  characteristics <- property_page %>% html_nodes(".facilities span") %>% html_text() %>%
    paste(collapse = ",")
  return(characteristics)
}

