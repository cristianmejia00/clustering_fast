# 20161220
# Function to get cluster, and
# Widgets that complement The Knowledge Dashboard

library(data.table)
library(plyr)
library(dplyr)
library(plotly)
library(tools)
library(DT)
library(igraph)

#####################################################################
#####################################################################

# This outputs ONLY 1 country per paper
# Input a DATA FRAME
# Works over column RP
# Parse and get the country name of the corresponding author
getCountry <- function(d) {
  addresses <- strsplit(d$RP, split = ", |,") #Separate string by the comma
  addresses <- lapply(addresses, function(x) {x[length(x)]}) #Get the last value which is the country
  addresses <- sapply(addresses, function(x) gsub("\\.", "", x)) #Remove the dot
  addresses <- tolower(addresses)
  addresses[grep(" usa$|^usa$|^[[:alpha:]][[:alpha:]] |^[[:alpha:]][[:alpha:]]$", addresses)] <- "usa" #Standardize all US addresses
  addresses[grep("england|scotland|wales|north ireland", addresses)] <- "united kingdom"
  # Add the country
  Country <- as.character(addresses)
  Country[which(Country=="character(0)")] <- "-" #Correct the papers with no country info
  
  return(Country)
}

# This outputs MULTIPLE countries per paper
# Inputs a COLUMN (i.e. myDataCorrect$C1)
# Works over column C1
# This function extract the countries participating in the study.
# Countries are not repeated. i.e. A paper with multiple authors from different institutions in the US and 1 author
# affiliated to an institution in the UK will return "usa; england"
getCountries <- function(a_C1_column) {
  countries <- gsub("\\[.*?\\]", "author_list", a_C1_column)
  countries <- strsplit(countries, split = "; ") 
  countries <- sapply(countries, function(ii) {
    tmp <- sapply(ii, function(jj) {
      x <- strsplit(jj, split = ", |,") %>% unlist
      return(x[length(x)])
    }) %>%  
      trimws %>% 
      unique %>% 
      tolower %>% 
      unname
    tmp <- gsub("\\.", "", tmp)
    tmp[grep(" usa$|^usa$|^[[:alpha:]][[:alpha:]] |^[[:alpha:]][[:alpha:]]$", tmp)] <- "usa"
    tmp[grep("england|scotland|wales|north ireland", tmp)] <- "united kingdom"
    tmp <- unique(tmp)
    return(paste(tmp, collapse = "; "))})
  return(countries)
}


# Function that transforms the WOS country names
# to ISO 3166 3-characters code.
# We need this codes to plot in map libraries
# The conversion tables is expected to be a data frame with headers including "ISO3166alpha3" and "WOS" with the corresponding ISO code and country names as appear in the WOS.
# I already created this file and have it here as an asset. 
country_codes <- read.csv(file.path(getwd(), "zz_assets", "country_conversion.csv"), header = TRUE, stringsAsFactors = FALSE)
iso_search <- country_codes$ISO3166alpha3
names(iso_search) <- country_codes$WOS
rm(country_codes)
getIsoCountries <- function(a_countries_column, conversion_vector = iso_search) {
  tmp <- strsplit(a_countries_column, split = "; ")
  tmp <- lapply(tmp, function(x){
    paste(conversion_vector[x], collapse = "; ")
  })
  return(as.character(tmp))
}

# This outputs MULTIPLE countries per paper
# Inputs a COLUMN (i.e. myDataCorrect$C1)
# Works over column C1
# This function extract the institutions participating in the study.
# Institutes are not repeated. i.e. A paper with multiple authors from same institution in the US and 1 author
# affiliated to an institution in the UK will return, for example, "univ harvard; univ tokyo"
getInstitutions <- function(a_c1_column) {
  inst <- gsub("\\[.*?\\] ", "", a_c1_column ) %>%
    tolower %>%
    strsplit(split ="; ")
  inst <- lapply(inst, function(ii) {
    lapply(ii, function(jj) {
      x <- strsplit(jj, split = ", ") %>% unlist
      return(x[1])
    }) %>% 
      unlist %>%
      unique %>%
      paste(., collapse = "; ")
  })
  return(inst)
}

## Generic
# Auxiliar function to get the top values from a Factiva 
# Works for "NS"("AU") categories "CO"("ID") companies "IN"("DE") industries "RE"("C1") regions. Factiva("WOS")
TopSomething <- function(d, coll = "ID" , top = 5){
  categories <- tolower(d[[coll]]) 
  if (coll %in% c('Countries', 'Institutions', 'AU', 'SO')) {
    categories <- stringr::str_to_title(categories)
  }
  categories <- categories %>% 
    strsplit(split ="; ") %>% 
    unlist %>% 
    table %>% 
    sort(decreasing = TRUE) %>% 
    .[1:top]
  names(categories)[tolower(names(categories)) %in% c('usa')] <- 'USA'
  return(categories)
}


## Remove word count from abstract
remove_word_counts_line <- function(a_text) {
  a_text <- gsub(" [A-z0-9 ]*?\\(Word\\).*$", "", a_text)
  a_text <- gsub(" [A-z0-9 ]*?\\(word\\).*$", "", a_text)
  return(a_text)
}

## Remove copyright statement from abstract
# Function to remove the copyright from abstracts.
# Input: A char string or vector. Ususally the abstracts from WOS
# Output: The imput without the copyritgh statements.
# Dependencies: None.

# Test for 
# Elsevier
# rights reserved
# Published by
# creativecommons
# C0
remove_copyright_statements <- function(a_text) {
  a_text <- gsub(" [A-z0-9 ]*?\\(C\\).*$", "", a_text)
  a_text <- gsub(" [A-z0-9 ]*?\\(c\\).*$", "", a_text)
  a_text <- gsub(" [A-z0-9 ]*?\\.\\(C\\).*$", "", a_text)
  a_text <- gsub(" [A-z0-9 ]*?\\.\\(c\\).*$", "", a_text)
  a_text <- gsub(" CO .*$", "", a_text)
  a_text <- gsub(" This is an open access article .*$", "", a_text)
  a_text <- gsub(" Published by .*$", "", a_text)
  a_text <- gsub("(1|2)\\d{3} E.*$", "", a_text)
  return(a_text)
}
