
library(tidyverse)
library(httr)
library(jsonlite)

#try curl 
api_token <- ''


animals_data_adopted <- foreach(page = 1:100, .combine='rbind') %do% {
  urls <- paste0('https://api.petfinder.com/v2/animals/?status=adopted&page=',page,'&limit=100')
  raw_data <- httr::GET(urls,accept_json(), add_headers("Authorization" = paste("Bearer", api_token, sep = " ")))
  mydata <- httr::content(raw_data,type="application/json")
  mydata <- fromJSON(rawToChar(raw_data$content), flatten = TRUE)
  animals <- mydata$animals
} 




