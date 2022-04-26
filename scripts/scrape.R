
library(tidyverse)
library(httr)
library(jsonlite)

#try curl 
api_token <- ''


cat_adopted_pre_2020 <- foreach(page = 1:1000, .combine='bind_rows') %do% {
  urls <- paste0('https://api.petfinder.com/v2/animals?type=cat&status=adopted&before=2020-01-01T00:00:00-05:00&page=',page,'&limit=100') #status=adopted&
  raw_data <- httr::GET(urls,accept_json(), add_headers("Authorization" = paste("Bearer", api_token, sep = " ")))
  mydata <- httr::content(raw_data,type="application/json")
  mydata <- fromJSON(rawToChar(raw_data$content), flatten = TRUE)
  animals <- mydata$animals
} 



write_csv(animal_adopted_pre_2020 , './data/cat_before_2020.csv')