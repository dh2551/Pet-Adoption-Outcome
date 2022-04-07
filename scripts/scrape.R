
library(tidyverse)
library(httr)
library(jsonlite)

#try curl 
api_token <- ''
raw_data <- httr::GET(url,accept_json(), add_headers("Authorization" = paste("Bearer", api_token, sep = " ")))


mydata <- httr::content(raw_data,type="application/json")
mydata<- fromJSON(rawToChar(raw_data$content), flatten = TRUE)

animals <- mydata$animals


