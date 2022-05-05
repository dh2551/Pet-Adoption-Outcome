
library(tidyverse)
library(httr)
library(jsonlite)

#try curl 
api_token <- '' ## insert your token 

dog_adopted_pre_2020 <- foreach(page = 1:1000, .combine='bind_rows') %do% {
  urls <- paste0('https://api.petfinder.com/v2/animals?type=dog&status=adopted&before=2020-01-01T00:00:00-05:00&page=',page,'&limit=100') #status=adopted&
  raw_data <- httr::GET(urls,accept_json(), add_headers("Authorization" = paste("Bearer", api_token, sep = " ")))
  mydata <- httr::content(raw_data,type="application/json")
  mydata <- fromJSON(rawToChar(raw_data$content), flatten = TRUE)
  animals <- mydata$animals
} 

cat_adopted_pre_2020 <- foreach(page = 1:1000, .combine='bind_rows') %do% {
  urls <- paste0('https://api.petfinder.com/v2/animals?type=cat&status=adopted&before=2020-01-01T00:00:00-05:00&page=',page,'&limit=100') #status=adopted&
  raw_data <- httr::GET(urls,accept_json(), add_headers("Authorization" = paste("Bearer", api_token, sep = " ")))
  mydata <- httr::content(raw_data,type="application/json")
  mydata <- fromJSON(rawToChar(raw_data$content), flatten = TRUE)
  animals <- mydata$animals
}

dog_adopted_pre_2021 <- foreach(page = 1:1000, .combine='bind_rows') %do% {
  urls <- paste0('https://api.petfinder.com/v2/animals?type=dog&status=adopted&before=2021-01-01T00:00:00-05:00&page=',page,'&limit=100') #status=adopted&
  raw_data <- httr::GET(urls,accept_json(), add_headers("Authorization" = paste("Bearer", api_token, sep = " ")))
  mydata <- httr::content(raw_data,type="application/json")
  mydata <- fromJSON(rawToChar(raw_data$content), flatten = TRUE)
  animals <- mydata$animals
} 

cat_adopted_pre_2021 <- foreach(page = 1:1000, .combine='bind_rows') %do% {
  urls <- paste0('https://api.petfinder.com/v2/animals?type=cat&status=adopted&before=2021-01-01T00:00:00-05:00&page=',page,'&limit=100') #status=adopted&
  raw_data <- httr::GET(urls,accept_json(), add_headers("Authorization" = paste("Bearer", api_token, sep = " ")))
  mydata <- httr::content(raw_data,type="application/json")
  mydata <- fromJSON(rawToChar(raw_data$content), flatten = TRUE)
  animals <- mydata$animals
} 

dog_2020_q4 <- dog_adopted_pre_2021 %>% 
  filter(published_at < '2021-01-01T00:00:00-05:00' & published_at > '2020-10-01T00:00:00-05:00')

dog_2019_q4 <- dog_adopted_pre_2020 %>% 
  filter(published_at < '2020-01-01T00:00:00-05:00' & published_at > '2019-10-01T00:00:00-05:00')

cat_2020_q4 <- cat_adopted_pre_2021 %>% 
  filter(published_at < '2021-01-01T00:00:00-05:00' & published_at > '2020-10-01T00:00:00-05:00')

cat_2019_q4 <- cat_adopted_pre_2020 %>% 
  filter(published_at < '2020-01-01T00:00:00-05:00' & published_at > '2019-10-01T00:00:00-05:00')


write_csv(cat_adopted_pre_2020 , './data/cat_2019_q4.csv')

write_csv(cat_adopted_pre_2021 , './data/cat_2020_q4.csv')

write_csv(cat_adopted_pre_2020 , './data/cat_2019_q4.csv')

write_csv(dog_adopted_pre_2021 , './data/dog_2020_q4.csv')









