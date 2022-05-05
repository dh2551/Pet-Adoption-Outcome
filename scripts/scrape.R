
library(tidyverse)
library(httr)
library(jsonlite)

#try curl 
api_token <- '' ## insert your token 

dog_adopted_pre_2020 <- foreach(page = 1:1000, .combine='bind_rows') %do% {
  urls <- paste0('https://api.petfinder.com/v2/animals?type=Dog&status=adopted&before=2020-01-01T00:00:00-05:00&page=',page,'&limit=100') #status=adopted&
  raw_data <- httr::GET(urls,accept_json(), add_headers("Authorization" = paste("Bearer", api_token, sep = " ")))
  mydata <- httr::content(raw_data,type="application/json")
  mydata <- fromJSON(rawToChar(raw_data$content), flatten = TRUE)
  animals <- mydata$animals
} 

cat_adopted_pre_2020 <- foreach(page = 1:1000, .combine='bind_rows') %do% {
  urls <- paste0('https://api.petfinder.com/v2/animals?type=Cat&status=adopted&before=2020-01-01T00:00:00-05:00&page=',page,'&limit=100') #status=adopted&
  raw_data <- httr::GET(urls,accept_json(), add_headers("Authorization" = paste("Bearer", api_token, sep = " ")))
  mydata <- httr::content(raw_data,type="application/json")
  mydata <- fromJSON(rawToChar(raw_data$content), flatten = TRUE)
  animals <- mydata$animals
}

dog_adopted_pre_2021 <- foreach(page = 1:1000, .combine='bind_rows') %do% {
  urls <- paste0('https://api.petfinder.com/v2/animals?type=Dog&status=adopted&before=2021-01-01T00:00:00-05:00&page=',page,'&limit=100') #status=adopted&
  raw_data <- httr::GET(urls,accept_json(), add_headers("Authorization" = paste("Bearer", api_token, sep = " ")))
  mydata <- httr::content(raw_data,type="application/json")
  mydata <- fromJSON(rawToChar(raw_data$content), flatten = TRUE)
  animals <- mydata$animals
} 

cat_adopted_pre_2021 <- foreach(page = 1:1000, .combine='bind_rows') %do% {
  urls <- paste0('https://api.petfinder.com/v2/animals?type=Cat&status=adopted&before=2021-01-01T00:00:00-05:00&page=',page,'&limit=100') #status=adopted&
  raw_data <- httr::GET(urls,accept_json(), add_headers("Authorization" = paste("Bearer", api_token, sep = " ")))
  mydata <- httr::content(raw_data,type="application/json")
  mydata <- fromJSON(rawToChar(raw_data$content), flatten = TRUE)
  animals <- mydata$animals
} 


write_csv(cat_adopted_pre_2020, './data/cat_2019.csv')

write_csv(dog_adopted_pre_2020, './data/dog_2019.csv')

write_csv(cat_adopted_pre_2021, './data/cat_2020.csv')

write_csv(dog_adopted_pre_2021, './data/dog_2020.csv')



dog_2020 <- read_csv('./data/dog_2020.csv')
dog_2019 <- read_csv('./data/dog_2019.csv')
cat_2020 <- read_csv('./data/cat_2020.csv')
cat_2019 <- read_csv('./data/cat_2019.csv')

dog_2020_q4 <- dog_2020 %>% 
  filter(published_at <= ymd('2021-01-01')& published_at >= ymd('2020-10-01'))

dog_2019_q4 <- dog_2019 %>% 
  filter(published_at <= ymd('2020-01-01')& published_at >= ymd('2019-10-01'))

cat_2020_q4 <- cat_2020 %>% 
  filter(published_at <= ymd('2021-01-01')& published_at >= ymd('2020-10-01'))

cat_2019_q4 <- cat_2019 %>% 
  filter(published_at <= ymd('2020-01-01')& published_at >= ymd('2019-10-01'))


write_csv(cat_2019_q4, './data/cat_2019_q4.csv')

write_csv(dog_2019_q4, './data/dog_2019_q4.csv')

write_csv(cat_2020_q4, './data/cat_2020_q4.csv')

write_csv(dog_2020_q4, './data/dog_2020_q4.csv')









