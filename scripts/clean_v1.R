################################################
###
### author: Angela He & Mingyao Xu
### purpose: clean raw data for model fitting 
### input: adopted_before_2020.csv 
###        adopted_before_202x
###        adopted_after_
### output: adopted_before_2020_cleaned.csv
###
################################################

### load library 
library(tidyverse)


### load data 

adopted_all_pre_2020 <- read_csv('./data/adopted_before_2020_76000.csv' )

## check na rate 
empty_rate <- adopted_all_pre_2020 %>% summarise_all(list(name = ~sum(is.na(.))/length(.)))
## get cats and dogs
adopted_pre_2020_v1 <- adopted_all_pre_2020 %>%
  filter(status_changed_at != published_at,
         type == 'Cat' | type == 'Dog') %>%
  select(-organization_animal_id, -photos, -videos, -distance, -breeds.secondary,
         -colors.secondary, -colors.tertiary,-attributes.declawed, 
         -primary_photo_cropped.small, -primary_photo_cropped.medium,       ##not sure about dropping these
         -primary_photo_cropped.large, -primary_photo_cropped.full,         ##not sure about dropping these
         -contact.address.address1, -contact.address.address2, -contact.address.postcode)

#check again
empty_rate_2 <- adopted_pre_2020_v1 %>% summarise_all(list(name = ~sum(is.na(.))/length(.)))




