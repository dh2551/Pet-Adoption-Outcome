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
library(lubridate)
library(tidytext)


### load data 

cat_pre_2020 <- read_csv('./data/cat_before_2020.csv' )
dog_pre_2020 <- read_csv('./data/dog_adopted_before_2020.csv' )

adopted_all_pre_2020 <- rbind(cat_pre_2020, dog_pre_2020)

## check na rate 
empty_rate <- adopted_all_pre_2020 %>% summarise_all(list(name = ~sum(is.na(.))/length(.)))

## get cats and dogs and drop columns with too many NAs 
## unsure if we should drop primary color
adopted_pre_2020_full <- adopted_all_pre_2020 %>%
  filter(status_changed_at != published_at,
         type == species) %>%
  select(-coat, -tags,-organization_animal_id, -photos, -videos, -distance, 
         -breeds.secondary, -colors.secondary, -colors.tertiary,-attributes.declawed, # Just changed the order features
         -primary_photo_cropped.small, -primary_photo_cropped.medium,       ##not sure about dropping these
         -primary_photo_cropped.large, -primary_photo_cropped.full,         ##we should drop them
         -contact.address.address1, -contact.address.address2, -contact.address.postcode) 

# i don't think name matters; there are 14999 NAs for color, and I still not sure if we keep it;
# there are only 1822 for CA, I think we drop it

# check again
empty_rate_2 <- adopted_pre_2020_full %>% summarise_all(list(name = ~sum(is.na(.))/length(.)))


# drop columns that are useless 
adopted_pre_2020_v1 <- adopted_pre_2020_full %>%
  select(-url, -species, -`_links.self.href`, -`_links.type.href`, -`_links.organization.href`,
         -name, -colors.primary, -contact.address.country)


# re-level columns 
#   age: baby 1, young 2, adult 3, senior 4 (as.factor)
#   size: small 1, medium 2, large 3, extra large 4 (as.factor)
#   gender: male 1, female 2 (as.factor)
#   environment.child/dog/cat: true 1, false 0, na--> uncertain 2 (as.factor)
adopted_pre_2020_v1$age <- factor(adopted_pre_2020_v1$age,c("Baby","Young","Adult","Senior"))
adopted_pre_2020_v1$size <- factor(adopted_pre_2020_v1$size,c("Small","Medium","Large","Extra Large"))
adopted_pre_2020_v1$gender <- factor(adopted_pre_2020_v1$gender,c("Male","Female"))
adopted_pre_2020_v1$environment.children <- factor(adopted_pre_2020_v1$environment.children,c("TRUE","FALSE","NA"))
adopted_pre_2020_v1$environment.cats <- factor(adopted_pre_2020_v1$environment.cats,c("TRUE","FALSE","NA"))
adopted_pre_2020_v1$environment.dogs <- factor(adopted_pre_2020_v1$environment.dogs,c("TRUE","FALSE","NA"))

 
# construct new columns
#   adopted within a month: yes 1, no 0
#   contact: email, phone, both 
#   description: should we do a word count column + number of positive word used to describe pet column
#   breed - pure breed/mixed (ie cat:domestic tabby calico, dog: pitty boxer )
#   "desired breed": look up aspca or similar orgs for something like 
#                    list of most popular, and top 5 label 1-5, others 0?
#   

adopted_pre_2020_v1 <- adopted_pre_2020_v1 %>%
  mutate(duration_adopt = difftime(status_changed_at,published_at, unit="days"),
         less_then_30_days = ifelse(duration_adopt < 30, 1, 0),
         contacts = case_when(is.na(contact.phone) == TRUE & is.na(contact.email) == FALSE ~ 'email',
                                  is.na(contact.phone) == FALSE & is.na(contact.email) == TRUE ~ 'phone',
                                  is.na(contact.phone) == FALSE & is.na(contact.email) == FALSE ~'both',
                                  TRUE ~ 'neither') )

description_count <- adopted_pre_2020_v1 %>% select(id,description) %>% unnest_tokens(word, description) 
description_word_count <- description_count %>% group_by(id) %>% summarise(count = n())
adopted_pre_2020_v1 <- left_join(adopted_pre_2020_v1,description_word_count)%>% mutate(count = ifelse(is.na(description)==TRUE,0,count))

sentiment_bing <- get_sentiments("bing")
description_count <- inner_join(description_count,sentiment_bing) 
description_sum <- description_count %>% group_by(id) %>% summarise(pos_count = sum(sentiment == "positive"),
                                                                    neg_count = sum(sentiment == "negative"))

adopted_pre_2020_v1 <- left_join(adopted_pre_2020_v1,description_sum) 




