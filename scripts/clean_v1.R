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


##

cat_breed <- adopted_pre_2020_v1 %>% 
  filter(type == 'Cat') %>%
  distinct(breeds.primary)

cat_breed_check <- adopted_pre_2020_v1 %>%
  filter(type == 'Cat') %>% 
  select(id, type, breeds.primary, breeds.mixed)

dog_breed_check <- adopted_pre_2020_v1 %>%
  filter(type == 'Dog') %>% 
  select(id, type, breeds.primary, breeds.mixed)


dog_breed <- adopted_pre_2020_v1 %>%
  filter(type == 'Dog') %>%
  distinct(breeds.primary)

# https://humanepro.org/page/pets-by-the-numbers

# if breed is "mutt"/mixed bred, then breeds_bin = 0 
# these breeds include dsh, dmh, dlh, torbie, calico, tabby
# if breed is "pure", then breeds_bin = 1
# tiger, hemingway can be either purebred or mutt (tiger & polydactyl are type of cat not cat breeds)
# for these two we will examine "breed.mixed"

breeds_binary <- adopted_pre_2020_v1 %>%
  mutate(breeds_bin = case_when(breeds.primary == 'Domestic Short Hair' | breeds.primary ==  'Domestic Medium Hair' |
                                  breeds.primary == 'Domestic Long Hair'| breeds.primary ==  'Torbie' | 
                                  breeds.primary == 'Calico' | breeds.primary == 'Tabby' | 
                                  breeds.primary == 'Tortoiseshell' | breeds.primary == 'Dilute Tortoiseshell' | 
                                  breeds.primary == 'Dilute Calico' | breeds.primary == 'Tuxedo' ~ 0 ,
                                breeds.primary == 'Tiger' & breeds.mixed == TRUE ~ 0,
                                breeds.primary == 'Extra-Toes Cat / Hemingway Polydactyl' & breeds.mixed == TRUE ~ 0 ,
                                breeds.primary == 'American Bulldog' | breeds.primary == 'American Buly' | 
                                  breeds.primary == 'Boxer' |  breeds.primary == 'Bull Terrier' | 
                                  breeds.primary == 'Boston Terrier' | breeds.primary == 'Bullmastiff' | 
                                  breeds.primary == 'Cane Corso' | breeds.primary == 'English Bulldog' |
                                  breeds.primary == 'Mixed Breed' | breeds.primary == 'Pit Bull Terrier' | 
                                  breeds.primary == 'Staffordshire Bull Terrier' ~ 0, 
                                TRUE ~ 1)) %>%
  select(id, breeds_bin, breeds.primary, breeds.mixed)                          

check <- breeds_binary %>%
  filter(breeds_bin == 1, breeds.mixed == TRUE)

table(breeds_binary$breeds_bin)





