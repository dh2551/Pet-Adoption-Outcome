################################################
###
### author: Angela He & Mingyao Xu
### purpose: fit tree based models (random forest & boosting)
### input: adopted_2019_q4_cleaned.csv
###        adopted_2020_q4_cleaned.csv
### output: figures
###
################################################


##### load library #####
library(tidyverse)
library(lubridate)
library(ROCR)
library(ranger)

adopted_2019<- read_csv('./data/adopted_2019_q4_cleaned.csv' )
adopted_2020<- read_csv('./data/adopted_2020_q4_cleaned.csv' )

################################################ method 1 ################################################

train_2019 <- adopted_2019 %>% 
  filter(month_published == 10 | month_published == 11) %>% 
  select(-month_published, - year_published)
test_2019 <- adopted_2019 %>% filter(month_published == 12) %>%
  select(-month_published, - year_published)


fit_rf_1 <- ranger(less_than_30_days~ .,
                 data = train_2019, num.tree = 1000, probability = TRUE )
# summary(fit_rf_1)

fit_pred_1 <- predict(fit_rf_1, data = test_2019)

test_2019$pred_prob_rf_1 <- predict(fit_rf_1, data = test_2019)$predictions[,2]


pred_test_data_rf_1 <- prediction(test_2019$pred_prob_rf_1,test_2019$less_than_30_days)
pred_test_data_rf_1  <- performance(pred_test_data_rf_1, 'auc')
test_rf_auc_1 <- pred_test_data_rf_1@y.values[[1]]
test_rf_auc_1  

test_2020_1 <- adopted_2020 %>%
  select(-month_published, - year_published)
test_2020_1$pred_prob_rf_1 <- predict(fit_rf_1, data = test_2020_1)$predictions[,2]


pred_test_data_rf_1_2020 <- prediction(test_2020_1$pred_prob_rf_1,test_2020_1$less_than_30_days)
pred_test_data_rf_1_2020 <- performance(pred_test_data_rf_1_2020, 'auc')
test_rf_auc_1_2020 <- pred_test_data_rf_1_2020@y.values[[1]]
test_rf_auc_1_2020  



