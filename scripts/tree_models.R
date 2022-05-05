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
library(caret)
library(xgboost)

set.seed(2034)

adopted_2019 <- read_csv('./data/adopted_2019_q4_cleaned.csv' )
adopted_2020 <- read_csv('./data/adopted_2020_q4_cleaned.csv' )


############################ RF split train test sets ############################
#### train 2019 oct and nov, test 2019 dec 
train_2019 <- adopted_2019 %>% 
  filter(month_published == 10 | month_published == 11) %>% 
  select(-month_published, - year_published)
test_2019 <- adopted_2019 %>% filter(month_published == 12) %>%
  select(-month_published, - year_published)

#### 80/20 splits on 2019 
split_size <- floor(0.8*nrow(adopted_2019))

shuffled_2019 <- adopted_2019 %>% 
  dplyr::slice(sample(1:n()))

train_2019_80 <- shuffled_2019 %>%
  dplyr::slice(1:split_size) %>%
  select(-month_published, - year_published)

test_2019_20 <- adopted_2019 %>% 
  dplyr::slice(split_size:n()) %>%
  select(-month_published, - year_published)


################################################ method 1 ################################################

#### fit rf model for method 1
fit_rf_1 <- ranger(less_than_30_days~ .,
                 data = train_2019, num.tree = 100, 
                 probability = TRUE, importance = 'impurity')

# fit_rf_altmann <- ranger(less_than_30_days~ .,
#                    data = train_2019, num.tree = 100, 
#                    probability = TRUE, importance = 'permutation')

# summary(fit_rf_1)

# fit_pred_1 <- predict(fit_rf_1, data = test_2019)

test_2019_rf <- test_2019
test_2019_rf$pred_prob_rf_1 <- predict(fit_rf_1, data = test_2019_rf)$predictions[,2]

#### auc of fit_2019_1011 on 12.2019
pred_test_data_rf_1 <- prediction(test_2019_rf$pred_prob_rf_1,test_2019_rf$less_than_30_days)
pred_test_data_rf_1  <- performance(pred_test_data_rf_1, 'auc')
test_rf_auc_1 <- pred_test_data_rf_1@y.values[[1]]
test_rf_auc_1  ## 0.66


#### check metrics 2019

test_2019_rf <- test_2019_rf %>% mutate(pred_0.5 = ifelse(pred_prob_rf_1 >= 0.5, 1, 0))
confusionMatrix(as.factor(test_2019_rf$pred_0.5),as.factor(test_2019_rf$less_than_30_days))
#### Accuracy : 0.6675; Specificity : 0.8051


# import_pvals_altmann <- importance_pvalues(fit_rf_altmann, method = 'altmann', 
#                    data = test_2019, formula = less_than_30_days~.)


#### fit_2019_1011, fit 2020
test_2020_1_rf <- adopted_2020 %>%
  select(-month_published, - year_published)

test_2020_1_rf$pred_prob_rf_1 <- predict(fit_rf_1, data = test_2020_1_rf)$predictions[,2]

#### auc of fit_2019_1011 on 10,11,12.2020
pred_test_data_rf_1_2020 <- prediction(test_2020_1_rf$pred_prob_rf_1,test_2020_1_rf$less_than_30_days)
pred_test_data_rf_1_2020 <- performance(pred_test_data_rf_1_2020, 'auc')
test_rf_auc_1_2020 <- pred_test_data_rf_1_2020@y.values[[1]]
test_rf_auc_1_2020  ##0.64

#### 2020 
test_2020_1_rf <- test_2020_1_rf %>% mutate(pred_0.5 = ifelse(pred_prob_rf_1 >= 0.5, 1, 0))
confusionMatrix(as.factor(test_2020_1_rf$pred_0.5),as.factor(test_2020_1_rf$less_than_30_days)) 
#### Accuracy : 0.6611; Specificity : 0.7590


################################################ method 2 ################################################
#### 80/20 split train fit
#### fit rf model for method 1
fit_rf_2 <- ranger(less_than_30_days~ .,
                   data = train_2019_80, num.tree = 100, 
                   probability = TRUE, importance = 'impurity')

# fit_rf_altmann <- ranger(less_than_30_days~ .,
#                          data = train_2019, num.tree = 100, 
#                          probability = TRUE, importance = 'permutation')

# summary(fit_rf_1)

# fit_pred_1 <- predict(fit_rf_1, data = test_2019)
test_2019_20_rf <- test_2019_20

test_2019_20_rf$pred_prob_rf_2 <- predict(fit_rf_2, data = test_2019_20_rf)$predictions[,2]

#### auc of fit_2019_1011 on 12.2019
pred_test_data_rf_2 <- prediction(test_2019_20_rf$pred_prob_rf_2,test_2019_20_rf$less_than_30_days)
pred_test_data_rf_2  <- performance(pred_test_data_rf_2, 'auc')
test_rf_auc_2 <- pred_test_data_rf_2@y.values[[1]]
test_rf_auc_2  ## 0.68


#### test on 2020

test_2020_1_rf$pred_prob_rf_2 <- predict(fit_rf_2, data = test_2020_1_rf)$predictions[,2]

#### auc of fit_2019_1011 on 10,11,12.2020

pred_test_data_rf_2_2020 <- prediction(test_2020_1_rf$pred_prob_rf_2,test_2020_1_rf$less_than_30_days)
pred_test_data_rf_2_2020 <- performance(pred_test_data_rf_2_2020, 'auc')
test_rf_auc_2_2020 <- pred_test_data_rf_2_2020@y.values[[1]]
test_rf_auc_2_2020  ## 0.64


#### check other metrics

test_2019_20_rf <- test_2019_20_rf %>%
  mutate(pred_5 = ifelse(pred_prob_rf_2 >= 0.5, 1, 0))

test_2020_1_rf <- test_2020_1_rf%>%
  mutate(pred_5 = ifelse(pred_prob_rf_2 >= 0.5, 1, 0))

#### 2019
confusionMatrix(as.factor(test_2019_20_rf$pred_5),as.factor(test_2019_20_rf$less_than_30_days))
####Accuracy : 0.6849; Specificity : 0.8997

#### 2020
confusionMatrix(as.factor(test_2020_1_rf$pred_5),as.factor(test_2020_1_rf$less_than_30_days))
#### Accuracy : 0.6772; Specificity : 0.8059


#### Graphs 
rf_plot_dat <- test_2019_rf 
rf_plot_dat_20 <- test_2019_20_rf


rf_precision_plot_1 <- test_2019_rf  %>% 
  ungroup() %>%
  arrange(desc(pred_prob_rf_1)) %>%
  mutate(numrest = row_number(),
         real_outcome = cumsum(as.numeric(as.character(less_than_30_days))),
         percent_outcome = real_outcome/numrest)

rf_precision_plot_2 <- test_2019_20_rf %>% 
  ungroup() %>%
  arrange(desc(pred_prob_rf_2)) %>%
  mutate(numrest = row_number(),
         real_outcome = cumsum(as.numeric(as.character(less_than_30_days))),
         percent_outcome = real_outcome/numrest)

rf_precision_plot_3 <- test_2020_1_rf %>% 
  ungroup() %>%
  arrange(desc(pred_prob_rf_1)) %>%
  mutate(numrest = row_number(),
         real_outcome = cumsum(as.numeric(as.character(less_than_30_days))),
         percent_outcome = real_outcome/numrest)

rf_precision_plot_4 <- test_2020_1_rf %>% 
  ungroup() %>%
  arrange(desc(pred_prob_rf_2)) %>%
  mutate(numrest = row_number(),
         real_outcome = cumsum(as.numeric(as.character(less_than_30_days))),
         percent_outcome = real_outcome/numrest)

  

precision_rf_1 <- ggplot() + 
  geom_line(data = rf_precision_plot_1, aes(x=numrest, y=percent_outcome, color = "method1")) +    # method 1 2019
  geom_line(data = rf_precision_plot_2, aes(x=numrest, y=percent_outcome, color = "method2")) +  # 80/20 split. 2019
  labs(x= 'Number of Animals', y = 'Model Precision', title = 'Precision at k curve') +
  scale_color_manual(values = c("method1" = "darkblue", "method2" = "indianred"), name = "methods") 

# ggsave(filename = './figures/precision_rf_2019.png', plot = precision_rf_1)

precision_rf_2 <- ggplot() + 
  geom_line(data = rf_precision_plot_3, aes(x=numrest, y=percent_outcome, color = "method1")) +    # method 1 2020
  geom_line(data = rf_precision_plot_4, aes(x=numrest, y=percent_outcome, color = "method2")) +  # 80/20 split 2020
  labs(x= 'Number of Animals', y = 'Model Precision', title = 'Precision at k curve') + 
  scale_color_manual(values = c("method1" = "darkblue", "method2" = "indianred"), name = "methods") 

# ggsave(filename = './figures/precision_rf_2020.png', plot = precision_rf_2)

###


fit_rf_1_corrected <- ranger(less_than_30_days~ .,
                             data = train_2019, num.tree = 100, 
                             probability = TRUE, importance = 'impurity_corrected')

import_pvalues <- importance_pvalues(fit_rf_1_corrected)
import_pvalues 


################################################  XGBOOST  ################################################
################################################ method 1 ################################################
#### train on 1011, test on 12
#### data split 
train_2019_xgb <- select(train_2019, -less_than_30_days)
test_2019_xgb <- select(test_2019, -less_than_30_days)



fit_xgb_1 <- xgboost(data = data.matrix(train_2019_xgb), 
                     label = train_2019$less_than_30_days, 
                     max.depth = 5, eta = .5, min_child_weigth = 2,  nrounds = 30,
                     objective = "binary:logistic")

xgb_pred_prob_1 <- predict(fit_xgb_1, data.matrix(test_2019_xgb))


#### check metrics 

importance_matrix <- xgb.importance(model = fit_xgb_1)
print(importance_matrix)

png('./figures/xgb_importance_plot.png', width = 960, height = 640)
xgb.plot.importance(importance_matrix = importance_matrix)


xgb_pred_prob_1_5 <- ifelse(xgb_pred_prob_1 >= 0.5, 1, 0)



#### auc of fit_2019_1011 on 12.2019

pred_test_data_xgb_2019 <- prediction(xgb_pred_prob_1, test_2019$less_than_30_days)
pred_test_data_xgb_2019 <- performance(pred_test_data_xgb_2019, 'auc')
pred_test_data_xgb_2019 <- pred_test_data_xgb_2019@y.values[[1]]
pred_test_data_xgb_2019 ## 0.66


confusionMatrix(as.factor(xgb_pred_prob_1_5),as.factor(test_2019$less_than_30_days)) 
#### Accuracy : 0.6723; Specificity : 0.8110


### graphs 
xgb_plot_dat <- test_2019 
xgb_plot_dat$predicted_prob <- xgb_pred_prob_1

xgb_precision_plot <- xgb_plot_dat %>% 
  ungroup() %>%
  arrange(desc(predicted_prob )) %>%
  mutate(numrest = row_number(),
         real_outcome = cumsum(as.numeric(as.character(less_than_30_days))),
         percent_outcome = real_outcome/numrest)

   
precision_xgb <- ggplot() + 
  geom_line(data = xgb_precision_plot, aes(x=numrest, y=percent_outcome), color = 'red') +
  labs(x= 'Number of Animals', y = 'Model Precision', title = 'Precision at k curve')

# ggsave(filename = './figures/precision_xgb.png', plot = precision_xgb)

# #### fine tuning 
# 
# fit_xgb_2 <- xgboost(data = data.matrix(train_2019_xgb), 
#                      label = train_2019$less_than_30_days, 
#                      max.depth = 5, eta = 0.5, nthread = 2, nrounds = 20, 
#                      objective = "binary:logistic")
# 
# xgb_pred_prob_2 <- predict(fit_xgb_2, data.matrix(test_2019_xgb))
# 
# importance_matrix_2 <- xgb.importance(model = fit_xgb_2)
# print(importance_matrix_2)
# 
# xgb.plot.importance(importance_matrix = importance_matrix_2)
# 
# xgb_pred_prob_2_5 <- ifelse(xgb_pred_prob_2 >= 0.5, 1, 0)
# confusionMatrix(as.factor(xgb_pred_prob_2_5),as.factor(test_2019$less_than_30_days))

#### check the chi-sq
# c2 <- chisq.test(adopted_2019$attributes.house_trained, adopted_2019$less_than_30_days)
# print(c2)

