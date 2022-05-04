##################################################
###
### author: Angela He & Mingyao Xu
### purpose: fit logistic models (glm & lasso)
### input: adopted_2019_q4_cleaned.csv
###        adopted_2020_q4_cleaned.csv
### output: figures
###
################################################
library(tidyverse)
library(lubridate)
library(glmnet)
library(ROCR)
library(caret)
library(rcompanion)
# Read in data
adopted_2019 <- read_csv('./data/adopted_2019_q4_cleaned.csv' )
adopted_2020 <- read_csv('./data/adopted_2020_q4_cleaned.csv' )

# Train/Test set1
train_2019 <- adopted_2019 %>% 
  filter(month_published == 10 | month_published == 11) %>% select(-month_published, - year_published)
test_2019 <- adopted_2019 %>% filter(month_published == 12)%>% select(-month_published, - year_published)

# Train/Test set 2
adopted_2019 <- adopted_2019 %>% select(-month_published, - year_published)
adopted_2020 <- adopted_2020 %>% select(-month_published, - year_published)
#################################################GLM##############################################
# Set 1
# Train glm
model_glm1 <- glm(less_than_30_days~.,data = train_2019, family = binomial())
# Predict
test_glm1 <- test_2019  %>% mutate(predicted.probability =  
                                        predict(model_glm1, test_2019, type='response'))
# AUC
test.pred <- prediction(test_glm1$predicted.probability, test_glm1$less_than_30_days)
test.perf <- performance(test.pred, "auc")
auc_glm1 <- test.perf@y.values[[1]]
auc_glm1 # 0.66
# Summary of glm1
summary(model_glm1)
model_coef <- exp(model_glm1$coefficients)
options(scipen=999)
model_coef

# Set 2
# Train glm with set 2
model_glm2 <- glm(less_than_30_days~.,data = adopted_2019, family = binomial())
# Predict
test_glm2 <- adopted_2020  %>% mutate(predicted.probability =  
                                        predict(model_glm2, adopted_2020, type='response'))
# AUC
test.pred <- prediction(test_glm2$predicted.probability, test_glm2$less_than_30_days)
test.perf <- performance(test.pred, "auc")
auc_glm2 <- test.perf@y.values[[1]]
auc_glm2 # 0.65

# Set 3
# Train glm with 5 predictors from random tree
model_glm3 <- glm(less_than_30_days~.,data = select(train_2019,attributes.house_trained,age,environment.cats,contacts,breeds_bin,less_than_30_days), family = binomial())
# Predict
test_glm3 <- test_2019  %>% mutate(predicted.probability =  
                                        predict(model_glm3, test_2019, type='response'))
# AUC
test.pred <- prediction(test_glm3$predicted.probability, test_glm3$less_than_30_days)
test.perf <- performance(test.pred, "auc")
auc_glm3 <- test.perf@y.values[[1]]
auc_glm3 # 0.65

# Comparison between model_glm1 and model_glm3 using confusion matrix and AIC, BIC
test_glm1<- test_glm1%>%
  mutate(pred_5 = ifelse(predicted.probability >= 0.5, 1, 0))
confusionMatrix(as.factor(test_glm1$pred_5),as.factor(test_glm1$less_than_30_days))

test_glm3<- test_glm3%>%
  mutate(pred_5 = ifelse(predicted.probability >= 0.5, 1, 0))
confusionMatrix(as.factor(test_glm3$pred_5),as.factor(test_glm3$less_than_30_days))

compareGLM(model_glm1,model_glm3)
#######################################Lasso############################################
# Set 1
# Change the form from data frame to matrix
train_lasso1 <- train_2019 %>% select(-less_than_30_days)
train_lasso1  <- data.matrix(train_lasso1 )
train_lasso1_outcome <- train_2019$less_than_30_days
train_lasso1_outcome <- data.matrix(train_lasso1_outcome)
# Find optimal lambda
cv_model <-cv.glmnet(train_lasso1,train_lasso1_outcome,alpha =1)
l1 <- cv_model$lambda.min
# Train lasso model
model_lasso1 <- glmnet(train_lasso1,train_lasso1_outcome,alpha = 1, lambda = l1)
coef(model_lasso1) # None of predictors is dropped
# Predict
test_2019_list <- test_2019 %>% select(-less_than_30_days)
test_2019_list <- data.matrix(test_2019_list)
test_2019_outcome <- test_2019$less_than_30_days
result_2019<- predict(model_lasso1,s=l1,newx = test_2019_list)
# AUC
test.pred <- prediction(result_2019, test_2019_outcome)
test.perf <- performance(test.pred, "auc")
auc_lasso1 <- test.perf@y.values[[1]]
auc_lasso1 # 0.61

# Comparison between model_glm1 and model_lasso1 using confusion matrix
test_glm1<- test_glm1%>%
  mutate(pred_5 = ifelse(predicted.probability >= 0.5, 1, 0))
confusionMatrix(as.factor(test_glm1$pred_5),as.factor(test_glm1$less_than_30_days))

test_lasso1 <- result_2019
pred_5 <- ifelse(result_2019 >= 0.5, 1, 0)
confusionMatrix(as.factor(pred_5),as.factor(test_2019_outcome))

#######################################RECALL AT K% GLM & LASSO################################################
# recall-at-k% plot glm set1
plot.data_glm <- test_glm1  %>% arrange( desc(predicted.probability) ) %>%
  mutate(nums = row_number(), percent.outcome = cumsum(less_than_30_days)/sum(less_than_30_days),
         perc = nums/n()) %>% select(perc, percent.outcome)

# recall-at-k% plot lasso set1
test_2019$predicted.probability <- result_2019
plot.data_lasso <- test_2019  %>% arrange( desc(predicted.probability) ) %>%
  mutate(nums = row_number(), percent.outcome = cumsum(less_than_30_days)/sum(less_than_30_days),
         perc = nums/n()) %>% select(perc, percent.outcome)

theme_set(theme_bw())
# Create group column
plot.data_glm$group <- "glm"
plot.data_lasso$group <- "lasso"

# Combine plot data
p1_glm_lasso <- rbind(plot.data_glm,plot.data_lasso)
# Plot 
p1_glm_lasso <- ggplot(data=p1_glm_lasso, aes(x=perc, y=percent.outcome,color = group))
p1_glm_lasso <- p1_glm_lasso + geom_line() + ggtitle("Recall-at-k% plot")
p1_glm_lasso <- p1_glm_lasso + scale_x_log10('\nPercent of pets', limits=c(0.003, 1), breaks=c(.003,.01,.03,.1,.3,1),
                                             labels=c('0.3%','1%','3%','10%','30%','100%'))
p1_glm_lasso <- p1_glm_lasso + scale_y_continuous("Recall", limits=c(0, 1), labels=scales::percent)
p1_glm_lasso

#######################################RECALL AT K% GLM1 & 3################################################
# Plot data for glm1
plot.data_glm1 <- test_glm1  %>% arrange( desc(predicted.probability) ) %>%
  mutate(nums = row_number(), percent.outcome = cumsum(less_than_30_days)/sum(less_than_30_days),
         perc = nums/n()) %>% select(perc, percent.outcome)
# Plot data for glm3
plot.data_glm3 <- test_glm3  %>% arrange( desc(predicted.probability) ) %>%
  mutate(nums = row_number(), percent.outcome = cumsum(less_than_30_days)/sum(less_than_30_days),
         perc = nums/n()) %>% select(perc, percent.outcome)
# Create group column
plot.data_glm1$group <- "glm1"
plot.data_glm3$group <- "glm3"
# Combine and plot
p1_glm_comb<- rbind(plot.data_glm1,plot.data_glm3)

p1_glm_comb <- ggplot(data=p1_glm_comb, aes(x=perc, y=percent.outcome,color = group))
p1_glm_comb <- p1_glm_comb + geom_line() + ggtitle("Recall-at-k% plot")
p1_glm_comb <- p1_glm_comb + scale_x_log10('\nPercent of pets', limits=c(0.003, 1), breaks=c(.003,.01,.03,.1,.3,1),
                                             labels=c('0.3%','1%','3%','10%','30%','100%'))
p1_glm_comb <- p1_glm_comb + scale_y_continuous("Percent of pets aopted within 30 days", limits=c(0, 1), labels=scales::percent)
p1_glm_comb

#######################################CALIBRATION PLOT GLM1 AND LASSO ##########################################
# Plot data for glm1
plot.data_glm1_cali <- test_glm1 %>% mutate(calibration = round(100*predicted.probability)) %>%
  group_by(calibration) %>% summarize(model.estimate = mean(predicted.probability),
                                      nums = n(),
                                      empirical.estimate = mean(less_than_30_days))
# Plot data for lasso
test_lasso1 <- data.frame(result_2019,test_2019_outcome)
test_lasso1 <- rename(test_lasso1,predicted.probability=s1,less_than_30_days=test_2019_outcome)

plot.data_lasso_cali <- test_lasso1 %>% mutate(calibration = round(100*predicted.probability)) %>%
  group_by(calibration) %>% summarize(model.estimate = mean(predicted.probability),
                                      nums = n(),
                                      empirical.estimate = mean(less_than_30_days))
# Group and combine
plot.data_glm1_cali$group <- "glm1"
plot.data_lasso_cali$group <- "lasso"

plot.data_cali <-rbind(plot.data_glm1_cali,plot.data_lasso_cali)
# Plot
p2 <- ggplot(data = plot.data_cali, aes(y=empirical.estimate, x=model.estimate,color = group))
p2 <- p2 + geom_point(alpha=0.5, aes(size=nums)) + ggtitle("Calibration Plot")
p2 <- p2 + scale_size_area(guide='none', max_size=15)
p2 <- p2 + geom_abline(intercept=0, slope=1, linetype="dashed")
p2 <- p2 +scale_y_log10('Empirical probability \n', limits=c(.1,1), breaks=c(.1,.5,1),
                        labels=c('10%','50%','100%'))
p2 <- p2 + scale_x_log10('\nModel estimated probability', limits=c(.1,1), breaks=c(.1,.5,1),
                         labels=c('10%','50%','100%'))
p2













