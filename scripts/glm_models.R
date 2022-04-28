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

adopted_2019 <- read_csv('./data/adopted_2019_q4_cleaned.csv' )
adopted_2020 <- read_csv('./data/adopted_2020_q4_cleaned.csv' )

# Train/Test set1
train_2019 <- adopted_2019 %>% 
  filter(month_published == 10 | month_published == 11) %>% select(-month_published, - year_published)

test_2019 <- adopted_2019 %>% filter(month_published == 12)%>% select(-month_published, - year_published)

# Train/Test set 2

adopted_2019 <- adopted_2019 %>% select(-month_published, - year_published)
adopted_2020 <- adopted_2020 %>% select(-month_published, - year_published)
###########################GLM########################
# Set 1
model_glm1 <- glm(less_than_30_days~.,data = train_2019, family = binomial())

test_glm1 <- test_2019  %>% mutate(predicted.probability =  
                                        predict(model_glm1, test_2019, type='response'))

test.pred <- prediction(test_glm1$predicted.probability, test_glm1$less_than_30_days)
test.perf <- performance(test.pred, "auc")
auc_glm1 <- test.perf@y.values[[1]]
auc_glm1 # 0.66

# Set 2
model_glm2 <- glm(less_than_30_days~.,data = adopted_2019, family = binomial())

test_glm2 <- adopted_2020  %>% mutate(predicted.probability =  
                                        predict(model_glm2, adopted_2020, type='response'))

test.pred <- prediction(test_glm2$predicted.probability, test_glm2$less_than_30_days)
test.perf <- performance(test.pred, "auc")
auc_glm2 <- test.perf@y.values[[1]]
auc_glm2 # 0.65

###########################Lasso################################
# Set 1
train_lasso1 <- train_2019 %>% select(-less_than_30_days)
train_lasso1  <- data.matrix(train_lasso1 )
train_lasso1_outcome <- train_2019$less_than_30_days
train_lasso1_outcome <- data.matrix(train_lasso1_outcome)

cv_model <-cv.glmnet(train_lasso1,train_lasso1_outcome,alpha =1)
l1 <- cv_model$lambda.min

model_lasso1 <- glmnet(train_lasso1,train_lasso1_outcome,alpha = 1, lambda = l1)
coef(model_lasso1)
# None of predictor is dropped

test_lasso1 <- test_2019 %>% select(-less_than_30_days)
test_lasso1 <- data.matrix(test_lasso1)
test_lasso1_outcome <- test_2019$less_than_30_days
test_lasso1_outcome <- data.matrix(test_lasso1_outcome)
result_lasso1 <- predict(model_lasso1,s=l1,newx = test_lasso1)

test.pred <- prediction(result_lasso1, test_lasso1_outcome)
test.perf <- performance(test.pred, "auc")
auc_lasso1 <- test.perf@y.values[[1]]
auc_lasso1 # 0.61
# Set 2
test_2019 <- adopted_2019 %>% select(-less_than_30_days)
test_2019 <- data.matrix(test)
test_2019_outcome <- adopted_2019$less_than_30_days
test_2019_outcome <- data.matrix(test_2019_outcome)

cv_model <-cv.glmnet(test_2019,test_2019_outcome,alpha =1)
l2 <- cv_model$lambda.min

model_lasso2 <- glmnet(test_2019,test_2019_outcome,alpha = 1, lambda = l2)
coef(model_lasso2)
# environment.children is dropped

test_2020 <- adopted_2020 %>% select(-less_than_30_days)
test_2020 <- data.matrix(test_2020)
test_2020_outcome <- adopted_2020$less_than_30_days
result_2020<- predict(model_lasso,s=l2,newx = test_2020)

test.pred <- prediction(result_2020, test_2020_outcome)
test.perf <- performance(test.pred, "auc")
auc_lasso2 <- test.perf@y.values[[1]]
auc_lasso2 # 0.60


#######COMPARISON######
#????????????????????????
anova(model_glm1,model_lasso1)
#######################################recall-at-k%####################################################
# recall-at-k% plot glm set1
plot.data_glm <- test_glm1  %>% arrange( desc(predicted.probability) ) %>%
  mutate(nums = row_number(), percent.outcome = cumsum(less_than_30_days)/sum(less_than_30_days),
         perc = nums/n()) %>% select(perc, percent.outcome)

# recall-at-k% plot lasso set1
test_2019$predicted.probability <- result_lasso1
plot.data_lasso <- test_2019  %>% arrange( desc(predicted.probability) ) %>%
  mutate(nums = row_number(), percent.outcome = cumsum(less_than_30_days)/sum(less_than_30_days),
         perc = nums/n()) %>% select(perc, percent.outcome)

theme_set(theme_bw())

plot.data_glm$group <- "glm"
plot.data_lasso$group <- "lasso"
p1_glm_lasso <- rbind(plot.data_glm,plot.data_lasso)

p1_glm_lasso <- ggplot(data=p1_glm_lasso, aes(x=perc, y=percent.outcome,color = group))
p1_glm_lasso <- p1_glm_lasso + geom_line() + ggtitle("Recall-at-k% plot")
p1_glm_lasso <- p1_glm_lasso + scale_x_log10('\nPercent of pets', limits=c(0.003, 1), breaks=c(.003,.01,.03,.1,.3,1),
                                             labels=c('0.3%','1%','3%','10%','30%','100%'))
p1_glm_lasso <- p1_glm_lasso + scale_y_continuous("Percent of pets aopted within 30 days", limits=c(0, 1), labels=scales::percent)
p1_glm_lasso

#######################################CALIBRATION PLOT ##################################################

plot.data_glm_cali <- test_glm1 %>% mutate(calibration = round(100*predicted.probability)) %>%
  group_by(calibration) %>% summarize(model.estimate = mean(predicted.probability),
                                      nums = n(),
                                      empirical.estimate = mean(less_than_30_days))

p2 <- ggplot(data = plot.data_glm_cali, aes(y=empirical.estimate, x=model.estimate))
p2 <- p2 + geom_point(alpha=0.5, aes(size=nums)) + ggtitle("Calibration Plot")
p2 <- p2 + scale_size_area(guide='none', max_size=15)
p2 <- p2 + geom_abline(intercept=0, slope=1, linetype="dashed")
p2 <- p2 + scale_y_log10('Empirical probability \n', limits=c(.001,1), breaks=c(.001,.003,.01,.03,.1,.3,1),
                         labels=c('0.1%','0.3%','1%','3%','10%','30%','100%'))
p2 <- p2 + scale_x_log10('\nModel estimated probability', limits=c(.001,1), breaks=c(.001,.003,.01,.03,.1,.3,1),
                         labels=c('0.1%','0.3%','1%','3%','10%','30%','100%'))
p2
#ggsave(plot = p2, path = "../figures", filename = "calibration.png",height=5, width=5)

p_test <- ggplot(data = plot.data_glm_cali, aes(y=empirical.estimate, x=model.estimate))
p_test <- p_test + geom_point(alpha=0.5, aes(size=nums)) + ggtitle("Calibration Plot")
p_test <- p_test + scale_size_area(guide='none', max_size=15)
p_test <- p_test + geom_abline(intercept=0, slope=1, linetype="dashed")
p_test <- p_test + scale_y_log10('Empirical probability \n', limits=c(.1,1), breaks=c(.1,.5,1),
                         labels=c('10%','50%','100%'))
p_test <- p_test + scale_x_log10('\nModel estimated probability', limits=c(.1,1), breaks=c(.1,.5,1),
                         labels=c('10%','50%','100%'))
p_test














