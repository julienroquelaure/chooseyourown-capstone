# Setup
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)


dl <- tempfile()

download.file("https://archive.ics.uci.edu/ml/machine-learning-databases/00222/bank-additional.zip", dl)

calls <- fread(text = gsub("::", "\t", readLines(unzip(dl, "bank-additional/bank-additional-full.csv"))))


# Partition training/test sets
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = calls$y, times = 1, p = 0.1, list = FALSE)
calls_train <- calls[-test_index,]
calls_test <- calls[test_index,]
x_train <- calls_train %>%
  select(-y)
y_train <- calls_train$y
x_test <- calls_test %>%
  select(-y)
y_test <- calls_test$y

#age variable

calls_train %>%
  ggplot(aes(age, fill=as.factor(y))) +
  geom_bar()
age_structure <-calls_train %>%
  group_by(age) %>%
  summarize(n=n(), share = sum(y=="yes") / (sum(y=="yes")+sum(y=="no")))
age_structure %>% 
  ggplot(aes(x=age, y=share)) + geom_line()
age_structure %>% 
  ggplot(aes(x=age, y=n)) + geom_line()


#job variable
calls_train %>%
  ggplot(aes(job, fill=as.factor(y))) +
  geom_bar()
calls_train %>%
  group_by(job) %>%
  summarize(share = sum(y=="yes") / (sum(y=="yes")+sum(y=="no")))


#marital variable
calls_train %>%
  ggplot(aes(marital, fill=as.factor(y))) +
  geom_bar()
calls_train %>%
  group_by(marital) %>%
  summarize(share = sum(y=="yes") / (sum(y=="yes")+sum(y=="no")))

#education
calls_train %>%
  ggplot(aes(education, fill=as.factor(y))) +
  geom_bar()
calls_train %>%
  group_by(education) %>%
  summarize(share = sum(y=="yes") / (sum(y=="yes")+sum(y=="no")))

#default
calls_train %>%
  ggplot(aes(default, fill=as.factor(y))) +
  geom_bar()
sum(calls$default == "yes")
calls_train %>%
  group_by(default) %>%
  summarize(share = sum(y=="yes") / (sum(y=="yes")+sum(y=="no")))

#housing
calls_train %>%
  ggplot(aes(housing, fill=as.factor(y))) +
  geom_bar()
calls_train %>%
  group_by(housing) %>%
  summarize(share = sum(y=="yes") / (sum(y=="yes")+sum(y=="no")))

#loan
calls_train %>%
  ggplot(aes(loan, fill=as.factor(y))) +
  geom_bar()
calls_train %>%
  group_by(loan) %>%
  summarize(share = sum(y=="yes") / (sum(y=="yes")+sum(y=="no")))

#contact
calls_train %>%
  ggplot(aes(contact, fill=as.factor(y))) +
  geom_bar()
calls_train %>%
  group_by(contact) %>%
  summarize(share = sum(y=="yes") / (sum(y=="yes")+sum(y=="no")))

#month
calls_train %>%
  ggplot(aes(month, fill=as.factor(y))) +
  geom_bar()
calls_train %>%
  group_by(month) %>%
  summarize(share = sum(y=="yes") / (sum(y=="yes")+sum(y=="no")))



#day
calls_train %>%
  ggplot(aes(day_of_week, fill=as.factor(y))) +
  geom_bar()
calls_train %>%
  group_by(day_of_week) %>%
  summarize(share = sum(y=="yes") / (sum(y=="yes")+sum(y=="no")))

#duration
calls_train %>%
  ggplot(aes(duration, fill=as.factor(y))) +
  geom_bar()
duration_structure <- calls_train %>%
  mutate(interval = (duration - duration%%60)/60) %>%
  group_by(interval) %>%
  summarize(share = sum(y=="yes") / (sum(y=="yes")+sum(y=="no")))
duration_structure %>% 
  ggplot(aes(x=(interval), y=share)) + geom_line()


#campaign
calls_train %>%
  ggplot(aes(campaign, fill=as.factor(y))) +
  geom_bar() 
campaign_structure <- calls_train %>%
  group_by(campaign) %>%
  summarize(share = sum(y=="yes") / (sum(y=="yes")+sum(y=="no")))
campaign_structure %>% 
  ggplot(aes(x=(campaign), y=share)) + geom_line()

#pdays
calls_train %>%
  ggplot(aes(pdays, fill=as.factor(y))) +
  geom_bar() 
pdays_structure <- calls_train %>%
  filter(pdays<999) %>%
  group_by(pdays) %>%
  summarize(share = sum(y=="yes") / (sum(y=="yes")+sum(y=="no")))
pdays_structure %>% 
  ggplot(aes(x=(pdays), y=share)) + geom_line()

#previous
calls_train %>%
  ggplot(aes(previous, fill=as.factor(y))) +
  geom_bar() 
calls_train %>%
  group_by(previous) %>%
  summarize(share = sum(y=="yes") / (sum(y=="yes")+sum(y=="no")))


#poutcome
calls_train %>%
  ggplot(aes(poutcome, fill=as.factor(y))) +
  geom_bar() 
calls_train %>%
  group_by(poutcome) %>%
  summarize(share = sum(y=="yes") / (sum(y=="yes")+sum(y=="no")))

#emp.var.rate
calls_train %>%
  ggplot(aes(emp.var.rate, fill=as.factor(y))) +
  geom_bar() 
calls_train %>%
  group_by(emp.var.rate) %>%
  summarize(share = sum(y=="yes") / (sum(y=="yes")+sum(y=="no")))

#cons.price.idx
calls_train %>%
  ggplot(aes(cons.price.idx, fill=as.factor(y))) +
  geom_bar() 
calls_train %>%
  group_by(cons.price.idx) %>%
  summarize(share = sum(y=="yes") / (sum(y=="yes")+sum(y=="no"))) %>% 
  ggplot(aes(x=(cons.price.idx), y=share)) + geom_line()

#cons.conf.idx
calls_train %>%
  ggplot(aes(cons.conf.idx, fill=as.factor(y))) +
  geom_bar() 
calls_train %>%
  group_by(cons.conf.idx) %>%
  summarize(share = sum(y=="yes") / (sum(y=="yes")+sum(y=="no"))) %>% 
  ggplot(aes(x=(cons.conf.idx), y=share)) + geom_line()

#euribor3m
calls_train %>%
  ggplot(aes(euribor3m, fill=as.factor(y))) +
  geom_bar() 
calls_train %>%
  group_by(euribor3m) %>%
  summarize(share = sum(y=="yes") / (sum(y=="yes")+sum(y=="no"))) %>% 
  ggplot(aes(x=(euribor3m), y=share)) + geom_line()

#nr.employed
calls_train %>%
  ggplot(aes(nr.employed, fill=as.factor(y))) +
  geom_bar() 
calls_train %>%
  group_by(nr.employed) %>%
  summarize(share = sum(y=="yes") / (sum(y=="yes")+sum(y=="no")))



#month to number
calls$month[which(calls$month=="mar")] <- 3
calls$month[which(calls$month=="apr")] <- 4
calls$month[which(calls$month=="may")] <- 5
calls$month[which(calls$month=="jun")] <- 6
calls$month[which(calls$month=="jul")] <- 7
calls$month[which(calls$month=="aug")] <- 8
calls$month[which(calls$month=="sep")] <- 9
calls$month[which(calls$month=="oct")] <- 10
calls$month[which(calls$month=="nov")] <- 11
calls$month[which(calls$month=="dec")] <- 12
calls$month <- as.numeric(calls$month)


#day to number
calls$day_of_week[which(calls$day_of_week=="mon")] <- 1
calls$day_of_week[which(calls$day_of_week=="tue")] <- 2
calls$day_of_week[which(calls$day_of_week=="wed")] <- 3
calls$day_of_week[which(calls$day_of_week=="thu")] <- 4
calls$day_of_week[which(calls$day_of_week=="fri")] <- 5
calls$day_of_week <- as.numeric(calls$day_of_week)

#Normalization and binarization of the variables

calls_norm <- calls %>%
  mutate(age = ((age-mean(age))/sd(age))**2) %>%
  mutate(job = ifelse(job=="admin.", 1, 0)) %>%
  mutate(marital = ifelse(marital=="single", 1, 0)) %>%
  mutate(education = ifelse(education=="university.degree", 1, 0)) %>%
  mutate(default = ifelse(default=="no", 0, 1)) %>%
  mutate(housing = ifelse(housing=="yes", 1, 0)) %>%
  mutate(loan = ifelse(loan=="yes", 1, 0)) %>%
  mutate(contact = ifelse(contact=="cellular", 1, 0)) %>%
  mutate(duration = (duration-mean(duration))/sd(duration)) %>%
  mutate(campaign = (campaign-mean(campaign))/sd(campaign)) %>%
  mutate(pdays = ifelse(pdays<999, 1 ,0)) %>%
  mutate(previous =  (previous-mean(previous))/sd(previous))%>%
  mutate(poutcome = ifelse(poutcome=="success", 1, 0)) %>%
  mutate(y = ifelse(y=="yes", 1, 0)) %>%
  mutate(emp.var.rate = (emp.var.rate-mean(emp.var.rate))/sd(emp.var.rate)) %>%
  mutate(cons.price.idx = (cons.price.idx-mean(cons.price.idx))/sd(cons.price.idx)) %>%
  mutate(cons.conf.idx = (cons.conf.idx-mean(cons.conf.idx))/sd(cons.conf.idx)) %>%
  mutate(euribor3m = (euribor3m-mean(euribor3m))/sd(euribor3m)) %>%
  mutate(nr.employed = (nr.employed -mean(nr.employed ))/sd(nr.employed ))

#train/test split with the transformed variables

calls_norm_train <- calls_norm[-test_index,]
calls_norm_test <- calls_norm[test_index,]
x_train <- calls_norm_train %>%
  select(-y)
y_train <- as.numeric(calls_norm_train$y)
x_test <- calls_norm_test %>%
  select(-y)
y_test <- as.numeric(calls_norm_test$y)

#Linear model on all variables

train_glm <- train(x_train, y_train, method="glm")
predict_glm <- predict(train_glm, x_test)
predict_glm_bin <- ifelse(predict_glm >.5, 1, 0)
mean(predict_glm_bin == y_test)
train_glm$finalModel

#Linear model on the 5 chosen variables

calls_norm_train <- calls_norm_train %>%
  select(contact, duration, pdays, poutcome, emp.var.rate,  y)
x_train <- calls_norm_train %>%
  select(-y)
y_train <- as.numeric(calls_norm_train$y)
x_test <- calls_norm_test %>%
  select(-y)
y_test <- as.numeric(calls_norm_test$y)


train_glm <- train(x_train, y_train, method="glm")
predict_glm <- predict(train_glm, x_test)
predict_glm_bin <- ifelse(predict_glm >.5, 1, 0)
mean(predict_glm_bin == y_test)

#KNN model with the tuning algorithm in the comments

train_knn <- train(x_train, y_train, method="knn",
                   tuneGrid = data.frame(k = 100))
predict_knn <- predict(train_knn, x_test)
predict_knn_bin <- ifelse(predict_knn >.5, 1, 0)
mean(predict_knn_bin == y_test)

# 
# train_knn <- train(x_train_2, y_train, method="knn",
#                   tuneGrid = data.frame(k = seq(1, 100)))
# predict_knn <- predict(train_knn, x_test_2)
# predict_knn_bin <- ifelse(predict_knn >.5, 1, 0)
# mean(predict_knn_bin == y_test)
# train_knn$bestTune

