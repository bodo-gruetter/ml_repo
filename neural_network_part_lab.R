#####################################
#Bodo Gruetter, bodojeremy.gruetter@stud.hslu.ch
#Necati Van, necati.vanr@stud.hslu.ch
#Cengiz Cetinkaya, cengiz.cetinkaya@stud.hslu.ch
#Malik Sogukoglu, malik.sogukoglu@stud.hslu.ch
#Group 13

########## Import Packages ##########
library(ggplot2)
library(e1071)
library(mgcv)
library(corrplot)
library(tree)
library(mgcv)
library(tidyverse)
library(boot)
library(MASS)
library(ipred)
library(randomForest)
library(gbm)
######## NEURONAL NETWORK ##########
library(neuralnet)
library(h2o)
library(bit64)
library(caret)

########## Prepare Environment ##########
# Set seed for reproducability
set.seed(123)
# clear environment
rm(list = ls())
# set plotting window to default
par(mfrow = c(1, 1))

########## Getting Data ##########
##Load Data
d.bike <- read.csv("bikesharing.csv", header=TRUE)

##Remove instant and dteday
d.bike <- subset(d.bike, select=-c(instant, dteday))

##cast categoricals to factor
d.bike$season <- as.factor(d.bike$season)
d.bike$yr <- as.factor(d.bike$yr)
d.bike$mnth <- as.factor(d.bike$mnth)
d.bike$hr <- as.factor(d.bike$hr)
d.bike$holiday <- as.factor(d.bike$holiday)
d.bike$weekday <- as.factor(d.bike$weekday)
d.bike$workingday <- as.factor(d.bike$workingday)
d.bike$weathersit <- as.factor(d.bike$weathersit)

##Descriptive data analysis
str(d.bike)
head(d.bike)
tail(d.bike)
sum(is.na(d.bike))
mean(is.na(d.bike))

##Creating train and test data set
# set seed
set.seed(123)

# create weighted train (75%) and test (25%) set
d.bike.train.id <- sample(seq_len(nrow(d.bike)),size = floor(0.75*nrow(d.bike)))
d.bike.train <- d.bike[d.bike.train.id,]
d.bike.test <- d.bike[-d.bike.train.id,]  

# Check
nrow(d.bike)
nrow(d.bike.train)
nrow(d.bike.test)
str(d.bike.train)
str(d.bike.test)

######################### Block 2 ########################################## 
########## UPDATING DATASET FOR CLASSIFICATION ##########
##Remove features
str(d.bike)
d.bike.new <- subset(d.bike, select=-c(holiday, weekday, workingday, windspeed, casual, registered))
str(d.bike.new)

# build 5 classes 0 to 4 according to the number cnt
max(d.bike.new$cnt)
max(d.bike.new$cnt)/5
for(i in 1:nrow(d.bike)){
  if(d.bike.new$cnt[i] >= 0 & d.bike.new$cnt[i] < 196){
    d.bike.new$class[i] <- 0
  } else if (d.bike.new$cnt[i] >= 196 & d.bike.new$cnt[i] < 391){
    d.bike.new$class[i] <- 1
  } else if (d.bike.new$cnt[i] >= 391 & d.bike.new$cnt[i] < 586){
    d.bike.new$class[i] <- 2
  } else if (d.bike.new$cnt[i] >= 586 & d.bike.new$cnt[i] < 781){
    d.bike.new$class[i] <- 3
  } else if (d.bike.new$cnt[i] >= 781 & d.bike.new$cnt[i] <= 977){
    d.bike.new$class[i] <- 4
  }
}

# create weighted train (75%) and test (25%) set
d.bike.train.id <- sample(seq_len(nrow(d.bike.new)),size = floor(0.75*nrow(d.bike.new)))
d.bike.train.new <- d.bike.new[d.bike.train.id,]
d.bike.test.new <- d.bike.new[-d.bike.train.id,]

# Check
nrow(d.bike.new)
str(d.bike.new)
nrow(d.bike.train.new)
str(d.bike.train.new)
nrow(d.bike.test.new)
str(d.bike.test.new)



#################### NEURONAL NETWORK ###########################
#This last chapter shows the.... //Todo Text

# set.seed(321)
# (ref=sample(1:dim(d.bike.new)[1], 1, replace=F))
# displayDigit(d.bike.new[ref,-1])

set.seed(10)
training <- d.bike.train.new[,-1]
#training <- d.bike.train.new
test <- d.bike.test.new
test
test_total <- d.bike.new
test_total

h2o.init()
train_hf <- as.h2o(training)
set.seed(100)

model_dl <- h2o.deeplearning(y = 4, training_frame = train_hf, nfolds = 5, seed=250, verbose = F)

# now make a prediction on TRAIN
predicted_h20_deep_neural_network_probabilites_train <- h2o.predict(model_dl, train_hf)
table( as.data.frame(predicted_h20_deep_neural_network_probabilites_train$predict))
predicted_h20_deep_neural_network_class_train <- as.data.frame(predicted_h20_deep_neural_network_probabilites_train$predict)
training
table(training$cnt, predicted_h20_deep_neural_network_class_train[,1])
predicted_h20_deep_neural_network_performance_train <- mean(predicted_h20_deep_neural_network_class_train[,1] == training$cnt)*100
print(paste('training accuracy: ',predicted_h20_deep_neural_network_performance_train,"%"))

# now make a prediction on TEST
test_hf <- as.h2o(test)
predicted_h20_deep_neural_network_probabilites_test <- h2o.predict(model_dl, test_hf)
table( as.data.frame(predicted_h20_deep_neural_network_probabilites_test$predict))
predicted_h20_deep_neural_network_class_test <- as.data.frame(predicted_h20_deep_neural_network_probabilites_test$predict)

table(test$cnt, as.integer(predicted_h20_deep_neural_network_class_test[,1]))
predicted_h20_deep_neural_network_performance_test <- mean(predicted_h20_deep_neural_network_class_test[,1] == test$cnt)*100
print(paste('test accuracy: ',predicted_h20_deep_neural_network_performance_test,"%"))

set.seed(321)
(ref_test=sample(1:dim(test)[1], 20, replace=F))
for (idx in ref_test){
  real_class =test[idx,1]
  prediction = predicted_h20_deep_neural_network_class_test[idx,1]
  correct = (as.integer(real_class)==as.integer(prediction))
  print(paste('Index ',idx,' class: ',(as.integer(real_class)-1)," --> predicted: ",prediction," [correct = ",correct,"]"))
  #displayDigit(test_total[idx,-1])
  i <- readline(prompt="Press [enter] to continue")
}