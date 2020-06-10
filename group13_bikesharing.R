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

########## EXPLORATIVE ANALYSIS ##########

## Investigating the Correlation between all predictors
predictors = c("dteday", "season", "yr","mnth","hr","holiday","weekday",
             "workingday","weathersit","temp","atemp","hum","windspeed", "casual", "registered")
corrplot(cor(data.matrix(d.bike[predictors])))

## Investigating cnt
ggplot(data = d.bike, aes(x=cnt)) +
  geom_histogram(bins=30, colour="black", ylab="Frequency") + xlab("Count") + ylab("Frequency")
mean(d.bike$cnt)
sd(d.bike$cnt)

d.bike$log.cnt <- log(d.bike$cnt)
ggplot(data = d.bike, aes(x=log.cnt)) +
  geom_histogram(bins=30, colour="black", ylab="Frequency") + xlab("Count") + ylab("Frequency")

#Bootstrap with cnt
sort(d.bike$cnt)
mean(d.bike$cnt)

id <- sample(1:length(d.bike$cnt), replace = TRUE)
d.bike$cnt[id]
mean(d.bike$cnt[id])

B <- 1000
t.mean <- c()
for(i in 1:B){
  t.id <- sample(1:length(d.bike$cnt), replace = TRUE)
  t.d.bike <- d.bike$cnt[t.id]
  t.mean[i] <- mean(t.d.bike)
}
length(t.mean)

hist(t.mean, breaks = 50)
abline(v = mean(d.bike$cnt), col = "red")

sorted.means <- sort(t.mean)
quantile(sorted.means, probs = c(0.025, 0.975))


## Investigating the season
ggplot(data = d.bike, aes(group=season, y = cnt, x = as.factor(season))) +
  geom_boxplot()

lm.season.1 <- lm(cnt ~ as.factor(season), data = d.bike)
lm.season.0 <- lm(cnt ~ 1, data = d.bike)
anova(lm.season.0, lm.season.1)

## Investigating the year
ggplot(data = d.bike, aes(group=yr, y = cnt, x = yr)) +
  geom_boxplot()

lm.year.1 <- lm(cnt ~ as.factor(yr), data = d.bike)
lm.year.0 <- lm(cnt ~ 1, data = d.bike)
anova(lm.year.0, lm.year.1)

## Investigating the month
ggplot(data = d.bike, aes(group=mnth, y = cnt, x = mnth)) +
  geom_boxplot() + scale_x_discrete(limits=seq(1,12))

lm.month.1 <- lm(cnt ~ as.factor(mnth), data = d.bike)
lm.month.0 <- lm(cnt ~ 1, data = d.bike)
anova(lm.month.0, lm.month.1)

## Investigating the hour
ggplot(data = d.bike, aes(group=hr, y = cnt, x = hr)) +
  geom_boxplot() + scale_x_discrete(limits=seq(0,23))

lm.hr.1 <- lm(cnt ~ as.factor(hr), data = d.bike)
lm.hr.0 <- lm(cnt ~ 1, data = d.bike)
anova(lm.hr.0, lm.hr.1)

## Investigating the holiday
ggplot(data = d.bike, aes(group=holiday, y = cnt, x = holiday)) +
  geom_boxplot()

lm.holiday.1 <- lm(cnt ~ as.factor(holiday), data = d.bike)
lm.holiday.0 <- lm(cnt ~ 1, data = d.bike)
anova(lm.holiday.0, lm.holiday.1)

## Investigating the weekday
ggplot(data = d.bike, aes(group=weekday, y= cnt, x = weekday)) +
  geom_boxplot()

lm.weekday.1 <- lm(cnt ~ as.factor(weekday), data = d.bike)
lm.weekday.0 <- lm(cnt ~ 1, data = d.bike)
anova(lm.weekday.0, lm.weekday.1)

## Investigating the workingday
ggplot(data = d.bike, aes(group=workingday, y = cnt, x = workingday)) +
  geom_boxplot()

lm.workingday.1 <- lm(cnt ~ as.factor(workingday), data = d.bike)
lm.workingday.0 <- lm(cnt ~ 1, data = d.bike)
anova(lm.workingday.0, lm.workingday.1)

## Investigating the weather situation
ggplot(data = d.bike, aes(group=weathersit, y = cnt, x = weathersit)) +
  geom_boxplot()

lm.weathersit.1 <- lm(cnt ~ as.factor(weathersit), data = d.bike)
lm.weathersit.0 <- lm(cnt ~ 1, data = d.bike)
anova(lm.weathersit.0, lm.weathersit.1)

## Investigating the temperature
ggplot(data = d.bike, mapping = aes(y = cnt, x = temp)) +
  geom_point()

gam.temp.1 <- gam(cnt ~ s(temp), data = d.bike)
summary(gam.temp.1)

#Interactions
qplot(y = cnt, x = temp, data = d.bike, facets = ~ season) + geom_smooth()

qplot(y = cnt, x = temp, data = d.bike, facets = ~ yr) + geom_smooth()

qplot(y = cnt, x = temp, data = d.bike, facets = ~ mnth) + geom_smooth()

qplot(y = cnt, x = temp, data = d.bike, facets = ~ hr) + geom_smooth()

qplot(y = cnt, x = temp, data = d.bike, facets = ~ weathersit) + geom_smooth()

## Investigating the felt temperature
ggplot(data = d.bike, mapping = aes(y = cnt, x = atemp)) +
  geom_point()

gam.atemp.1 <- gam(cnt ~ s(atemp), data = d.bike)
summary(gam.atemp.1)

#interactions
qplot(y = cnt, x = atemp, data = d.bike, facets = ~ season) + geom_smooth()

qplot(y = cnt, x = atemp, data = d.bike, facets = ~ yr) + geom_smooth()

qplot(y = cnt, x = atemp, data = d.bike, facets = ~ mnth) + geom_smooth()

qplot(y = cnt, x = atemp, data = d.bike, facets = ~ hr) + geom_smooth()

qplot(y = cnt, x = atemp, data = d.bike, facets = ~ weathersit) + geom_smooth()

## Investigating the humidity
ggplot(data = d.bike, mapping = aes(y = cnt, x = hum)) +
  geom_point()

gam.hum.1 <- gam(cnt ~ s(hum), data = d.bike)
summary(gam.hum.1)

#interactions
qplot(y = cnt, x = hum, data = d.bike, facets = ~ season) + geom_smooth()

qplot(y = cnt, x = hum, data = d.bike, facets = ~ yr) + geom_smooth()

qplot(y = cnt, x = hum, data = d.bike, facets = ~ mnth) + geom_smooth()

qplot(y = cnt, x = hum, data = d.bike, facets = ~ hr) + geom_smooth()

qplot(y = cnt, x = hum, data = d.bike, facets = ~ weathersit) + geom_smooth()

## Investigating the windspeed
ggplot(data = d.bike, mapping = aes(y = cnt, x = windspeed)) +
  geom_point()

lm.windspeed.1 <- lm(cnt ~ windspeed, data = d.bike)
summary(lm.windspeed.1)

## Investigating the casual users
ggplot(data = d.bike, mapping = aes(y = cnt, x = casual)) +
  geom_point()

gam.casual.1 <- gam(cnt ~ s(casual), data = d.bike)
summary(gam.casual.1)

## Investigating the registered users
ggplot(data = d.bike, mapping = aes(y = cnt, x = registered)) +
  geom_point()

lm.registered.1 <- lm(cnt ~ registered, data = d.bike)
summary(lm.registered.1)

########## MODEL DEVELOPMENT ##########
#model with all predictors
full.model.1 <- cnt ~ as.factor(season) + as.factor(yr) + as.factor(mnth) + as.factor(hr) + as.factor(holiday) + as.factor(weekday) + as.factor(workingday) + as.factor(weathersit) + poly(hum, degree = 8.7) + poly(temp, degree = 8) + poly(atemp, degree = 8.9) + windspeed + casual + registered

#starting model with interaction effects
starting.model.1 <- cnt ~ as.factor(season) + as.factor(yr) + as.factor(mnth) + as.factor(hr) + as.factor(weathersit) + poly(hum, degree = 8.7) + poly(temp, degree = 8) + poly(atemp, degree = 8.9) + season:temp + season:atemp + season:hum + yr:hum + mnth:temp + mnth:atemp + mnth:hum + hr:temp + hr:atemp + hr:hum + weathersit:temp + weathersit:atemp + weathersit:hum

#starting model without interaction effects
starting.model.2 <- cnt ~ as.factor(season) + as.factor(yr) + as.factor(mnth) + as.factor(hr) + as.factor(weathersit) + poly(hum, degree = 8.7) + poly(temp, degree = 8) + poly(atemp, degree = 8.9)

#starting model without polynomial effects
starting.model.3 <- cnt ~ as.factor(season) + as.factor(yr) + as.factor(mnth) + as.factor(hr) + as.factor(weathersit) + temp + atemp + hum + season:temp + season:atemp + season:hum + yr:hum + mnth:temp + mnth:atemp + mnth:hum + hr:temp + hr:atemp + hr:hum + weathersit:temp + weathersit:atemp + weathersit:hum

#starting model without polynomial and interaction effects
starting.model.4 <- cnt ~ as.factor(season) + as.factor(yr) + as.factor(mnth) + as.factor(hr) + as.factor(weathersit) + temp + atemp + hum

#Comparison of starting models
lm.starting.model.1 <- lm(starting.model.1, data = d.bike)
lm.starting.model.2 <- lm(starting.model.2, data = d.bike)
lm.starting.model.3 <- lm(starting.model.3, data = d.bike)
lm.starting.model.4 <- lm(starting.model.4, data = d.bike)
summary(lm.starting.model.1)$adj.r.squared
summary(lm.starting.model.2)$adj.r.squared
summary(lm.starting.model.3)$adj.r.squared
summary(lm.starting.model.4)$adj.r.squared

#Updating better model (lm.starting.model.1)
#Drop the interactions without an effect
lm.starting.model.1 <- update(lm.starting.model.1, . ~ . - weathersit:atemp)
lm.starting.model.1 <- update(lm.starting.model.1, . ~ . - weathersit:temp)
lm.starting.model.1 <- update(lm.starting.model.1, . ~ . - hr:atemp)
lm.starting.model.1 <- update(lm.starting.model.1, . ~ . - mnth:temp)
summary(lm.starting.model.1)

#Drop the interactions with weak effect
lm.starting.model.1 <- update(lm.starting.model.1, . ~ . - mnth:hum)
lm.starting.model.1 <- update(lm.starting.model.1, . ~ . - season:temp)
summary(lm.starting.model.1)

#Drop the interactions with medium effect
lm.starting.model.1 <- update(lm.starting.model.1, . ~ . - weathersit:hum)
summary(lm.starting.model.1)

#Drop atemp
#lm.starting.model.1 <- update(lm.starting.model.1, . ~ . - atemp)
#summary(lm.starting.model.1)

#Drop temp
#lm.starting.model.1 <- update(lm.starting.model.1, . ~ . - temp)
#summary(lm.starting.model.1)

#Comparison of lm.starting.model.1 and model with all predictors
lm.full.model.1 <- lm(full.model.1, data = d.bike)
summary(lm.starting.model.1)
summary(lm.full.model.1)

#save starting.model.1 in final model
final.model.1 <- cnt ~ as.factor(season) + as.factor(yr) + as.factor(mnth) + as.factor(hr) + as.factor(weathersit) + poly(hum, degree = 8.7) + poly(temp, degree = 8) + poly(atemp, degree = 8.9) + atemp:season + hum:season + hum:yr + atemp:mnth + temp:hr + hum:hr

########## CROSS VALIDATION ##########
#Compare starting.model.1-4, full.model and final.model with adj. r-squared
lm.starting.model.1 <- lm(starting.model.1, data = d.bike)
lm.starting.model.2 <- lm(starting.model.2, data = d.bike)
lm.starting.model.3 <- lm(starting.model.3, data = d.bike)
lm.starting.model.4 <- lm(starting.model.4, data = d.bike)
lm.full.model.1 <- lm(full.model.1, data = d.bike)
lm.final.model.1 <- lm(final.model.1, data = d.bike)
summary(lm.starting.model.1)$adj.r.squared
summary(lm.starting.model.2)$adj.r.squared
summary(lm.starting.model.3)$adj.r.squared
summary(lm.starting.model.4)$adj.r.squared
summary(lm.full.model.1)$adj.r.squared
summary(lm.final.model.1)$adj.r.squared


for(i in 1:10){
  d.bike.train.id <- sample(seq_len(nrow(d.bike)),size = floor(0.75*nrow(d.bike)))
  d.bike.train <- d.bike[d.bike.train.id,]
  d.bike.test <- d.bike[-d.bike.train.id,]

  #predict data with starting model 1
  lm.starting.model.1.train <- lm(starting.model.1, data = d.bike.train)
  predicted.starting.model.1.test <- predict(lm.starting.model.1.train,
                                             newdata = d.bike.test)
  r.squared.starting.model.1 <- cor(predicted.starting.model.1.test, d.bike.test$cnt)^2
  
  #predict data with starting model 2
  lm.starting.model.2.train <- lm(starting.model.2, data = d.bike.train)
  predicted.starting.model.2.test <- predict(lm.starting.model.2.train,
                                             newdata = d.bike.test)
  r.squared.starting.model.2 <- cor(predicted.starting.model.2.test, d.bike.test$cnt)^2
  
  #predict data with starting model 3
  lm.starting.model.3.train <- lm(starting.model.3, data = d.bike.train)
  predicted.starting.model.3.test <- predict(lm.starting.model.3.train,
                                             newdata = d.bike.test)
  r.squared.starting.model.3 <- cor(predicted.starting.model.3.test, d.bike.test$cnt)^2
  
  #predict data with starting model 4
  lm.starting.model.4.train <- lm(starting.model.4, data = d.bike.train)
  predicted.starting.model.4.test <- predict(lm.starting.model.4.train,
                                             newdata = d.bike.test)
  r.squared.starting.model.4 <- cor(predicted.starting.model.4.test, d.bike.test$cnt)^2
  
  #predict data with full model
  lm.full.model.train <- lm(full.model.1, data = d.bike.train)
  predicted.full.model.test <- predict(lm.full.model.train,
                                       newdata = d.bike.test)
  r.squared.full.model <- cor(predicted.full.model.test, d.bike.test$cnt)^2
  
  #predict data with final model
  lm.final.model.train <- lm(final.model.1, data = d.bike.train)
  predicted.final.model.test <- predict(lm.final.model.train,
                                        newdata = d.bike.test)
  r.squared.final.model <- cor(predicted.final.model.test, d.bike.test$cnt)^2
}

mean(r.squared.starting.model.1)
mean(r.squared.starting.model.2)
mean(r.squared.starting.model.3)
mean(r.squared.starting.model.4)
mean(r.squared.full.model)
mean(r.squared.final.model)

########## REGRESSION ANALYSIS ##########
##Linear Regression
lm.bike.1 <- lm(final.model, data = d.bike)
summary(lm.bike.1)

#calculate the Root-mean-squared error
lm.bike.1.rmse <- sqrt(mean(lm.bike.1$residuals^2))
lm.bike.1.rmse

##Non-Linear Regression
gam.bike.1 <- gam(cnt ~ as.factor(season) + as.factor(yr) + as.factor(mnth) + as.factor(hr) + as.factor(weathersit) + s(hum) + s(temp) + s(atemp) + atemp:season + hum:season + hum:yr + atemp:mnth + temp:hr + hum:hr, data = d.bike)
summary(gam.bike.1)

#Calculate the Root-mean-squared error
gam.bike.1.rmse <- sqrt(mean(gam.bike.1$residuals^2))
gam.bike.1.rmse

##Poisson Regression
poi.bike.1 <- glm(cnt ~ as.factor(season) + as.factor(yr) + as.factor(mnth) + as.factor(hr) + as.factor(weathersit) + poly(hum, degree = 8.7) + poly(temp, degree = 8) + poly(atemp, degree = 8.9) + atemp:season + hum:season + hum:yr + atemp:mnth + temp:hr + hum:hr, data = d.bike)
summary(poi.bike.1)
length(coef(poi.bike.1))

#Calculate the Root-mean-squared error
poi.bike.1.rmse <- sqrt(mean(poi.bike.1$residuals^2))
poi.bike.1.rmse

##Compare the models
#Compare the three models
for(i in 1:10){
  d.bike.train.id <- sample(seq_len(nrow(d.bike)),size = floor(0.75*nrow(d.bike)))
  d.bike.train <- d.bike[d.bike.train.id,]
  d.bike.test <- d.bike[-d.bike.train.id,]
  
  #predict data with linear model
  lm.bike.1.train <- lm(final.model.1, data = d.bike.train)
  predicted.lm.bike.1.test <- predict(lm.bike.1.train,
                                             newdata = d.bike.test)
  r.squared.lm.bike.1 <- cor(predicted.lm.bike.1.test, d.bike.test$cnt)^2
  
  #predict data with non-linear model
  gam.bike.1.train <- gam(cnt ~ as.factor(season) + as.factor(yr) + as.factor(mnth) + as.factor(hr) + as.factor(weathersit) + s(hum) + s(temp) + s(atemp) + atemp:season + hum:season + hum:yr + atemp:mnth + temp:hr + hum:hr, data = d.bike.train)
  predicted.gam.bike.1.test <- predict(gam.bike.1.train,
                                        newdata = d.bike.test)
  r.squared.gam.bike.1 <- cor(predicted.gam.bike.1.test, d.bike.test$cnt)^2
  
  #predict data with poisson model
  poi.bike.1.train <- glm(final.model.1, data = d.bike)
  predicted.poi.bike.1.test <- predict(poi.bike.1.train,
                                        newdata = d.bike.test)
  r.squared.poi.bike.1 <- cor(predicted.poi.bike.1.test, d.bike.test$cnt)^2
}

mean(r.squared.lm.bike.1)
mean(r.squared.gam.bike.1)
mean(r.squared.poi.bike.1)

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


########## Decision Trees ##########
########## Classification Tree
##Tree Building
#fit the tree
tree.classification.bike.1 <- tree(as.factor(class) ~.-cnt, data = d.bike.train.new)
summary(tree.classification.bike.1)
#plot the tree
plot(tree.classification.bike.1)
text(tree.classification.bike.1, pretty=1, cex=0.75)

#Predict on training data
tree.classification.bike.pred.train <- predict(tree.classification.bike.1, d.bike.train.new, type="class")

#confusion table with classification error for train data
(tree.classification.bike.pred.train.ct <- table(tree.classification.bike.pred.train, as.factor(d.bike.train.new$class)))
tree.classification.bike.pred.train.correct <- 0
tree.classification.bike.pred.train.error <- 0
for (i1 in 1:3) {
  for (i2 in 1:3) {
    if (i1 == i2) {
      tree.classification.bike.pred.train.correct <- tree.classification.bike.pred.train.correct + tree.classification.bike.pred.train.ct[i1,i2]
    }else{
      tree.classification.bike.pred.train.error <- tree.classification.bike.pred.train.error + tree.classification.bike.pred.train.ct[i1,i2]
    }
  }
}
(tree.classification.bike.pred.train.rate <- tree.classification.bike.pred.train.correct/sum(tree.classification.bike.pred.train.ct)) 
(tree.classification.bike.pred.train.error <- 1 - tree.classification.bike.pred.train.rate) 

#Predict on test data
tree.classification.bike.pred.test <- predict(tree.classification.bike.1, d.bike.test.new, type="class")
#confusion table with classification error for test data
(tree.classification.bike.pred.test.ct <- table(tree.classification.bike.pred.test, as.factor(d.bike.test.new$class)))
tree.classification.bike.pred.test.correct <- 0
tree.classification.bike.pred.test.error <- 0
for (i1 in 1:3) {
  for (i2 in 1:3) {
    if (i1 == i2) {
      tree.classification.bike.pred.test.correct <- tree.classification.bike.pred.test.correct + tree.classification.bike.pred.test.ct[i1,i2]
    }else{
      tree.classification.bike.pred.test.error <- tree.classification.bike.pred.test.error + tree.classification.bike.pred.test.ct[i1,i2]
    }
  }
}
(tree.classification.bike.pred.test.rate <- tree.classification.bike.pred.test.correct/sum(tree.classification.bike.pred.test.ct)) 
(tree.classification.bike.pred.test.error <- 1 - tree.classification.bike.pred.test.rate) 

##Pruning of the classification tree
tree.classification.bike.pruning <- cv.tree(tree.classification.bike.1, FUN = prune.misclass)
summary(tree.classification.bike.pruning)
tree.classification.bike.pruning

# plot the cross-validation error-rate as a function of both size and \alpha (k):
par(mfrow=c(1,2))
plot(tree.classification.bike.pruning$size, tree.classification.bike.pruning$dev, type="b") # type="b": plot both, points and lines
plot(tree.classification.bike.pruning$k, tree.classification.bike.pruning$dev, type="b")
par(mfrow=c(1,1))

# prune the tree
prune.tree.classification.bike <- prune.misclass(tree.classification.bike.1, best=6) 
summary(prune.tree.classification.bike)
# plot the pruned tree
plot(prune.tree.classification.bike)
text(prune.tree.classification.bike,pretty=0)

# use pruned tree to predict on test data
prune.tree.classification.bike.pred.test <- predict(prune.tree.classification.bike,  d.bike.test.new, type="class")

#confusion table with classification error for pruned tree and test data
(prune.tree.classification.bike.pred.test.ct <- table(prune.tree.classification.bike.pred.test, as.factor(d.bike.test.new$class)))
(prune.tree.classification.bike.pred.test.correct <- sum(prune.tree.classification.bike.pred.test==as.factor(d.bike.test.new$class))/sum(prune.tree.classification.bike.pred.test.ct)) 
(prune.tree.classification.bike.pred.test.error <- 1 - prune.tree.classification.bike.pred.test.correct) 
#test rate and test error of unpruned tree for comparison
tree.classification.bike.pred.test.rate
tree.classification.bike.pred.test.error 

########## Regression Tree
# in this section the variable "cnt" will be predicted (--> factor)
table(d.bike.train.new$cnt) 

#"cnt" is a categorical variable -> normally classification tree is to be used. 
#...but since it has more than 32 levels, regression tree is generated

#regression tree is generated
tree.regression.bike <- tree(cnt ~ .-class, data = d.bike.train.new) 
plot(tree.regression.bike)
text(tree.regression.bike, pretty=1, cex=0.75)

#tree has 10 nodes
summary(tree.regression.bike)


##Proof: classification did not work with "cnt" because at most 32 levels are possible. cnt has more than 1000 levels...
#tree.classification.bike <- tree(as.factor(cnt) ~ .-class, data = d.bike.train.new) 
#tree.classification.bike.pred <- predict(tree.classification.bike, d.bike.train.new, type="class")


# ....regression tree is used for prediction
# prediction is made based on training data
tree.regression.bike.pred <- predict(tree.regression.bike, d.bike.train.new, type="vector")

# predictions of regression tree with true "cnt" values are compared. it shows a positive correlation
plot(tree.regression.bike.pred,d.bike.train.new$cnt)
abline (0 ,1) # compare with the function f(x)=x (intercept 0, slope 1)

# errors are calculated (error residual calculation: [predicted "cnt" values] - [real "cnt" values])
error <- tree.regression.bike.pred-d.bike.train.new$cnt
element_ID <- 1:length(error)

# Analysis of the resuduals: The majority of the residents are within the frequency of 200
plot(element_ID,error)
title(main="Analysis of the residuals")
abline(0 ,0, lwd=5, col="skyblue", lty="dotted")
abline(200 ,0, lwd=5, col="red", lty="dotted")
abline(-200 ,0, lwd=5, col="red", lty="dotted")


error_dataframe <- tibble(element_ID,error)
ggplot(data=error_dataframe) + geom_boxplot(aes(y=error))

# Histogram of error: Most errors do not seem to be over 200 up and down (as already seen in the residence analysis above).
# The errors appear to be bell shaped.
hist(error)

# some numbers on train data
# RSS: 141337474
# MSE: 10843.75
# deviation: 104.1333
(RSS <- sum((d.bike.train.new["cnt"]-tree.regression.bike.pred)^2))
(MSE <- mean(((d.bike.train.new["cnt"]-tree.regression.bike.pred)^2)$cnt))
(deviation <- sqrt(MSE))

# a prediction is made based on test data
tree.regression.bike.2.pred <- predict(tree.regression.bike, d.bike.test.new, type="vector")

# MSE
# An MSE comparison is made between train and test data. 
# As usual, the MSE of test data is higher - but in this case only slightly higher, 
# which shows a very good performance of the decision tree.

(MSE.train <- mean(((d.bike.train.new["cnt"]-tree.regression.bike.pred)^2)$cnt))
(MSE.test <- mean(((d.bike.test.new["cnt"]-tree.regression.bike.2.pred)^2)$cnt))

# Graphical comparison of error residuals between training and test data
# result: As one can see in the graph, the distribution of error rates between training and 
# test data looks the same --> good performance of the decision tree!

errors.2.in <- predict(tree.regression.bike.2, d.bike.train.new, type="vector")-d.bike.train.new$cnt
element.2.in <- as.integer(names(errors.2.in))
errors.2.in_dataframe <- tibble(element.2.in,errors.2.in,"TRAIN")
colnames(errors.2.in_dataframe) <- c('ID','error','type')
errors.2 <- predict(tree.regression.bike.2, d.bike.test.new, type="vector")-d.bike.test.new$cnt
element.2 <- as.integer(names(errors.2))
errors.2.out_dataframe <- tibble(element.2,errors.2,"TEST")
colnames(errors.2.out_dataframe) <- c('ID','error','type')

errors.2_dataframe <- bind_rows(errors.2.in_dataframe,errors.2.out_dataframe) 
errors.2_dataframe <- arrange(errors.2_dataframe, ID)

ggplot(data = errors.2_dataframe, mapping = aes(x = ID,y = error, color = type)) + 
  geom_point() + geom_boxplot(alpha = 0.5)


##Pruning

########## Bagging
bag.bike=bagging(cnt~.-class, data=d.bike.train.new, nbagg=25, coob =TRUE)
print(bag.bike)

#predict on test set
yhat.bag = predict(bag.bike,newdata=d.bike.test.new)
plot(yhat.bag, as.factor(d.bike.test.new$cnt))
abline(0,1)
#performance (MSE)
mean((yhat.bag-d.bike.test.new$cnt)^2)

########## Random Forest
rf.bike=randomForest(cnt~.,data=d.bike.train.new, mtry=6,importance =TRUE)

#predict on test set
yhat.rf = predict(rf.bike ,newdata=d.bike.test.new)

#performance (MSE)
mean((yhat.rf-d.bike.test.new$class)^2)
importance(rf.bike)
varImpPlot (rf.bike)

########## Boosting
boost.bike=gbm(cnt~.-class,data=d.bike.train.new,
               distribution="poisson",n.trees=1000, interaction.depth=4)
summary(boost.bike)

# Producing partial dependence plots for the two most important attributes hr and yr
plot(boost.bike ,i="hr") 
plot(boost.bike ,i="yr")

# Predict on test set
yhat.boost=predict(boost.bike,newdata=d.bike.test.new, n.trees=1000)
#performance (MSE)
mean((yhat.boost -d.bike.test.new$cnt)^2)

# Updating shrinkage
boost.bike=gbm(cnt~.-class,data=d.bike.train.new,distribution="poisson",n.trees=1000, interaction.depth=4,shrinkage = 0.02, verbose = F)
yhat.boost=predict(boost.bike,newdata = d.bike.test.new, n.trees=1000)
mean((yhat.boost -d.bike.test.new$cnt)^2)

boost.bike=gbm(cnt~.-class,data=d.bike.train.new,distribution="poisson",n.trees=1000, interaction.depth=4,shrinkage = 0.2, verbose = F)
yhat.boost=predict(boost.bike,newdata = d.bike.test.new, n.trees=1000)
mean((yhat.boost -d.bike.test.new$cnt)^2)

########## SVM ##########
##### Linear SVM with casual and atemp
ggplot(data = d.bike, mapping = aes(y = casual, x = atemp)) + geom_point()
xi <- 0.5525
ggplot(data = d.bike, mapping = aes(y = casual, x = atemp)) + geom_point() + geom_vline(xintercept = xi, color = "blue")

#Build class 0 and 1
for(i in 1:nrow(d.bike)){
  if(d.bike$atemp[i] < 0.5525){
    d.bike$class[i] <- 0
  } else if (d.bike$atemp[i] >= 0.5525){
    d.bike$class[i] <- 1
  }
}

c.bike <- data.frame(x=d.bike, y=as.factor(d.bike$class))
ggplot(data = c.bike, mapping = aes(y = x.casual, x = x.atemp,  color=y)) + geom_point() + geom_vline(xintercept = xi, size = 1, alpha = 0.5)

# Creating svm model
?svm
svm.bike.1 <- svm(y~x.casual+x.atemp,
                  data = c.bike,
                  kernel = "linear",
                  cost = 10,
                  scale = FALSE)
plot(svm.bike.1, c.bike, x.casual~x.atemp)
summary(svm.bike.1)

# Build a cost range for tuning
cost_range <-
  c(0.01,
    0.1,
    1,
    5,
    10,
    100)

# tune.out model
tune.out <- tune(
  svm,
  y ~ x.casual+x.atemp,
  data = c.bike,
  kernel = "linear",
  ranges = list(cost = cost_range)
)
summary(tune.out)

# get the model with best cost parameter
svm.bike.1.best <- tune.out$best.model
summary(svm.bike.1.best)
plot(svm.bike.1.best, c.bike, x.casual~x.atemp)

# show if there are any errors in prediction
table(predict = predict(svm.bike.1.best, c.bike),
      truth = c.bike$y)


###### SVM with model developed
d.bike.train.svm <- data.frame(x=subset(d.bike.train.new, select=-c(class)), y=as.factor(d.bike.train.new$class))
d.bike.test.svm <- data.frame(x=subset(d.bike.test.new, select=-c(class)), y=as.factor(d.bike.test.new$class))

#linear kernel
svm.bike.2 <- svm(y ~ .,
                  data = d.bike.train.svm,
                  kernel = "linear",
                  cost = 0.01,
                  scale = FALSE,
)
summary(svm.bike.2)

# calculate performance of svm.bike.1 on d.bike.train.svm
predict.svm.bike.2.train <- predict(svm.bike.2, d.bike.train.svm)
table(predict.svm.bike.2.train, d.bike.train.svm$y)

# calculate performance of svm.bike.1 on d.bike.test.svm
predict.svm.bike.2.test <- predict(svm.bike.2, d.bike.test.svm)
table(predict.svm.bike.2.test, d.bike.test.svm$y)

# Build a cost range for tuning
cost_range <-
  c(0.01,
    0.1,
    1,
    5,
    10,
    100,
    1000)

# tune.out model
tune.out <- tune(
  svm,
  y~.,
  data = d.bike.train.svm,
  kernel = "linear",
  ranges = list(cost = cost_range),
  scale = FALSE
  )
summary(tune.out)

# fit the best model
svm.bike.2.best <- tune.out$best.model
summary(svm.bike.2.best)

# calculate performance of svm.bike.best on d.bike.train.svm
predict.svm.bike.2.best.train <- predict(svm.bike.2.best, d.bike.train.svm)
table(predict.svm.bike.2.best.train, d.bike.train.svm$y)

corrects=sum(predict.svm.bike.2.best.train==d.bike.train.svm$y)
errors=sum(predict.svm.bike.2.best.train!=d.bike.train.svm$y)
(performance_train=corrects/(corrects+errors))

# calculate performance of svm.bike.best on d.bike.test.svm
predict.svm.bike.2.best.test <- predict(svm.bike.2.best, d.bike.test.svm)
table(predict.svm.bike.2.best.test, d.bike.test.svm$y)

corrects=sum(predict.svm.bike.2.best.test==d.bike.test.svm$y)
errors=sum(predict.svm.bike.2.best.test!=d.bike.test.svm$y)
(performance_test=corrects/(corrects+errors))

########## NEURONAL NETWORK ##########
