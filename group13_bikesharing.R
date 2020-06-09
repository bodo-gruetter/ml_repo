#####################################
#Bodo Gruetter, bodojeremy.gruetter@stud.hslu.ch
#Necati Van, necati.vanr@stud.hslu.ch
#Cengiz Cetinkaya, cengiz.cetinkaya@stud.hslu.ch
#Malik Sogukoglu, malik.sogukoglu@stud.hslu.ch
#Group 13

########## Import Packages ##########
library(ggplot2)
#install.packages("e1071")
library(e1071)     # for the SVM funtion()
library(mgcv)
#install.packages("corrplot")
library(corrplot)
#install.packages("tree")
library(tree)
library(mgcv)
library(boot)

########## Prepare Environment ##########
# clear environment
rm(list = ls())
# set plotting window to default
par(mfrow = c(1, 1))

########## Getting Data ##########
##Load Data
d.bike <- read.csv("bikesharing.csv", header=TRUE)

##Remove instant and dteday
d.bike <- subset(d.bike, select=-c(instant, dteday))

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
full.model.1 <- cnt ~ as.factor(season) + as.factor(yr) + as.factor(mnth) + as.factor(hr) + as.factor(holiday) + as.factor(weekday) + as.factor(workingday) + as.factor(weathersit) + temp + atemp + hum + windspeed + casual + registered

#starting model with interaction effects
starting.model.1 <- cnt ~ as.factor(season) + as.factor(yr) + as.factor(mnth) + as.factor(hr) + as.factor(weathersit) + temp + atemp + hum + season:temp + season:atemp + season:hum + yr:hum + mnth:temp + mnth:atemp + mnth:hum + hr:temp + hr:atemp + hr:hum + weathersit:temp + weathersit:atemp + weathersit:hum

#starting model without interaction effects
starting.model.2 <- cnt ~ as.factor(season) + as.factor(yr) + as.factor(mnth) + as.factor(hr) + as.factor(weathersit) + temp + atemp + hum

#Comparison
lm.starting.model.1 <- lm(starting.model.1, data = d.bike)
lm.starting.model.2 <- lm(starting.model.2, data = d.bike)
summary(lm.starting.model.1)
summary(lm.starting.model.2)

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
final.model <- cnt ~ as.factor(season) + as.factor(yr) + as.factor(mnth) + as.factor(hr) + as.factor(weathersit) + temp + atemp + hum + atemp:season + hum:season + hum:yr + atemp:mnth + temp:hr + hum:hr

########## CROSS VALIDATION ##########


########## REGRESSION ANALYSIS ##########
##Linear Regression
lm.bike.1 <- lm(final.model, data = d.bike)
summary(lm.bike.1)

#calculate the Root-mean-squared error
lm.rmse <- sqrt(mean(lm.bike.1$residuals^2))
lm.rmse

##Non-Linear Regression
gam.bike.0 <- gam(final.model, data = d.bike)
summary(gam.bike.0)
gam.bike.1 <- gam(cnt ~ as.factor(season) + as.factor(yr) + as.factor(mnth) + as.factor(hr) + as.factor(weathersit) + s(hum) + s(temp) + s(atemp) + atemp:season + hum:season + hum:yr + atemp:mnth + temp:hr + hum:hr, data = d.bike)
summary(gam.bike.1)

#Calculate the Root-mean-squared error
gam.rmse <- sqrt(mean(gam.bike.1$residuals^2))
gam.rmse

##Poisson Regression
poi.bike.1 <- glm(final.model, data = d.bike)
summary(poi.bike.1)

#Calculate the Root-mean-squared error
poi.rmse <- sqrt(mean(poi.bike.1$residuals^2))
poi.rmse

##Compare the models

########## UPDATING DATASET FOR CLASSIFICATION ##########
##Remove features
str(d.bike)
d.bike.new <- subset(d.bike, select=-c(holiday, weekday, workingday, temp, atemp, windspeed, casual, registered))
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
#Classifaction / Regression Tree
tree.example <- tree(cnt ~ .-class, data = d.bike.train.new)
plot(tree.example)
text(tree.example, cex=0.75)

#Pruning

#Bagging

#Random Forest

#Boosting


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
