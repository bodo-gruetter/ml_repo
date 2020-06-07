library(ggplot2)
#install.packages("e1071")
library(e1071)     # for the SVM funtion()
library(mgcv)

# clear environment
rm(list = ls())
# set plotting window to default
par(mfrow = c(1, 1))

########## Getting Data ##########

d.bike <- read.csv("bikesharing.csv", header=TRUE)

str(d.bike)
head(d.bike)
tail(d.bike)
sum(is.na(d.bike))
mean(is.na(d.bike))

########## EXPLORATIVE ANALYSIS ##########

## Investigating the season
ggplot(data = d.bike, aes(group=season, y = cnt, x = as.factor(season))) +
  geom_boxplot()

## Investigating the year
ggplot(data = d.bike, aes(group=yr, y = cnt, x = yr)) +
  geom_boxplot()

## Investigating the month
ggplot(data = d.bike, aes(group=mnth, y = cnt, x = mnth)) +
  geom_boxplot() + scale_x_discrete(limits=seq(1,12))

## Investigating the hour
ggplot(data = d.bike, aes(group=hr, y = cnt, x = hr)) +
  geom_boxplot() + scale_x_discrete(limits=seq(0,23))

## Investigating the hour
ggplot(data = d.bike, aes(group=hr, y = cnt, x = hr)) +
  geom_point() + scale_x_discrete(limits=seq(0,23))

## Investigating the holiday
ggplot(data = d.bike, aes(group=holiday, y = cnt, x = holiday)) +
  geom_boxplot()

## Investigating the weekday
ggplot(data = d.bike, aes(group=weekday, y= cnt, x = weekday)) +
  geom_boxplot()

## Investigating the workingday
ggplot(data = d.bike, aes(group=workingday, y = cnt, x = workingday)) +
  geom_boxplot()

## Investigating the weather situation
ggplot(data = d.bike, aes(group=weathersit, y = cnt, x = weathersit)) +
  geom_boxplot()

## Investigating the temperature
ggplot(data = d.bike, mapping = aes(y = cnt, x = temp)) +
  geom_point()

lm.temp.1 <- lm(cnt ~ temp, data = d.bike)
summary(lm.temp.1)

qplot(y = cnt, x = temp, data = d.bike, facets = ~ season)
+ geom_abline(intercept=lm.temp.1$coefficients[1], slope=lm.temp.1$coefficients[2], colour="red", size=1)

## Investigating the felt temperature
ggplot(data = d.bike, mapping = aes(y = cnt, x = atemp)) +
  geom_point()

## Investigating the humidity
ggplot(data = d.bike, mapping = aes(y = cnt, x = hum)) +
  geom_point()

## Investigating the windspeed
ggplot(data = d.bike, mapping = aes(y = cnt, x = windspeed)) +
  geom_point()

## Investigating the casual users
ggplot(data = d.bike, mapping = aes(y = cnt, x = casual)) +
  geom_point()

## Investigating the registered users
ggplot(data = d.bike, mapping = aes(y = cnt, x = registered)) +
  geom_point()

## Investigating Interactions

qplot(y = cnt, x = temp, data = d.bike, facets = ~ season) + geom_smooth()

qplot(y = cnt, x = temp, data = d.bike, facets = ~ yr) + geom_smooth()

qplot(y = cnt, x = temp, data = d.bike, facets = ~ mnth) + geom_smooth()

qplot(y = cnt, x = temp, data = d.bike, facets = ~ hr) + geom_smooth()

qplot(y = cnt, x = temp, data = d.bike, facets = ~ weathersit) + geom_smooth()

qplot(y = cnt, x = atemp, data = d.bike, facets = ~ season) + geom_smooth()

qplot(y = cnt, x = atemp, data = d.bike, facets = ~ yr) + geom_smooth()

qplot(y = cnt, x = atemp, data = d.bike, facets = ~ mnth) + geom_smooth()

qplot(y = cnt, x = atemp, data = d.bike, facets = ~ hr) + geom_smooth()

qplot(y = cnt, x = atemp, data = d.bike, facets = ~ weathersit) + geom_smooth()

qplot(y = cnt, x = hum, data = d.bike, facets = ~ season) + geom_smooth()

qplot(y = cnt, x = hum, data = d.bike, facets = ~ yr) + geom_smooth()

qplot(y = cnt, x = hum, data = d.bike, facets = ~ mnth) + geom_smooth()

qplot(y = cnt, x = hum, data = d.bike, facets = ~ hr) + geom_smooth()

qplot(y = cnt, x = hum, data = d.bike, facets = ~ weathersit) + geom_smooth()

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
lm.starting.model.1 <- update(lm.starting.model.1, . ~ . - atemp)
summary(lm.starting.model.1)

#Drop temp
lm.starting.model.1 <- update(lm.starting.model.1, . ~ . - temp)
summary(lm.starting.model.1)

#Comparison of lm.starting.model.1 and model with all predictors
lm.full.model.1 <- lm(full.model.1, data = d.bike)
summary(lm.starting.model.1)
summary(lm.full.model.1)

#save starting.model.1 in final model
final.model <- cnt ~ as.factor(season) + as.factor(yr) + as.factor(mnth) + as.factor(hr) + as.factor(weathersit) + hum + atemp:season + hum:season + hum:yr + atemp:mnth + temp:hr + hum:hr

########## REGRESSION ANALYSIS ##########
##Linear Regression
lm.bike.1 <- lm(final.model, data = d.bike)
summary(lm.bike.1)

#calculate the Root-mean-squared error
lm.rmse <- sqrt(mean(lm.bike.1$residuals^2))
lm.rmse

##Non-Linear Regression
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


########## SVM ##########

# Linear SVM with cnt and temp
i <- 720
s <- -900
ggplot(data = d.bike, mapping = aes(y = cnt, x = temp)) + geom_point() + geom_abline(intercept = i, slope = s)

# Creating class
c.bike <- data.frame(x=d.bike, t=as.factor(as.numeric(i + s * d.bike$temp <= d.bike$cnt)))
ggplot(data = c.bike, mapping = aes(y = x.cnt, x = x.temp,  color=t)) + geom_point() + geom_abline(slope = s, intercept = i, size = 1, alpha = 0.5)

# Creating svm model
svm.bike.1 <- svm(t~x.cnt+x.temp,
           data = c.bike,
           kernel = "linear",
           cost = 10,
           scale = FALSE)
plot(svm.bike.1, c.bike, x.cnt~x.temp)

svm.bike.1$index
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
  t ~ x.cnt+x.temp,
  data = c.bike,
  kernel = "linear",
  ranges = list(cost = cost_range)
)
summary(tune.out)

# get the model with pest cost parameter
svm.bike.best <- tune.out$best.model
summary(svm.bike.best)
plot(svm.bike.best, c.bike, x.cnt~x.temp)

# show if there are any errors in prediction
table(predict = predict(svm.bike.best, c.bike),
      truth = c.bike$t)

# Non linear SVM with cnt and temp


# Non linear SVM with developed model
