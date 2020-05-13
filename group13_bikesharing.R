library(ggplot2)

d.bike <- read.csv("bikesharing.csv", header=TRUE)
str(d.bike)
head(d.bike)
tail(d.bike)
sum(is.na(d.bike))
mean(is.na(d.bike))

## Investigating the season
ggplot(data = d.bike, aes(group=season, y = cnt, x = season)) +
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

qplot(y = cnt, x = temp, data = d.bike, facets = ~ season)

qplot(y = cnt, x = temp, data = d.bike, facets = ~ yr)

qplot(y = cnt, x = temp, data = d.bike, facets = ~ mnth)

qplot(y = cnt, x = temp, data = d.bike, facets = ~ hr)

qplot(y = cnt, x = temp, data = d.bike, facets = ~ holiday)

qplot(y = cnt, x = temp, data = d.bike, facets = ~ weekday)

qplot(y = cnt, x = temp, data = d.bike, facets = ~ workingday)

qplot(y = cnt, x = temp, data = d.bike, facets = ~ weathersit)

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
