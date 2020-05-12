library(ggplot2)

d.bike <- read.csv("bikesharing.csv", header=TRUE)
str(d.bike)
head(d.bike)
tail(d.bike)
sum(is.na(d.bike))
mean(is.na(d.bike))

## Investigating the season
ggplot(data = d.bike, aes(group=season, y = count, x = season)) +
  geom_boxplot()

## Investigating the holiday
ggplot(data = d.bike, aes(group=holiday, y = count, x = holiday)) +
  geom_boxplot()

## Investigating the workingday
ggplot(data = d.bike, aes(group=workingday, y = count, x = workingday)) +
  geom_boxplot()

## Investigating the workingday
ggplot(data = d.bike, aes(group=weather, y = count, x = weather)) +
  geom_boxplot()

## Investigating the temperature
ggplot(data = d.bike, mapping = aes(y = count, x = temp)) +
  geom_point()

qplot(y = count, x = temp, data = d.bike, facets = ~ season)

qplot(y = count, x = temp, data = d.bike, facets = ~ holiday)

qplot(y = count, x = temp, data = d.bike, facets = ~ workingday)

qplot(y = count, x = temp, data = d.bike, facets = ~ weather)

## Investigating the felt temperature
ggplot(data = d.bike, mapping = aes(y = count, x = atemp)) +
  geom_point()

qplot(y = count, x = atemp, data = d.bike, facets = ~ season)

qplot(y = count, x = atemp, data = d.bike, facets = ~ holiday)

qplot(y = count, x = atemp, data = d.bike, facets = ~ workingday)

qplot(y = count, x = atemp, data = d.bike, facets = ~ weather)

## Investigating the humidity
ggplot(data = d.bike, mapping = aes(y = count, x = humidity)) +
  geom_point()

qplot(y = count, x = humidity, data = d.bike, facets = ~ season)

qplot(y = count, x = humidity, data = d.bike, facets = ~ holiday)

qplot(y = count, x = humidity, data = d.bike, facets = ~ workingday)

qplot(y = count, x = humidity, data = d.bike, facets = ~ weather)

## Investigating the windspeed
ggplot(data = d.bike, mapping = aes(y = count, x = windspeed)) +
  geom_point()

qplot(y = count, x = windspeed, data = d.bike, facets = ~ season)

qplot(y = count, x = windspeed, data = d.bike, facets = ~ holiday)

qplot(y = count, x = windspeed, data = d.bike, facets = ~ workingday)

qplot(y = count, x = windspeed, data = d.bike, facets = ~ weather)

## Investigating the casual users
ggplot(data = d.bike, mapping = aes(y = count, x = casual)) +
  geom_point()

qplot(y = count, x = casual, data = d.bike, facets = ~ season)

qplot(y = count, x = casual, data = d.bike, facets = ~ holiday)

qplot(y = count, x = casual, data = d.bike, facets = ~ workingday)

qplot(y = count, x = casual, data = d.bike, facets = ~ weather)

## Investigating the registered users
ggplot(data = d.bike, mapping = aes(y = count, x = registered)) +
  geom_point()

qplot(y = count, x = registered, data = d.bike, facets = ~ season)

qplot(y = count, x = registered, data = d.bike, facets = ~ holiday)

qplot(y = count, x = registered, data = d.bike, facets = ~ workingday)

qplot(y = count, x = registered, data = d.bike, facets = ~ weather)
