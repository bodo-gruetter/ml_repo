library(ggplot2)

d.bike <- read.csv("bikesharing.csv", header=TRUE)
str(d.bike)
head(d.bike)
tail(d.bike)
sum(is.na(d.bike))
mean(is.na(d.bike))

ggplot(data = d.bike, aes(group=season, y = count, x = season)) +
  geom_boxplot()

ggplot(data = d.bike, aes(group=holiday, y = count, x = holiday)) +
  geom_boxplot()

ggplot(data = d.bike, aes(group=workingday, y = count, x = workingday)) +
  geom_boxplot()

ggplot(data = d.bike, aes(group=weather, y = count, x = weather)) +
  geom_boxplot()

ggplot(data = d.bike, mapping = aes(y = count, x = temp)) +
  geom_point()

ggplot(data = d.bike, mapping = aes(y = count, x = atemp)) +
  geom_point()

ggplot(data = d.bike, mapping = aes(y = count, x = humidity)) +
  geom_point()

ggplot(data = d.bike, mapping = aes(y = count, x = windspeed)) +
  geom_point()

ggplot(data = d.bike, mapping = aes(y = count, x = casual)) +
  geom_point()

ggplot(data = d.bike, mapping = aes(y = count, x = registered)) +
  geom_point()

