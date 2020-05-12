library(ggplot2)

##Getting the Data
# Loading the data into data.frame "d.wine"
d.wine <- read.csv("winequalityN.csv", header=TRUE)

# Showing the structure of d.wine
str(d.wine)

# Showing the first and last rows
head(d.wine)
tail(d.wine)

# Count and mean the not available (NA) values
sum(is.na(d.wine))
mean(is.na(d.wine))

# Omit the rows which contains NAs
d.wine <- na.omit(d.wine)
sum(is.na(d.wine))

# Showing the Distribution
table(d.wine$quality)

##Graphical Analysis
#simple boxplot with 2 catogorial variables
##Histogram
ggplot(data = d.wine, aes(fill=as.factor(d.wine$quality), x = d.wine$fixed.acidity)) +
  geom_histogram(bins=30, col="black")

##Boxplot
ggplot(data = d.wine, aes(y = d.wine$quality, x = d.wine$type)) +
  geom_boxplot()

ggplot(d.wine, aes(x=quality, y=fixed.acidity, fill=type)) + 
  geom_boxplot() +
  facet_wrap(~type)

qplot(y = d.wine$quality, x = d.wine$type,
      data = d.wine,
      facets = ~ d.wine$type)

ggplot(data = d.wine, aes(y = d.wine$quality, x = d.wine$type)) +
  geom_boxplot(alpha = 0) +
  geom_jitter(alpha = 0.1, color = "tomato")

ggplot(data = d.wine, aes(y = d.wine$fixed.acidity, x = d.wine$quality)) +
  geom_boxplot()


##Lab example
ggplot(data = d.wine, mapping = aes(y = d.wine$quality, x = d.wine$fixed.acidity)) +
  geom_point()

#
ggplot(data = d.wine, aes(y = d.wine$quality, x = d.wine$fixed.acidity)) +
  geom_jitter()
