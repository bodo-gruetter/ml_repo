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