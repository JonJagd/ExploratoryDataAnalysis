setwd('C:/Git/R/ExploratoryDataAnalysis')

pollution <- read.csv("data/avgpm25.csv", colClasses = c("numeric", "character",
 "factor", "numeric", "numeric"))
head(pollution)

summary(pollution$pm25)

## Boxplot
boxplot(pollution$pm25, col = "blue")

##Histogram
hist(pollution$pm25, col = "green")
rug(pollution$pm25)

hist(pollution$pm25, col = "green", breaks = 100)
rug(pollution$pm25)

## Overlaying Features
boxplot(pollution$pm25, col = "blue")
abline(h = 12)

## Barplot
barplot(table(pollution$region), col = "wheat", main = "Number of Counties in Each Region")

## Multiple Boxplots
boxplot(pm25 ~ region, data = pollution, col = "red")

## Multiple Histograms
par(mfrow = c(2, 1), mar = c(4, 4, 2, 1))
hist(subset(pollution, region == "east")$pm25, col = "green")
hist(subset(pollution, region == "west")$pm25, col = "green")

##Scatterplot
with(pollution, plot(latitude, pm25))
abline(h = 12, lwd = 2, lty = 2)

##Scatterplot - Using Color
with(pollution, plot(latitude, pm25, col = region))
abline(h = 12, lwd = 2, lty = 2)

## Multiple Scatterplots
par(mfrow = c(1, 2), mar = c(5, 4, 2, 1))
with(subset(pollution, region == "west"), plot(latitude, pm25, main = "West"))
with(subset(pollution, region == "east"), plot(latitude, pm25, main = "East"))

##How Does a Plot Get Created?
library(datasets)
with(faithful, plot(eruptions, waiting)) ## Make plot appear on screen device
title(main = "Old Faithful Geyser data") ## Annotate with a title

