## This whole file is a reproduction of Roger Pengs 40 minutes video instructing how to conduct an exploratory analysis on PM2.5 values
setwd("C:/Git/R/Data/ExploratoryDataAnalysis")

## Read the data file
pm0 <- read.table("RD_501_88101_1999-0.txt", comment.char = "#", header = FALSE, sep = "|", na.strings = "")

## Check the number of rows and columns
dim(pm0)
head(pm0) ## We are still missing the header

## Read in the first line from the data file to use as header
cnames <- readLines("RD_501_88101_1999-0.txt", 1)
## Split up the headers, Strplit returns a list
cnames <- strsplit(cnames, "|", fixed = TRUE)
## We only want the first element of the list as the header
names(pm0) <- cnames[[1]]
## The headers now contains spaces and therefore are not valid headers
## Therefore we use make.names() to create valid headers
names(pm0) <- make.names(cnames[[1]])

## We pull out the pm2.5-value
x0 <- pm0$Sample.Value
class(x0)
str(x0)
summary(x0)
## How many are missing?
mean(is.na(x0))

## Read the second data file
pm1 <- read.table("RD_501_88101_2012-0.txt", comment.char = "#", header = FALSE, sep = "|", na.strings = "")
## We use make.names() and the same header to create valid headers
names(pm1) <- make.names(cnames[[1]])
head(pm1)
## We pull out the pm2.5-value
x1 <- pm1$Sample.Value
str(x1)
summary(x1)
summary(x0)
## It looks like a decrease on average, but there are also some very high values in the 2012 dataset, perhaps outliers
mean(is.na(x1)) ## There are fewer missing values percentagewise
boxplot(x0, x1)

## The log10() can even out the extremes
boxplot(log10(x0), log10(x1)) 

## How many are neagtive?
negative <- x1 < 0 ## Negative is a logical vector
str(negative)
summary(negative)
sum(negative, na.rm = TRUE)
mean(negative, na.rm = TRUE) ## The proportion of negative values

## Looking at the dates
dates <- pm1$Date
str(dates)
summary(dates)
## We convert them from integers to dates
dates <- as.Date(as.character(dates), "%Y%m%d")
str(dates)
Sys.setlocale(category = "LC_ALL", locale = "english") ## Set the locale to english in order to produce an x-axis with english week days
hist(dates, "month")
## To see how the negative values distribute
hist(dates[negative], "month") ## dates og negative kombineres

## Looking at a certain monitor location by extracting unique sites for New York state
site0 <- unique(subset(pm0, State.Code == 36, select = c(County.Code, Site.ID)))
site1 <- unique(subset(pm1, State.Code == 36, select = c(County.Code, Site.ID)))

## Creating a new variable as a combination of County.Code and Site.ID
site0 <- paste(site0[, 1], site0[,2], sep = ".")
site1 <- paste(site1[, 1], site1[,2], sep = ".")
summary(site0)
str(site0)
str(site1)
## We can see there are fewer locations in 2012
## We find the sites common for the two data sets using the intersect function
both <- intersect(site0, site1)

## We want to see which sites have many observations
pm0$county.site <- with(pm0, paste(County.Code, Site.ID, sep = ".")) # we add the ne variable to the original dataset
pm1$county.site <- with(pm1, paste(County.Code, Site.ID, sep = ".")) # we add the ne variable to the original dataset

## Then we subset for New York county where the site is in both datasets
cnt0 <- subset(pm0, State.Code == 36 & county.site %in% both) ## creates a logical vector with those from pm0 in both
cnt1 <- subset(pm1, State.Code == 36 & county.site %in% both)
head(cnt0)

## We split up the data by county.site and count the rows/number of observations per monitor
sapply(split(cnt0, cnt0$county.site), nrow)
sapply(split(cnt1, cnt1$county.site), nrow) 

## Then we want to look at county.site 63.2008
pm0sub <- subset(pm0, State.Code == 36 & County.Code == 63 & Site.ID == 2008)
pm1sub <- subset(pm1, State.Code == 36 & County.Code == 63 & Site.ID == 2008)
dim(pm1sub)

## We start visualizing the data
dates1 <- pm1sub$Date ## Getting the dates out
x1sub <- pm1sub$Sample.Value ## getting the PM2.5 data out
plot(dates1, x1sub)
dates1 <- as.Date(as.character(dates1), "%Y%m%d") ## converting the dates integers to date
str(dates1)
plot(dates1, x1sub)
## Doing the same for the 1999 dataset
dates0 <- pm0sub$Date ## Getting the dates out
x0sub <- pm0sub$Sample.Value ## getting the PM2.5 data out
dates0 <- as.Date(as.character(dates0), "%Y%m%d") ## converting the dates integers to date
plot(dates0, x0sub)

## We start combining the two plots
par(mfrow = c(1, 2), mar = c(4, 4, 2, 1))

##Plotting the first 
plot(dates0, x0sub, pch = 20)
abline(h = median(x0sub, na.rm = T)) ## adding a median trendline to the PM2.5 values
##Plotting the second set 
plot(dates1, x1sub, pch = 20)
abline(h = median(x1sub, na.rm = T)) ## adding a median trendline to the PM2.5 values
## We need to make the plots on the same scale
## We start by finding the range of the two sets and assign it to a variable
rng <- range(x0sub, x1sub, na.rm = T)
## Then we make the plots again
par(mfrow = c(1, 2))
plot(dates0, x0sub, pch = 20, ylim = rng)
abline(h = median(x0sub, na.rm = T)) ## adding a median trendline to the PM2.5 values
plot(dates1, x1sub, pch = 20, ylim = rng)
abline(h = median(x1sub, na.rm = T)) ## adding a median trendline to the PM2.5 values
## We can see a big spread in the 1999 values, so we have generally lower levels and less spikes in 2012

## Exploring levels at State level
## So we want to take the average level by state
mn0 <- with(pm0, tapply(Sample.Value, State.Code, mean, na.rm = T))
str(mn0)
summary(mn0)
mn1 <- with(pm1, tapply(Sample.Value, State.Code, mean, na.rm = T))
summary(mn1)

## Now we want to create a dataframe that contains these values for every state
d0 <- data.frame(State = names(mn0), mean = mn0)
d1 <- data.frame(State = names(mn1), mean = mn1)
## Then we merge these data frames
mrg <- merge(d0, d1, by = "State")
dim(mrg)
head(mrg)
## Now we want to plot these data
par(mfrow = c(1, 1))
## We create a plot with the 52 values for the 1999 data frame
with(mrg, plot(rep(1999, 52), mrg[, 2], xlim = c(1998, 2013))) ## Looks like the with()-function is not actually needed
## For the second series we just add points
with(mrg, points(rep(2012, 52), mrg[, 3]))
## Now we want to connect the dots, and we can use the segments function for that
segments(rep(1999, 52), mrg[,2], rep(2012, 52), mrg[,3])
