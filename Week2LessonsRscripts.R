## Week 2 - Exploratory Data analysis
## Lattice system

library(lattice)
set.seed(10)
x <- rnorm(100)
f <- rep(0:1, each = 50)
y <- x + f - f * x + rnorm(100, sd = 0.5)
f <- factor(f, labels = c("Group 1", "Group 2"))
xyplot(y ~ x | f, layout = c(2, 1))

## Custom panel function
xyplot(y ~ x | f, panel = function(x, y, ...){
  panel.xyplot(x, y, ...) ## First call default panel function
  panel.lmline(x, y, col = 2) ## Overlay a simple linear regression line
})

## ggplot2 system
##install.packages("ggplot2")
library(ggplot2)
str(mpg)
qplot(displ, hwy, data = mpg, color = drv)

qplot(logpm25, NocturnalSympt, data = maacs, facets = . ~bmicat, geom = 
        c("point", "smooth"), method = "lm")


library(lattice)
library(datasets)
data(airquality)
p <- xyplot(Ozone ~ Wind | factor(Month), data = airquality)

library(datasets)
data(airquality)

##qplot(Wind, Ozone, data = airquality, geom = "smooth")

##qplot(Wind, Ozone, data = airquality)

qplot(Wind, Ozone, data = airquality, facets = . ~ factor(Month))

airquality = transform(airquality, Month = factor(Month))
qplot(Wind, Ozone, data = airquality, facets = . ~ Month)

library(ggplot2)
install.packages("ggplot2movies")
library(ggplot2movies)
g <- ggplot(movies, aes(votes, rating))G
p <- g + geom_point()
print(p)


qplot(votes, rating, data = movies)

qplot(votes, rating, data = movies, panel = panel.loess)

qplot(votes, rating, data = movies) + geom_smooth()

qplot(votes, rating, data = movies) + stats_smooth("loess")

qplot(votes, rating, data = movies, smooth = "loess")


