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