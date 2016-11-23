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






install.packages("RColorBrewer")
library(RColorBrewer)
cols <- brewer.pal(3, "BuGn")
cols
##[1] "#E5F5F9" "#99D8C9" "#2CA25F"
pal <- colorRampPalette(cols)
image(volcano, col = pal(20))