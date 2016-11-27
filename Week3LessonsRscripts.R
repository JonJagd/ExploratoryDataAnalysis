## Hierarchical clustering - example
set.seed(1234)
par(mar = c(0, 0, 0, 0)) ##par can be used to set or query graphical parameters. mar er argumentet for at angive margen på de 4 sider af et plot
x <- rnorm(12, mean = rep(1:3, each = 4), sd = 0.2)
y <- rnorm(12, mean = rep(c(1, 2, 1), each = 4), sd = 0.2)
plot(x, y, col = "blue", pch = 19, cex = 2) ##cex = størrelsen på punkterne
text(x + 0.05, y + 0.05, labels = as.character(1:12)) ## tekst til hvert punkt, som placeres ud fra x og y-koordinater


## Hierarchical clustering - dist
## Important parameters: x,method
dataFrame <- data.frame(x = x, y = y)
dist(dataFrame ) ##, method = "euclidean"

##Hierarchical clustering - hclust
dataFrame <- data.frame(x = x, y = y)
distxy <- dist(dataFrame)
hClustering <- hclust(distxy)
plot(hClustering)

#Prettier dendrograms
myplclust <- function(hclust, lab = hclust$labels, lab.col = rep(1, length(hclust$labels)),
                      hang = 0.1, ...) {
  ## modifiction of plclust for plotting hclust objects *in colour*! Copyright
  ## Eva KF Chan 2009 Arguments: hclust: hclust object lab: a character vector
  ## of labels of the leaves of the tree lab.col: colour for the labels;
  ## NA=default device foreground colour hang: as in hclust & plclust Side
  ## effect: A display of hierarchical cluster with coloured leaf labels.
  y <- rep(hclust$height, 2)
  x <- as.numeric(hclust$merge)
  y <- y[which(x < 0)]
  x <- x[which(x < 0)]
  x <- abs(x)
  y <- y[order(x)]
  x <- x[order(x)]
  plot(hclust, labels = FALSE, hang = hang, ...)
  text(x = x, y = y[hclust$order] - (max(hclust$height) * hang), labels = lab[hclust$order],
       col = lab.col[hclust$order], srt = 90, adj = c(1, 0.5), xpd = NA, ...)
}

##Pretty dendrograms
dataFrame <- data.frame(x = x, y = y)
distxy <- dist(dataFrame)
hClustering <- hclust(distxy)
myplclust(hClustering, lab = rep(1:3, each = 4), lab.col = rep(1:3, each = 4))


##heatmap()
dataFrame <- data.frame(x = x, y = y)
set.seed(143)
dataMatrix <- as.matrix(dataFrame)[sample(1:12), ]
heatmap(dataMatrix)


##K-means clustering - example
set.seed(1234)
par(mar = c(0, 0, 0, 0))
x <- rnorm(12, mean = rep(1:3, each = 4), sd = 0.2)
y <- rnorm(12, mean = rep(c(1, 2, 1), each = 4), sd = 0.2)
plot(x, y, col = "blue", pch = 19, cex = 2)
text(x + 0.05, y + 0.05, labels = as.character(1:12))


## kmeans()
## Important parameters: x, · centers, iter.max, nstart
dataFrame <- data.frame(x, y)
kmeansObj <- kmeans(dataFrame, centers = 3)
names(kmeansObj)

kmeansObj$cluster

##kmeans()
par(mar = rep(0.2, 4))
plot(x, y, col = kmeansObj$cluster, pch = 19, cex = 2)
points(kmeansObj$centers, col = 1:3, pch = 3, cex = 3, lwd = 3)

## Heatmaps
set.seed(1234)
dataMatrix <- as.matrix(dataFrame)[sample(1:12), ]
kmeansObj2 <- kmeans(dataMatrix, centers = 3)
par(mfrow = c(1, 2), mar = c(2, 4, 0.1, 0.1))
image(t(dataMatrix)[, nrow(dataMatrix):1], yaxt = "n") ## Et billede med dataMatrix transporneret ud (t) og læst fra kolonne 12:1
## yaxt specifies the y axis type. Specifying "n" suppresses plotting.
image(t(dataMatrix)[, order(kmeansObj$cluster)], yaxt = "n")


## Color U1li1es in R
## The grDevices package has two functions
## These functions take palettes of colors and help to interpolate between the colors
## The function colors() lists the names of colors you can use in any plotting function

## Color Palette Utilities in R
  ## colorRamp: Take a palette of colors and return a function that takes valeus between 0 and 1,
  ## indicating the extremes of the color palette (e.g. see the 'gray' function)

  ## colorRampPalette: Take a palette of colors and return a function that takes integer arguments
  ## and returns a vector of colors interpolating the palette (like heat.colors or topo.colors)

## colorRamp
  ## [,1] [,2] [,3] corresponds to [Red] [Blue] [Green] 
pal <- colorRamp(c("red", "blue"))

pal(0) ## HELT RØD
##[,1] [,2] [,3]
##[1,]  255    0    0
pal(1)  ## Helt blå
pal(0.5) ## Midt mellem blå og rød og man møder ikke grønd midt i mellem blå og rød
##[,1] [,2]  [,3]
##[1,] 127.5    0 127.5

pal(seq(0, 1, len = 10)) ## En sekventiel liste på 10 numre fra 0 - 1 giver en 10-pkts skala fra rød til blå

## colorRampPalette

pal <- colorRampPalette(c("red", "yellow"))
pal(2) ## Giver bare de to farver på skalaen som er rød og gul
## [1] "#FF0000" "#FFFF00"
pal(10) # En 10-pkts skala fra rød til gul
##[1] "#FF0000" "#FF1C00" "#FF3800" "#FF5500" "#FF7100"
##[6] "#FF8D00" "#FFAA00" "#FFC600" "#FFE200" "#FFFF00"


##install.packages("RColorBrewer")
library(RColorBrewer)
cols <- brewer.pal(3, "BuGn")
cols
##[1] "#E5F5F9" "#99D8C9" "#2CA25F"
pal <- colorRampPalette(cols)
par(mfrow = c(1,1))
image(volcano, col = pal(20)) ## Specifikation af 20 forskellige farver

##Smooth scatter function
x <- rnorm(10000)
y <- rnorm(10000)
smoothScatter(x, y)


##Some other plotting notes
##The rgb function can be used to produce any color via red, green, blue proportions
##Color transparency can be added via the alpha parameter to rgb
##The colorspace package can be used for a different control over colors

##Scatterplot with no transparency
plot(x,y, pch=19)

##Scatterplot with transparency
plot(x,y, col=rgb(0,0,0,0.2), pch=19)
