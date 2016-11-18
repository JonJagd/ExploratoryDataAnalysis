setwd('C:/Git/R/ExploratoryDataAnalysis')

##1. Install swirl

##Since swirl is an R package, you can easily install it by entering a single command from the R console:

##1
install.packages("swirl")
#If you 've installed swirl in the past make sure you have version 2.2.21 or later. You can check this with:



##1
packageVersion("swirl")
##2. Load swirl

##Every time you want to use swirl, you need to first load the package. From the R console:



##1
library(swirl)

##3. Install the Exploratory Data Analysis course

##swirl offers a variety of interactive courses, but for our purposes, you want the one called Exploratory Data Analysis. 
##Type the following from the R prompt to install this course:



##1
install_from_swirl("Exploratory Data Analysis")

##4. Start swirl and complete the lessons

##Type the following from the R console to start swirl:



##1
swirl()
## Then, follow the menus and select the Exploratory Data Analysis course when given the option. 
## For the first part of this course you should complete the following lessons: