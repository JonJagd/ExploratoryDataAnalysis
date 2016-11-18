## Set the working directory
setwd('C:/Git/R/ExploratoryDataAnalysis/CourseProject1')

## Read only the relevant lines from the data file into our data frame using sqldf
##install.packages("sqldf")
library(sqldf)
powerconsumption <- read.csv.sql("data/household_power_consumption.csv", 
    sql = "select * from file where Date = '1/2/2007' or Date = '2/2/2007'", sep = ";", eol = "\n")

## Create a datetime column
dates <- powerconsumption$Date
times <- powerconsumption$Time
datetime <- paste(dates, times)
datetime <- strptime(datetime, "%d/%m/%Y %H:%M:%S")

powerconsumption$Datetime <- datetime

# Plot1 - Create the plot and save it to a png file

png("ExData_Plotting1/plot1.png", width = 480, height = 480, res = 72)

hist(powerconsumption$Global_active_power, col = "red", main = "Global Active Power", xlab = "Global Active Power (kilowatts)")

dev.off()


# Plot2 - Create the plot and save it to a png file

png("ExData_Plotting1/plot2.png", width = 480, height = 480, res = 72)

plot(powerconsumption$Datetime, powerconsumption$Global_active_power, type = "l", xlab = "", ylab = "Global Active Power (kilowatts)")

dev.off()


# Plot3 - Create the plot and save it to a png file

png("ExData_Plotting1/plot3.png", width = 480, height = 480, res = 72)

plot(powerconsumption$Datetime, powerconsumption$Sub_metering_1, type = "l", col = "black", xlab = "", ylab = "Energy sub metering")
lines(powerconsumption$Datetime, powerconsumption$Sub_metering_2, type = "l", col = "red")
lines(powerconsumption$Datetime, powerconsumption$Sub_metering_3, type = "l", col = "blue")

legend('topright', c("Sub_metering_1","Sub_metering_2", "Sub_metering_3"), # puts text in the legend
lty = 1, # gives the legend appropriate symbols (lines)
lwd = 1, col = c("black", "red", "blue")) # gives the legend lines the correct color and width

dev.off()


# Plot4 - Create the plot and save it to a png file

png("ExData_Plotting1/plot4.png", width = 508, height = 508, res = 72) ## I have made this file 508 pixels like the images in from the https://github.com/rdpeng/ExData_Plotting1 in order to fin in the legend text in the third plot in plot 4

par(mfrow = c(2, 2))

#Plot A
plot(powerconsumption$Datetime, powerconsumption$Global_active_power, type = "l", xlab = "", ylab = "Global Active Power")
#Plot B
plot(powerconsumption$Datetime, powerconsumption$Voltage, type = "l", xlab = "datetime", ylab = "Voltage")

#Plot C
plot(powerconsumption$Datetime, powerconsumption$Sub_metering_1, type = "l", col = "black", xlab = "", ylab = "Energy sub metering")
lines(powerconsumption$Datetime, powerconsumption$Sub_metering_2, type = "l", col = "red")
lines(powerconsumption$Datetime, powerconsumption$Sub_metering_3, type = "l", col = "blue")
legend('topright', bty = "n", xjust = 1, text.width = strwidth("100,000,000,000"),
    c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), # puts text in the legend
    lty = 1, # gives the legend appropriate symbols (lines)
    lwd = 1, col = c("black", "red", "blue")) # gives the legend lines the correct color and width

#Plot D
plot(powerconsumption$Datetime, powerconsumption$Global_reactive_power, type = "l", xlab = "datetime", ylab = "Global_reactive_power")

dev.off()





## MISC

## Format the time column
#powerconsumption$Time <- strptime(powerconsumption$Time, format = "%H:%M:%S")
#head(powerconsumption) 
#class(powerconsumption$Time)



#powerconsumption$Date <- as.Date(powerconsumption$Date, format = "%d/%m/%Y")
#head(powerconsumption)
#class(powerconsumption$Date)



#dates <- powerconsumption$Date
#times <- powerconsumption$Time
#datetime <- paste(dates, times)
#datetime <- strptime(datetime, "%Y-%m-%d %H:%M:%S")
##datetime <- as.Date(datetime, format = "%d/%m/%Y %H:%M:%S")
##Date and Time variables to Date / Time classes in R using the ?strptime() and ?as.Date() functions.

## Extract from Help on strptime()
## read in date/time info in format 'm/d/y h:m:s'
#dates <- c("02/27/92", "02/27/92", "01/14/92", "02/28/92", "02/01/92")
#times <- c("23:03:20", "22:29:56", "01:03:30", "18:21:03", "16:56:26")
#x <- paste(dates, times)
#strptime(x, "%m/%d/%y %H:%M:%S")
