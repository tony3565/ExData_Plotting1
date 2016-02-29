# Exploratory Data Analysis - Homework Assignment #1 - Plot #2

# Load necessary libraries
library(datasets)
library(data.table)

# Unzip the data from a specific file. It would probably be better to 
# pass this information, but oh well...
unzip.data <- function () {
  # So, here is the very specific zip file
  zipFile <- 'exdata-data-household_power_consumption.zip'
  
  # The file in question
  if (!file.exists(zipFile)) {
    print("Zip file not found")
  }
  else {
    # Hey, at least we can grab the name
    zipFileStats <- unzip(zipFile, list = TRUE)
    
    # Let's see what we got
    unzip(zipFile)
    
    data.plot <- read.table(zipFileStats$Name, sep = ";", header = TRUE, stringsAsFactors = FALSE)
    
    # This is really not the advisable way of doing this, but quite frankly, time is short and I don't have a choice
    data      <- subset(data.plot, grepl('(2007-02-01|2007-02-02)', as.Date(data.plot$Date, "%d/%m/%Y")))
    
    return(data)
  }
}

plot4.data <- function() {
  # Grab the data of interest
  graphData.set <- unzip.data()
  
  # Create a histogram in a PNG file
  png(file="plot4.png", width=480, height=480)
  
  # Set up 4 plots
  par(mfrow = c(2,2), mar = c(4, 4, 2, 1))
  
  # Set up the plot without data and no X-axis
  plot(graphData.set$Global_active_power, type = "n", ylab = "Global Active Power (kilowatts)", xaxt = "n", xlab = "")
  axis(1, at = c(0, 1440, 2880), labels = c("Thu", "Fri", "Sat"))
  lines(graphData.set$Global_active_power)

  # Set up the plot without data and no X-axis
  plot(graphData.set$Voltage, type = "n", ylab = "Voltage", xaxt = "n", xlab = "datetime")
  axis(1, at = c(0, 1440, 2880), labels = c("Thu", "Fri", "Sat"))
  lines(graphData.set$Voltage)
  
  # Set up the plot without data and no X-axis
  plot(graphData.set$Sub_metering_1, type = "n", ylab = "Energy sub metering", xaxt = "n", xlab = "")
  axis(1, at = c(0, 1440, 2880), labels = c("Thu", "Fri", "Sat"))
  lines(graphData.set$Sub_metering_1              )
  lines(graphData.set$Sub_metering_2, col = "red" )
  lines(graphData.set$Sub_metering_3, col = "blue")
  legend("topright", lty = 1, lwd = 2, col = c("black", "red", "blue"), legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), cex = 0.75)

  # Set up the plot without data and no X-axis
  plot(graphData.set$Global_reactive_power, type = "n", ylab = "Global_reactive_power", xaxt = "n", xlab = "datetime")
  axis(1, at = c(0, 1440, 2880), labels = c("Thu", "Fri", "Sat"))
  lines(graphData.set$Global_reactive_power)
  
  # Shut it down
  dev.off()
  
}