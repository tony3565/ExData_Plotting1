# Exploratory Data Analysis - Homework Assignment #1 - Plot #1

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

plot1.data <- function() {
  # Grab the data of interest
  graphData.set <- unzip.data()
  
  # Create a histogram in a PNG file
  png(file="plot1.png", width=480, height=480)
  hist(as.numeric(graphData.set$Global_active_power), main = "Global Active Power", col = "red", xlab = "Global Active Power (kilowatts)")
  dev.off()
  
}

