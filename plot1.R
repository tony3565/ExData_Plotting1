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

plot.data <- function() {
  # Grab the data of interest
  graphData.set <- unzip.data()
  
  # Create a histogram in a PNG file
  png(file="plot1.png", width=480, height=480)
  hist(as.numeric(ts_data$Global_active_power), main = "Global Active Power", col = "red", xlab = "Global Active Power (kilowatts)")
  dev.off()
  
}

load.dataset <- function (set, features, labels) {
  
  # Generate the path
  # In this case, the directory is either going to be test or train
  # And, the data files are going to X_[test|train], y_[test|train],
  # or subject_[test|train]
  data.dir     <- paste(set, '/', sep = '')
  file.data    <- paste(data.dir, 'X_',       set, '.txt', sep = '')
  file.label   <- paste(data.dir, 'y_',       set, '.txt', sep = '')
  file.subject <- paste(data.dir, 'subject_', set, '.txt', sep = '')
  
  # Grab the identified data file, but only the columns selected
  # from the features.txt file. Also, set the column names to the
  # names acquired from features.txt 
  data <- read.table(file.data)[, features$index]
  names(data) <- features$name
  
  # Grab the identified label file, but only the first column 
  # Actually, there is only one column but this is necessary 
  # to eliminate the row number column that you get by default
  label.set  <- read.table(file.label)[,1]
  
  # Now, convert the numbers in the label file with the conversion
  # map provided by activity_labels.txt. Any matching levels from  
  # activity_labels.txt that are found in label.set are re-assigned
  # to the corresponding label provided by activity_labels.txt
  data$label <- factor(label.set, levels=labels$level, labels=labels$label)
  
  # Also, note that the number of rows in file.data and file.label are
  # identical. In the general case, the depth would have to be confirmed
  # to be identical. Here, a matching depth between the two files is presumed.
  
  # Grab the identified subject file. Again, grab the first column to
  # discard the row number (index) column. Also, the depth is expected
  # to match the depths of file.data and file.label. Really should add the
  # error check for that
  subject.set <- read.table(file.subject)[,1]
  
  # As with data$label, add another variable to data called data$subject.
  # Here, the values are just converted to type factor, but the human 
  # readable value remains the same.
  data$subject <- factor(subject.set)
  
  # Convert to data table
  data.table(data)
  
}
run.analysis <- function() {
  # Dive into the belly of the beast
  setwd('UCI HAR Dataset/')
  
  # First, need to get the features that we are interested in
  feature.set <- read.table('features.txt', col.names = c('index', 'name'))
  features    <- subset(feature.set, grepl('-(mean|std)[(]', feature.set$name))
  
  # Next, get the labels
  label.set   <- read.table('activity_labels.txt', col.names = c('level', 'label'))
  
  # Go get all that lovely data 
  train.set   <- load.dataset('train', features, label.set)
  test.set    <- load.dataset('test',  features, label.set)
  
  # Now that both train and test have been loaded, bind them together
  data.set   <- rbind(train.set, test.set)
  
  # Create the tide data set     ### .SD means something like subset of data table ###
  tidy.set    <- data.set[, lapply(.SD, mean), by=list(label,subject)]
  
  # Fix the variable names
  names <- names(tidy.set)
  names <- gsub('-mean', 'Mean', names)       # Replace '-mean' by 'Mean'
  names <- gsub('-std',  'Std',  names)       # Replace '-std'  by 'Std'
  names <- gsub('[()-]', '',     names)       # Remove all the parentheses and dashes
  setnames(tidy.set, names)                   # Replace all of the names with the new ones
  
  # Write the results above 'UCI HAR Dataset' to not soil the downloaded data
  setwd('..')
  write.csv(data.set, file = 'rawdata.csv',  row.names = FALSE)
  write.csv(tidy.set, file = 'tidydata.csv', row.names = FALSE, quote = FALSE)
  
  # Return the tide data set for inspection
  tidy.set
}

