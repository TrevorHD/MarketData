##### Load libraries --------------------------------------------------------------------------------------

# Load libraries
library(tidyverse)





##### Clean data sets and add derived variables -----------------------------------------------------------

# List of names to access data and assign to global environment
names <- c("DJDaily", "DJWeekly", "NDDaily", "NDWeekly", "SPDaily", "SPWeekly")

# Loop through names, performing operations for each of 6 data sets
for(i in 1:6){
  
  # Download data
  data <- read.csv(paste0("https://raw.githubusercontent.com/TrevorHD/MarketData/master/",
                          names[i], ".csv"))
  
  # Remove rows with missing entries
  data <- na.omit(data)
  
  # Remove entries prior to 1986, then remove date column
  data$Date <- as.Date(data$Date)
  data <- data[data[["Date"]] >= "1986-01-01", ]
  data <- data[, -1]
  
  # Add net percent change over previous 5 trading days/weeks
  # Add (geometric) mean percent change over previous 5 trading days/weeks
  data %>% 
    mutate(Prev5GM = (((1 + Lag1/100)*(1 + Lag2/100)*(1 + Lag3/100)*(1 + Lag4/100)*(1 + Lag5/100))^(1/5) - 1)*100,
           Prev5Pct = ((100 + Lag1)*(100 + Lag2)*(100 + Lag3)*(100 + Lag4)*(100 + Lag5)/(100^5) - 1)*100) -> data
  
  # Reorder columns
  data <- data[, c(2, 8, 3:7, 9:10, 1)]
  
  # Assign data to variable
  assign(names[i], data)}

# Clean unnecessary variables
remove(data, i, names)
