# Preprocessing for implementing PSF Algorithm
#
# F. Martinez Alvarez, A. Troncoso, J. C. Riquelme, and J. S. Aguilar Ruiz, 
#"Energy Time Series Forecasting Based on Pattern Sequence Similarity," IEEE Transactions on Knowledge and Data Engineering, vol. 23, no. 8, pp. 1230-1243, Aug. 2011.

setwd("D:/Research Work/Smart Meter/smart-meter-research/psf-algorithm/code/R")

library(lubridate)
library(xts)

TIME_ZONE <- "UTC"

FROM_DATE <- "2012-01-01"
TO_DATE   <- "2014-12-31"

BUILDING_NAME <- "B162 Alice Hoy.csv"

PED_SENSOR_NAME <- "Webb-Bridge-24-dim"
INPUT_FILE  <- paste("D:/Research Work/Smart Meter/smart-meter-research/data/melbourne-pedestrian-data/2016/smart-meter-format/",PED_SENSOR_NAME,".csv", sep="")
OUTPUT_FILE <- paste("D:/Research Work/Smart Meter/smart-meter-research/data/melbourne-pedestrian-data/2016/smart-meter-format/psf-format/psf-",tolower(PED_SENSOR_NAME),".csv", sep="")

NUM_CLUSTERS  <- 5 


calc_psf_label_representation <- function(from_date, to_date, input_file){
  data_subset                            <- read_subset_of_data(from_date, to_date, input_file) 
  data_subset_missing_removed            <- na.omit(data_subset)
  
  data_subset_aggregated_hourly          <- aggregate_hourly(data_subset_missing_removed)
  hourly_values_for_every_day            <- get_hourly_values_for_every_day(data_subset_aggregated_hourly) #get data in 24-dimension vector format for each day
  normalized_hourly_values_for_every_day <- normalize_hourly_values_for_every_day(hourly_values_for_every_day)
  
  fit <- kmeans(normalized_hourly_values_for_every_day, NUM_CLUSTERS)
  
  normalized_hourly_values_with_cluster <- cbind(normalized_hourly_values_for_every_day, fit$cluster) 
  
  return (normalized_hourly_values_with_cluster)
}

#nemmco data already formatted through java to be in 24-dimension vector format for each day
calc_psf_label_representation_with_nemmco_data <- function(input_file){
  hourly_values_for_every_day            <- read.csv(input_file, head=FALSE, sep=",")
  hourly_values_for_every_day[1]         <- NULL; #remove date and just have the values
  
  normalized_hourly_values_for_every_day <- normalize_hourly_values_for_every_day(hourly_values_for_every_day)
  
  fit <- kmeans(normalized_hourly_values_for_every_day, NUM_CLUSTERS)
  
  normalized_hourly_values_with_cluster <- cbind(normalized_hourly_values_for_every_day, fit$cluster) 
  
  return (normalized_hourly_values_with_cluster)
}


#different representation of nemmco data (ex - periods of day). 
#this is a preformatted file created using java (create-varied-representations.iml) so that each value is a normalized value. 
calc_psf_label_representation_with_different_load_representation <- function(input_file){
  periodic_values_for_every_day          <- read.csv(input_file, head=TRUE, sep=",")
  periodic_values_for_every_day[1]       <- NULL; #remove date and just have the values
  
  fit <- kmeans(periodic_values_for_every_day, NUM_CLUSTERS)
  
  periodic_values_for_every_day_with_cluster <- cbind(periodic_values_for_every_day, fit$cluster) 
  
  return (periodic_values_for_every_day_with_cluster)
}


read_subset_of_data <- function(from_date, to_date, input_file){
  rawdata               <- read.csv(input_file, head=TRUE, sep=",")
  rawdata$Date          <- as.Date (rawdata$Date, "%d %b %Y", t)
  data_subset           <- subset  (rawdata, Date >= as.Date(from_date,"%Y-%m-%d", tz=TIME_ZONE) & Date <= as.Date(to_date,"%Y-%m-%d", tz=TIME_ZONE))
  data_subset$Date_Time <- paste(data_subset$Date, data_subset$Time, sep=" ") #add Date_Time column by concatenating Date and Time
  
  return (data_subset)
}



aggregate_hourly <- function(data_subset){
  timeseries.xts <- as.xts(data_subset$kWh, as.POSIXct(data_subset$Date_Time, tz=TIME_ZONE), descr="xts object on data_subset") #create xts object
  hourlysum.xts  <- period.apply(timeseries.xts, INDEX=endpoints(timeseries.xts, "hours"), FUN=sum) #aggregate 15-min to hourly
  
  data_subset_hourly           <- data.frame(Date_Time=index(hourlysum.xts), kWh=coredata(hourlysum.xts)) #convert xts to dataframe
  data_subset_hourly$Date_Time <- trunc(data_subset_hourly$Date_Time, "hour") #truncate minutes  (convert timestamps from 01:45:00->01:00:00)
  
  return(data_subset_hourly)
}



get_hourly_values_for_every_day <- function(data_subset_aggregated_hourly){
  
  #gives list of data frames, 1 data frame for each day
  data_daily_values           <- split(data_subset_aggregated_hourly, as.Date(data_subset_aggregated_hourly$Date_Time, tz=TIME_ZONE)) 
  
  hourly_values_for_every_day <- data.frame(matrix(NA, nrow=0, ncol=0))
  date_names <- list()
  for (hourly_values_for_one_day in data_daily_values){
    hourly_values_for_every_day <- rbind(hourly_values_for_every_day, hourly_values_for_one_day$kWh) 
    
    #append to date_names list to use in rownames. convert date_time into just date and then to char
    date_names <- c(date_names, as.character(trunc(hourly_values_for_one_day$Date_Time,"day")[1])) 
  }
  rownames(hourly_values_for_every_day) <- date_names
  colnames(hourly_values_for_every_day) <- c(1:24) #hours in day
  
  return(hourly_values_for_every_day)
}



normalize_hourly_values_for_every_day <- function(hourly_values_for_every_day){
  #do the function for each day, MARGIN=1 applies over each row
  normalized_hourly_values_for_every_day <- apply(hourly_values_for_every_day, MARGIN=1, function(x){ 
    max_of_row <- max(x) #max of the day
    x <- x / max_of_row
    x <- c(x, max_of_row)#append max of row so can recreate raw data when needed by simply multiplying
  })
  
  #output of above has hours as row names. we want days as row names. therefore transpose matrix
  normalized_hourly_values_for_every_day <- t(normalized_hourly_values_for_every_day) 
  
  return(normalized_hourly_values_for_every_day)
}


if(file.exists(OUTPUT_FILE)){
  file.remove(OUTPUT_FILE)
}
# normalized_labeled_data <- calc_psf_label_representation(FROM_DATE, TO_DATE, INPUT_FILE)
normalized_labeled_data <- calc_psf_label_representation_with_nemmco_data(INPUT_FILE)
colnames(normalized_labeled_data)[25] <- "max_val_of_day"
colnames(normalized_labeled_data)[26] <- "cluster_number"

#cbind with rownames needed to write the date values to csv for each row. row.names=TRUE messes up the structure because also writing col.names
write.table(cbind(rownames(normalized_labeled_data), normalized_labeled_data), file=OUTPUT_FILE, append=TRUE, 
            col.names=!file.exists(OUTPUT_FILE), row.names = FALSE, sep=",")


