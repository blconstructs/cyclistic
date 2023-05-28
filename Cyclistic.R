#
# Cyclistic cap stone study
# script to extract, transform and load data
# target application is Tableau desktop via generated csv file
# input will be source data from folder with csv files
# output will be CVS file that will load with Tableau desktop
# folder paths can be predefined or queried
# apply humor when appropriate

# 
# load the libraries!!
#
# install.packages("readr")
# install.packages("dplyr")
# install.packages("lubridate")
# install.packages("tidyr")
library(readr)
library(dplyr)
library(lubridate)
library(tidyr)

#
# define any functions that we will need down the road
#
# print function to make our code more readable
pr <- function(t) print(t, quote = FALSE)
pr("================================================================================")
pr("FUNCTIONS")
pr("")

#
# define any variables that we will need down the road
#
pr("================================================================================")
pr("VARIABLES")
pr("")
myData <- NULL
myPath <- "C:/Users/Admin/Documents/Coursera/Cyclistic/data"
# uncomment next line for dialog to select path
# myPath <- choose.dir(default = myPath, caption = "Select folder")
pr(myPath)
pr("")
myFiles <- list.files(path=myPath, pattern='*.csv')
pr(myFiles)
pr("")

#
# pull in data from CSV files in the data folder
#
pr("================================================================================")
pr("LOADING the following files from data directory:")
pr("")
for (myFile in myFiles) {
  print(myFile, quote = FALSE)
  myFile <- paste(myPath, myFile, sep='/')
  if( is.null(myData) ) {
    myData <- read_csv(myFile, show_col_types = FALSE, progress = TRUE, skip_empty_rows = TRUE)
  } else {
    myData <- bind_rows(myData, read_csv(myFile, show_col_types = FALSE, skip_empty_rows = TRUE))
  }
}
if(is.null(myData)) {
  stop("=== Nothing imported! ===  HALT", call. = FALSE)
} else {
  pr("--IMPORT COMPLETED--")
  pr("")
}

#
# lets show some statistics
#
pr("================================================================================")
pr("STATISTICS")
pr("")
pr(str(myData))
pr("FROM THE TOP")
pr("")
pr(head(myData))
pr("")

#
# expected columns
# ride_id,rideable_type,started_at,ended_at,start_station_name,start_station_id,end_station_name,end_station_id,start_lat,start_lng,end_lat,end_lng,member_casual
#
# now from a simple preview of data in excel we can determine that some columns can be empty by design
# so, lets just make sure that other columns aren't empty
#
# lets run a data test to assert what integrity that we can
#
pr("================================================================================")
pr("INTEGRITY CHECKING")
pr("")
integTest1 <- nrow(subset(myData, is.na(ride_id) | is.na(rideable_type) | is.na(started_at) | is.na(ended_at) | is.na(start_lat) | is.na(start_lng) | is.na(member_casual)))
if(integTest1 == 0) {
  pr("Intergity check passed, data usable as imported.")
  pr("")
} else {
  stop("=== Intergity problem found! ===  HALT", call. = FALSE)
}

#
# lets apply modifications to observations
#
pr("================================================================================")
pr("MODIFYING COLUMNS")
pr("")
myData <- myData %>%
  # lets begin a process to juggle some timestamps, first we copy corrected values into temp columns
  mutate(
    temp_started_at = as_datetime( ifelse(started_at > ended_at, ended_at, started_at)),
    temp_ended_at = as_datetime( ifelse(started_at > ended_at, started_at, ended_at))
  ) %>%
  # next we copy corrected values back into the original timestamp columns
  mutate(
    started_at = temp_started_at,
    ended_at = temp_ended_at
  ) %>%
  # last we discard the temp columns
  mutate(
    temp_started_at = NULL,
    temp_ended_at = NULL
  ) %>%
  # in this process we correct some values to make things easier to display in Tableau
  # also we remove the ride_id column which has no reporting value
  mutate(
    rideable_type = ifelse(rideable_type == "classic_bike", "Classic", ifelse(rideable_type == "docked_bike", "Docked", ifelse(rideable_type == "electric_bike", "Electric", ""))),
    member_casual = ifelse(member_casual == "member", "Member", ifelse(member_casual == "casual", "Casual", "")),
    ride_id = NULL
  )

#
# provide a view of data
#
pr("================================================================================")
pr("VIEW DATA")
pr("")
View(myData)

#
# output data to CSV file
#
pr("================================================================================")
pr("WRITE DATA")
pr("")
myPath <- "C:/Users/Admin/Documents/Coursera/Cyclistic"
# uncomment next line for dialog to select path
# myPath <- choose.dir(default = myPath, caption = "Select folder")
if(is.na(myPath) == FALSE) {
  pr(myPath)
  outputCSVFile <- paste(myPath, "CyclisticOutput.csv", sep='/')
  # it's important here that I instruct RStudio not to mark empty values as NA
  write.csv(myData, outputCSVFile, row.names = FALSE, na = "")
} else {
  pr("--- Write skipped. ---")
}
pr("")

#
# time for cookies!
#
pr("================================================================================")
pr("PROCESS COMPLETE")
pr("")