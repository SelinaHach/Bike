#Load Packages
library(tidyverse)
library(lubridate)
library(dplyr)
library(janitor)
library(data.table)
library(readr)
library(psych)
library(hrbrthemes)
library(ggplot2)

#Combine Data for smooth workflow
compare_df_cols(trip_202201, trip_202202, trip_202203, trip_202204, trip_202205, trip_202206, trip_202207, trip_202208, trip_202209, trip_202210, trip_202211, trip_202212, return = "mismatch")

#Remove data was dropped beginning 
data_trips <- data_trips %>%
  select(-c(start_lat, start_lng, end_lat, end_lng))

#List of column names
colnames(data_trips)

#How many rows are in data frame?
nrow(data_trips)

#Dimensions of the data frame?
dim(data_trips)

#See the first 6 rows of data frame
head(data_trips)

#See the last 6 rows of data frame
tail(data_trips)

#See list of columns and data types (numeric, character, etc)
str(data_trips)

#Statistical summary of data. Mainly for numerics
summary(data_trips)

#Remove data where started_at is greater than ended_at
data_trips <- data_trips %>%
  filter(started_at < ended_at)

#Remove duplicates
data_trips <- distinct(data_trips)

#Separate date in date, day, month, year for better analysis
data_trips$date <- as.Date(data_trips$started_at) #The default format is yyyy-mm-dd
data_trips$month <- format(as.Date(data_trips$date), "%m")
data_trips$day <- format(as.Date(data_trips$date), "%d")
data_trips$year <- format(as.Date(data_trips$date), "%Y")
data_trips$day_of_week <- format(as.Date(data_trips$date), "%A")

#Add ride length column
data_trips$ride_length <- difftime(data_trips$ended_at, data_trips$started_at)

#Inspect the structure of the columns
str(data_trips)

#Convert “ride_length” from Factor to numeric
data_trips$ride_length <- as.numeric(as.character(data_trips$ride_length))

#Remove “bad” data
clean_data_trips <- data_trips[!data_trips$ride_length<0,]

#Save the clean data
write.csv(clean_data_trips,"data\\clean_data_trips.csv",row.names = FALSE)

#Order the clean data
clean_data_trips$month <- factor(data_trips$month, levels = c("Jan_22", "Feb_22", "Mar_22", 
                                                      "Apr_22", "May_22", "Jun_22", 
                                                      "Jul_22", "Aug_22", "Sep_22", 
                                                      "Oct_22", "Nov_22", "Dec_22"))

#Analysis:- min, max, median, average
summary(clean_data_trips$ride_length)

#Compare members and casual users
#Compare of mean, median, max and min
aggregate(clean_data_trips$ride_length ~ clean_data_trips$member_casual, FUN = mean)

aggregate(clean_data_trips$ride_length ~ clean_data_trips$member_casual, FUN = median)

aggregate(clean_data_trips$ride_length ~ clean_data_trips$member_casual, FUN = max)

aggregate(clean_data_trips$ride_length ~ clean_data_trips$member_casual, FUN = min)

#Average duration per rider type sorted by day of the week
#Order the data
clean_data_trips$day_of_week <- ordered(clean_data_trips$day_of_week, levels = c("Sunday", "Monday", "Tuesday", 
                                                                                 "Wednesday", "Thursday", 
                                                                                 "Friday", "Saturday"))

aggregate(clean_data_trips$ride_length ~ clean_data_trips$member_casual + clean_data_trips$day_of_week, FUN = mean)

#Average duration sorted by rider type, then day of the week
clean_data_trips %>% 
  group_by(member_casual, day_of_week) %>% 
  summarise(number_of_ride = n(),
            avgerage_duration = mean(ride_length),
            median_duration = median(ride_length),
            max_duration = max(ride_length),
            min_duration = min(ride_length))

#Save data for Visualizations
clean_data_trips_viz <- clean_data_trips %>% 
  select(rideable_type, started_at, ended_at, start_station_name, end_station_name, member_casual, month, day, year, day_of_week, ride_length)

write.csv(clean_data_trips_viz,"data\\clean_data_trips_viz.csv",row.names = FALSE)

#Visualizations
write.csv(clean_data_trips_viz, "clean_data_trips_viz.csv", row.names = FALSE)






