#install and load packages
install.packages("tidyverse")
install.packages("lubridate")
install.packages("ggplot2")
library(tidyverse)
library(lubridate)
library(ggplot2)

#set working directory
getwd()
setwd("~/Downloads/bike_trips_data")

#gather data 
Apr_2021 <- read.csv("202104-divvy-tripdata.csv")
May_2021 <- read.csv("202105-divvy-tripdata.csv")
Jun_2021 <- read.csv("202106-divvy-tripdata.csv")
Jul_2021 <- read.csv("202107-divvy-tripdata.csv")
Aug_2021 <- read.csv("202108-divvy-tripdata.csv")
Sep_2021 <- read.csv("202109-divvy-tripdata.csv")
Oct_2021 <- read.csv("202110-divvy-tripdata.csv")
Nov_2021 <- read.csv("202111-divvy-tripdata.csv")
Dec_2021 <- read.csv("202112-divvy-tripdata.csv")
Jan_2022 <- read.csv("202201-divvy-tripdata.csv")
Feb_2022 <- read.csv("202202-divvy-tripdata.csv")
Mar_2022 <- read.csv("202203-divvy-tripdata.csv")

colnames(Apr_2021)
colnames(May_2021)
colnames(Jun_2021)
colnames(Jul_2021)
colnames(Aug_2021)
colnames(Sep_2021)
colnames(Oct_2021)
colnames(Nov_2021)
colnames(Dec_2021)
colnames(Jan_2022)
colnames(Feb_2022)
colnames(Mar_2022)

#combine data into one data frame
all_trips <- bind_rows(Apr_2021, May_2021, Jun_2021, Jul_2021, Aug_2021, Sep_2021, Oct_2021, Nov_2021, Dec_2021, Jan_2022, Feb_2022, Mar_2022)
#drop starting and ending lattitude/longitude rows
all_trips <- (all_trips %>%
                select(-c(start_lat, start_lng, end_lat, end_lng)))

#inspect data
nrow(all_trips)
dim(all_trips)
head(all_trips)
str(all_trips)
summary(all_trips)

#unique values
table(all_trips$member_casual) 
table(all_trips$rideable_type)

#add date, day, day of the week, month, year, and time columns
all_trips$date <- as.Date(all_trips$started_at)
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")
all_trips$time <- format(all_trips$started_at, format = "%H:%M")


#add trip length column and convert output to numeric values 
all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at)
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))

#removed docked_bike types and rides with negative times
all_trips_v2 <- all_trips[!(all_trips$rideable_type == "docked_bike" | all_trips$ride_length<=0),]

table(all_trips_v2$rideable_type)

mean(all_trips_v2$ride_length) #straight average (total ride length / rides)
median(all_trips_v2$ride_length) #midpoint number in the ascending array of ride lengths
max(all_trips_v2$ride_length) #longest ride
min(all_trips_v2$ride_length) #shortest ride 
summary(all_trips_v2$ride_length)

aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)

#order days of the week 
all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(member_casual, weekday) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n()							#calculates the number of rides and average duration 
            ,average_duration = mean(ride_length)) %>% 		# calculates the average duration
  arrange(member_casual, weekday)								# sorts

#bar graph: rides per weekday 
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(x = "Weekday", y = "Total Number of Rides", title = "Rides Per Weekday", fill = "Customer Type") +
  scale_y_continuous(breaks=c(0,100000,200000,300000,400000,500000), labels=c("0","100,000","200,000","300,000","400,000","500,000"))

#bar graph: average ride duration/week day
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs( x = "Weekday", y = "Average Ride Duration in Minutes", title = "Average Ride Duration per Weekday", fill = "Customer Type") +
  scale_y_time()

all_trips_v2

all_trips_v2 %>% 
  group_by(member_casual) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length))

#bar graph: bike type breakdown 
all_trips_v2 %>% 
  group_by(member_casual, rideable_type) %>% 
  summarise(number_of_rides = n()) %>%
  arrange(member_casual, rideable_type) %>%
  ggplot(aes(x = member_casual, y = number_of_rides, fill = rideable_type)) +
  geom_col(position = "dodge") +
  labs(x = "Customer Type", y = "Total Number of Rentals", title = "Bike Type", fill = "Bike Type") +
  scale_y_continuous(breaks=c(0,500000,1000000,1500000,2000000), labels=c("0","500,000","1,000,000","1,500,000","2,000,000"))


#bar graph: total rides per month 
all_trips_v2 %>% 
  mutate(month = month(started_at, label = TRUE)) %>% 
  group_by(member_casual, month) %>% 
  summarise(number_of_rides = n()) %>% 
  arrange(member_casual, month) %>%
  ggplot(aes(x = month, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(x = "Month", y = "Total Number of Rides", title = "Rides per Month", fill = "Customer Type") +
  scale_x_discrete(limits = month.abb) +
  scale_y_continuous(breaks=c(0,100000,200000,300000,400000), labels = c("0","100,000","200,000","300,000","400,000"))

counts <- aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)
write.csv(counts, file = 'Capstone.csv')

write.csv(all_trips_v2, file = "All_trips.csv")




