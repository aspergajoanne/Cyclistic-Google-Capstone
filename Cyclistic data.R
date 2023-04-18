#load libraries 
library(tidyverse) #calculations
library(lubridate) #dates 
library(hms) #time
library(data.table) #exporting data frame
library(readxl) #to read xlsx files
library(skimr)
install.packages("janitor")
install.packages("skimr")
install.packages("dplyr")
install.packages ("knitr")


cyclistic_df <- rbind(feb2022_df, mar2022_df, apr2022_df, may2022_df, jun2022_df, 
                      jul2022_df, aug2022_df, sep2022_df, oct2022_df, 
                      nov2022_df, dec2022_df, jan2023_df, feb2023_df)

cyclistic_full <- cyclistic_df

rm(feb2022_df)
rm(mar2022_df)
rm(apr2022_df)
rm(may2022_df)
rm(jun2022_df)
rm(jul2022_df)
rm(aug2022_df)
rm(sep2022_df)
rm(oct2022_df)
rm(nov2022_df)
rm(dec2022_df)
rm(jan2023_df)
rm(feb2023_df)


cyclistic_full$ride_length <- difftime(cyclistic_df$ended_at, cyclistic_df$started_at, units = "mins")
View(cyclistic_full)

cyclistic_full <- cyclistic_full %>% 
  rename( customers=member_casual)


glimpse(cyclistic_full)
skim_without_charts(cyclistic_full)
colnames(cyclistic_full)  
nrow(cyclistic_full)
dim(cyclistic_full)  
head(cyclistic_full)  
tail(cyclistic_full)
str(cyclistic_full) 
summary(cyclistic_full) 

cyclistic_full$date <- as.Date(cyclistic_full$started_at) #The default format is yyyy-mm-dd
cyclistic_full$month <- format(as.Date(cyclistic_full$date), "%m")
cyclistic_full$day <- format(as.Date(cyclistic_full$date), "%d")
cyclistic_full$day_of_week <- wday(cyclistic_full$started_at) #calculate the day of the week 
cyclistic_full$day_of_week <- format(as.Date(cyclistic_full$date), "%A") #create column for day of week
cyclistic_full$year <- format(as.Date(cyclistic_full$date), "%Y")
cyclistic_full$day_of_week <- format(as.Date(cyclistic_full$date), "%A")
cyclistic_full$hour <- (format(cyclistic_full$started_at,format="%H")) #create new column for hour
#create new column for time
cyclistic_full$date <- as.Date(cyclistic_full$started_at)
cyclistic_full$time <- format(cyclistic_full$started_at,"%H:%M:%S")
cyclistic_full$month <- format(as.Date(cyclistic_full$date), "%m")

cyclistic_full <- cyclistic_full[!(cyclistic_full$ride_length <=0),] #remove where ride_length is 0 or negative


cyclistic_full <-cyclistic_full %>% mutate(season = 
                                             case_when(month == "03" ~ "Spring",
                                                       month == "04" ~ "Spring",
                                                       month == "05" ~ "Spring",
                                                       month == "06"  ~ "Summer",
                                                       month == "07"  ~ "Summer",
                                                       month == "08"  ~ "Summer",
                                                       month == "09" ~ "Fall",
                                                       month == "10" ~ "Fall",
                                                       month == "11" ~ "Fall",
                                                       month == "12" ~ "Winter",
                                                       month == "01" ~ "Winter",
                                                       month == "02" ~ "Winter")
)

#create column for different time_of_day: Night, Morning, Afternoon, Evening
cyclistic_full <-cyclistic_full %>% mutate(time_of_day = 
                                             case_when(hour == "00" ~ "Night",
                                                       hour == "01" ~ "Night",
                                                       hour == "02" ~ "Night",
                                                       hour == "03" ~ "Night",
                                                       hour == "04" ~ "Night",
                                                       hour == "05" ~ "Night",
                                                       hour == "06" ~ "Morning",
                                                       hour == "07" ~ "Morning",
                                                       hour == "08" ~ "Morning",
                                                       hour == "09" ~ "Morning",
                                                       hour == "10" ~ "Morning",
                                                       hour == "11" ~ "Morning",
                                                       hour == "12" ~ "Afternoon",
                                                       hour == "13" ~ "Afternoon",
                                                       hour == "14" ~ "Afternoon",
                                                       hour == "15" ~ "Afternoon",
                                                       hour == "16" ~ "Afternoon",
                                                       hour == "17" ~ "Afternoon",
                                                       hour == "18" ~ "Evening",
                                                       hour == "19" ~ "Evening",
                                                       hour == "20" ~ "Evening",
                                                       hour == "21" ~ "Evening",
                                                       hour == "22" ~ "Evening",
                                                       hour == "23" ~ "Evening")
)

cyclistic_date <- cyclistic_date %>% 
  rename( customers=member_casual)

summary(cyclistic_full)
str(cyclistic_full)
colnames(cyclistic_full)

cyclistic_full %>%
  group_by(customers, rideable_type) %>% 
  count(rideable_type)

nrow(cyclistic_full)

cyclistic_full%>%
  group_by(customers) %>% 
  count(customers)

cyclistic_full %>%
  group_by(rideable_type) %>% 
  count(rideable_type)

cyclistic_full %>%
  group_by(customers) %>% 
  count(hour) %>% 
  print(n = 48) #lets you view the entire tibble

#total rides
cyclistic_full %>%
  count(hour) %>% 
  print(n = 24) #lets you view the entire tibble

cyclistic_full %>%
  group_by(customers) %>% 
  filter(time_of_day == "Morning") %>% 
  count(time_of_day)

cyclistic_full %>%
  filter(time_of_day == "Morning") %>% 
  count(time_of_day)

cyclistic_full %>%
  group_by(customers) %>% 
  filter(time_of_day == "Afternoon") %>% 
  count(time_of_day)

cyclistic_full %>%
  filter(time_of_day == "Afternoon") %>% 
  count(time_of_day)

cyclistic_full %>%
  group_by(customers) %>% 
  filter(time_of_day == "Evening") %>% 
  count(time_of_day)

cyclistic_full %>%
  filter(time_of_day == "Evening") %>% 
  count(time_of_day)

cyclistic_full %>%
  group_by(customers) %>% 
  filter(time_of_day == "Night") %>% 
  count(time_of_day)

cyclistic_full %>%
  filter(time_of_day == "Night") %>% 
  count(time_of_day)

cyclistic_full %>%
  group_by(customers) %>% 
  count(time_of_day)

cyclistic_full %>%
  group_by(time_of_day) %>% 
  count(time_of_day)

#total rides by member type
cyclistic_full %>%
  group_by(customers) %>% 
  count(day_of_week)

#total rides 
cyclistic_full %>%
  count(day_of_week)

#total rides by member type
cyclistic_full %>%
  group_by(customers) %>% 
  count(day) %>% 
  print(n = 62) 

#total rides
cyclistic_full %>%
  count(day) %>% 
  print(n = 31) 

cyclistic_full %>%
  group_by(customers) %>% 
  count(month) %>% 
  print(n = 24) #lets you view the entire tibble

cyclistic_full %>%
  count(month) 

cyclistic_full %>%
  group_by(customers) %>% 
  filter(season == "Spring") %>% 
  count(season)

#total rides
cyclistic_full %>%
  filter(season == "Spring") %>% 
  count(season)

cyclistic_full %>%
  group_by(customers) %>% 
  filter(season == "Summer") %>% 
  count(season)

#total rides
cyclistic_full %>%
  filter(season == "Summer") %>% 
  count(season)

cyclistic_full %>%
  group_by(customers) %>% 
  filter(season == "Fall") %>% 
  count(season)

#total rides
cyclistic_full %>%
  filter(season == "Fall") %>% 
  count(season)

#total rides by member type
cyclistic_full %>%
  group_by(customers) %>% 
  filter(season == "Winter") %>% 
  count(season)

#total rides 
cyclistic_full %>%
  filter(season == "Winter") %>% 
  count(season)

#total rides by member type
cyclistic_full %>%
  group_by(season, customers) %>% 
  count(season)

#total rides
cyclistic_full %>%
  group_by(season) %>% 
  count(season)

cyclistic_avgRide <- mean(cyclistic_full$ride_length)
print(cyclistic_avgRide)

cyclistic_full %>% group_by( customers) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

cyclistic_full%>%
  group_by(customers) %>%
  summarize(total_minutes = sum(ride_length))

cyclistic_full %>% group_by(hour, customers) %>% 
  summarise_at(vars(ride_length),
               list(time = mean)) %>% 
  print(n=48) #lets you view entire tibble

cyclistic_full %>% group_by(hour) %>% 
  summarise_at(vars(ride_length),
               list(time = mean)) %>% 
  print(n=24) #lets you view entire tibble

#average ride length by member type
cyclistic_full %>% 
  group_by(customers) %>% 
  filter(time_of_day == "Morning") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))



#average ride length
cyclistic_full %>% 
  filter(time_of_day == "Morning") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

cyclistic_full %>% 
  group_by(customers) %>% 
  filter(time_of_day == "Afternoon") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#average ride length
cyclistic_full %>% 
  filter(time_of_day == "Afternoon") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

cyclistic_full %>% 
  group_by(customers) %>% 
  filter(time_of_day == "Evening") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#average ride length
cyclistic_full %>% 
  filter(time_of_day == "Evening") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

cyclistic_full %>% 
  group_by(customers) %>% 
  filter(time_of_day == "Night") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#average ride length
cyclistic_full %>% 
  filter(time_of_day == "Night") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#average ride length by member type
cyclistic_full %>% 
  group_by(time_of_day, customers) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#average ride length
cyclistic_full %>% 
  group_by(time_of_day) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

cyclistic_full %>% group_by(customers, day_of_week) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

cyclistic_full %>% group_by(day, customers) %>% 
  summarise_at(vars(ride_length),
               list(time = mean)) %>% 
  print(n=62)  #lets you view entire tibble

#average ride_length
cyclistic_full %>% group_by(day) %>% 
  summarise_at(vars(ride_length),
               list(time = mean)) %>% 
  print(n=31)  #lets you view entire tibble

#average ride_length 
cyclistic_full %>% group_by(day_of_week) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

cyclistic_full %>% group_by(month, customers) %>% 
  summarise_at(vars(ride_length),
               list(time = mean)) %>% 
  print(n=24)  #lets you view entire tibble

#average ride_length
cyclistic_full %>% group_by(month) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

cyclistic_full %>% 
  group_by(customers) %>% 
  filter(season == "Spring") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#average ride length
cyclistic_full %>% 
  filter(season == "Spring") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

cyclistic_full %>% 
  group_by(customers) %>% 
  filter(season == "Summer") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#average ride length for summer 
cyclistic_full %>% 
  filter(season == "Summer") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

cyclistic_full %>% 
  group_by(customers) %>% 
  filter(season == "Fall") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#average ride length
cyclistic_full %>% 
  filter(season == "Fall") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

cyclistic_full %>% 
  group_by(customers) %>% 
  filter(season == "Winter") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#average ride length
cyclistic_full %>% 
  filter(season == "Winter") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

cyclistic_full %>% 
  group_by(season, customers) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#average ride length 
cyclistic_full %>% 
  group_by(season) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#created a new dataframe to use in Tableau
cyclistic_tableau <- cyclistic_full

#clean the data
cyclistic_tableau <- cyclistic_tableau %>%
  select(-c(time, started_at, ended_at)) #remove columns not needed: time, started_at, ended_at

#download the new data as a .csv file
fwrite(cyclistic_tableau,"cyclistic_full.csv")
