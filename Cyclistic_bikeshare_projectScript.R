# This is the SQL script used to import, compile and analyse the Cyclistic Case 
# Study for the Capstone Project of the Google Data Analytics Professional Certificate.
# It uses the data from Divvy Bikes (https://www.divvybikes.com/system-data).
# The database used is Oracle Database 18c Express Edition.
# The steps followed:
# 1. Download the individual CSV documents
# 2. Import each into separate tables, regularizing data types
# 3. Combine all the data into one table
# 4. Inspect data for anomalies
# 5. Identify and exclude data with anomalies
# 6. Create queries for data visualisations


# Step 1: Adding datasets into RStudio

> Feb_2022 <- read.csv("~/Downloads/202202-divvy-tripdata.csv")
>   View(Feb_2022)
> Mar_2022 <- read.csv("~/Downloads/202203-divvy-tripdata.csv")
>   View(Mar_2022)
> Apr_2022 <- read.csv("~/Downloads/202204-divvy-tripdata.csv")
>   View(Apr_2022)
> May_2022 <- read.csv("~/Downloads/202205-divvy-tripdata.csv")
>   View(May_2022)
> Jun_2022 <- read.csv("~/Downloads/202206-divvy-tripdata.csv")
>   View(Jun_2022)
> Jul_2022 <- read.csv("~/Downloads/202207-divvy-tripdata.csv")
>   View(Jul_2022)
> Aug_2022 <- read.csv("~/Downloads/202208-divvy-tripdata.csv")
>   View(Aug_2022)
> Sep_2022 <- read.csv("~/Downloads/202209.divvy.tripdata.csv")
>   View(Sep_2022)
> Oct_2022 <- read.csv("~/Downloads/202210.divvy.tripdata.csv")
>   View(Oct_2022)
> Nov_2022 <- read.csv("~/Downloads/202211.divvy.tripdata.csv")
>   View(Nov_2022)

# Step 2/3: Left Joined datasets to merge into 1 frame

> Dec_Jan_data <- merge(x = Dec_2021, y = Jan_2022, all = TRUE)
> Feb_Mar_data <- merge(x = Feb_2022, y = Mar_2022, all = TRUE)
> Apr_May_data <- merge(x = Apr_2022, y = May_2022, all = TRUE)
> Jun_Jul_data <- merge(x = Jun_2022, y = Jul_2022, all = TRUE)
> Aug_Sep_data <- merge(x = Aug_2022, y = Sep_2022, all = TRUE)
> Oct_Nov_data <- merge(x = Oct_2022, y = Nov_2022, all = TRUE)
> Dec_to_Mar_data <- merge(x = Dec_Jan_data, y = Feb_Mar_data, all = TRUE)
> Apr_to_Jul_data <- merge(x = Apr_May_data, y = Jun_Jul_data, all = TRUE)
> Aug_to_Nov_data <- merge(x = Aug_Sep_data, y = Oct_Nov_data, all = TRUE)
> Dec_to_Jul_data <- merge(x = Dec_to_Mar_data, y = Apr_to_Jul_data, all = TRUE)
> Cyclistic_12mo_data <- merge(x = Dec_to_Jul_data, y = Aug_to_Nov_data, all = TRUE)

# checking data

> colnames(Cyclistic_12mo_data)
> str(Cyclistic_12mo_data)
> head(Cyclistic_12mo_data)
> View(Cyclistic_12mo_data)

# Step 4/5: Inspected data for any anomalies to keep in mind for analysis

# Added col day of the week to record which day the ride occured
> Cyclistic_12mo_data$day_of_the_week <- wday(Cyclistic_12mo_data$started_at, week_start = 1)
> Cyclistic_12mo_data$day_of_the_week <- wday(Cyclistic_12mo_data$started_at, label = TRUE)


# Added cols to calculate the length of rides in hours and mins
Cyclistic_12mo_data_2$ride_length_hours <- round(difftime(Cyclistic_12mo_data_2$End_Time, Cyclistic_12mo_data_2$Start_Time, units = "hours"), digits = 2)
Cyclistic_12mo_data_2$ride_length_mins <- round(difftime(Cyclistic_12mo_data_2$End_Time, Cyclistic_12mo_data_2$Start_Time, units = "mins"), digits = 2)


# Removing any duplicate ride_id from table --- all unique entries
Cyclistic_12mo_data_2 <- Cyclistic_12mo_data %>% distinct(ride_id, .keep_all = TRUE)

# Checking for any inconsistencies
> unique(Cyclistic_12mo_data$rideable_type)
> unique(Cyclistic_12mo_data$member_casual)

# Checked overview of data
> summary(Cyclistic_12mo_data)
> skim_without_charts(Cyclistic_12mo_data)

# Removed null, na values
> Cyclistic_12mo_data_2 <- na.omit(Cyclistic_12mo_data)

# Split date and time
Cyclistic_12mo_data_2[c('Start_Date', 'Start_Time')] <- str_split_fixed(Cyclistic_12mo_data_2$started_at, ' ', 2)
Cyclistic_12mo_data_2[c('End_Date', 'End_Time')] <- str_split_fixed(Cyclistic_12mo_data_2$ended_at, ' ', 2)



# Step 6: Creating visualizations in RStudio
ggplot(Cyclistic_12mo_data_2, aes(day_of_the_week)) + 
  geom_density(aes(fill=factor(rideable_type)), alpha =0.8) + 
  labs(title ="Daily Rides Density plot",x="Day of the Week",y="Density",fill = "Ride Type")



ggplot(data = Cyclistic_12mo_data_2) +
  geom_point(mapping = aes(x = ride_length_hours, y = day_of_the_week))


