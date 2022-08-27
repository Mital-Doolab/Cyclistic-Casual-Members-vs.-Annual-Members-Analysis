## Install packages in R for Data Analysis
install.packages("tidyverse") ## This package was downloaded to help with data wrangling.
library(tidyverse)
library(janitor)  ## This package will be used to help with examining and cleaning data. 
library(lubridate) ## This package will be used convert data in our data set into date and date-time formats. 
library(tidyr) ## This package will be used for data cleaning.
library(readr) ## This package will be used to import data. 
library(data.table) ## This package will help aggregate our large data set faster. 
library(dplyr) ## This package will help us with data manipulation tasks.
library(tibble) ## This package will help us in organizing our data, if needed. 
install.packages("skimr")
library(skimr) ## This package will be used to create statistical summarize in data frames, tibbles, data tables, and vectors. 
install.packages("ggplot2") ## This package was downloaded to create data visualizations. 
library(ggplot2)
install.packages("rio") ## This package will be used to save summary tables that we may want to share in our presentation. 
library(rio)

## I want to set my working directory so I can access my data set as well as save my work in this directory so I can easily locate it when needed. I am only completing this step as I am using a desktop version of R.
getwd() ## Using this function I am going to check what my current working directory is
setwd("/Users/mitaldoolab/Desktop/Data Analytics/Bike Share/Cyclistic Bike Share analysis") ## Using this function I am going to set my working directory.

## Import and load  each data set needed for this analysis. Also, to make it easier to call each dataset I will give each data set a new simpler name.
df1 <- read.csv("2021-01-05.csv")
df2 <- read.csv("2021-02-04.csv")
df3 <- read.csv("2021-03-09.csv")
df4 <- read.csv("2021-04-08.csv")
df5 <- read.csv("2021-05-07.csv")
df6 <- read.csv("2021-06-11.csv")
df7 <- read.csv("2021-07-15.csv")
df8 <- read.csv("2021-08-14.csv")
df9 <- read.csv("2021-09-08.csv")
df10 <- read.csv("2021-10-04.csv")
df11 <- read.csv("2021-11-04.csv")
df12 <- read.csv("2021-12-08.csv")

## To ensure that each dataset was uploaded correctly and there has been no loss of data integrity I will view each data set using the "View()" function. 
View(df1)
View(df2)
View(df3)
View(df4)
View(df5)
View(df6)
View(df7)
View(df8)
View(df9)
View(df10)
View(df11)
View(df12)

## In order to analyze all the data we need to merge the datasets into one data set. However, before doing this we want to check that each dataset has the same number of columns and the same data type for each column. We will use the "compare_df_cols" function.
compare_df_cols(df1, df2, df3, df4, df5, df6, df7, df8, df9, df10, df11, df12)

## After examining and comparing the number of columns and data type for each dataset, everything is consistent between all data types. Thus we will merge all the datasets using the "rbind" function. We are also name this new data set "Cyclistic_data2021". We will give the merged dataset a new name so we can call this data set when needed for our analysis. We will also View the dataset to ensure the data combined correctly. 
Cyclistic_data2021 <- rbind(df1, df2, df3, df4, df5, df6, df7, df8, df9, df10, df11, df12)
View(Cyclistic_data2021)

## When examining the data type for each data frame earlier we noticed that the "ended_at" and "started_at" columns are in character format. For our analysis we need these two columns to be in date and time format so we will change the data type from character to data and time using the "strptime" function.
Cyclistic_data2021$started_at = strptime(Cyclistic_data2021$started_at, "%Y-%m-%d %H:%M:%S")
Cyclistic_data2021$ended_at = strptime(Cyclistic_data2021$ended_at, "%Y-%m-%d %H:%M:%S")

## To ensure that the "started-at" and "ended_at" columns have been correctly converted from character data type to date-time date type we will use the "str()" function. 
str(Cyclistic_data2021)

## We will start to analyze our data by examining the types of members ("member" or "casual" and total number of each type of member.  We will create a frequency table using the table() function. We will also call this table "member_type". After creating the table we will view the table using the "View()" function.  
total_member_type <- table(Cyclistic_data2021$member_casual)
View(total_member_type)

## Lets create a data visualization using the data we collected from our total_member_type analysis to better visualize the data. After creating our data visualization we will save our visual. 
total <- c(2489347, 2989749)
rider_type <- c("Casual Members", "Annual Members")
total_percent <- round(total/sum(total) * 100)
total_percent<- paste(total_percent, "%", sept = "") ## This will add a percentage symbol after the number
pie(total, labels = paste(total_percent), main = "Member Type", sub ="Percentage of user type", col = c("Orange", "Purple")) +
  legend("topright", cex = 0.8, legend = rider_type, fill = c("Orange", "Purple"))

## We will examine which bike types were used. We will create a frequency table using the table() function. We will also call this table "bike_type". After creating the table we will view the table using the "View()" function.
bike_type <- table(Cyclistic_data2021$rideable_type)
View(bike_type)

## Lets create a data visualization using the data we collected from our bike_type analysis to better visualize the data. After creating our data visualization we will save our visual.
barplot(bike_type, main = "Types of Bikes", xlab = "Type of Bike", ylab = "Number of Times Used", col = "Blue")

## To help us analyze our data we will be adding a column for date, month, day, day of the week, and year of each ride. 
Cyclistic_data2021$date <- as.Date(Cyclistic_data2021$started_at)
Cyclistic_data2021$month <- format(as.Date(Cyclistic_data2021$date),"%B")
Cyclistic_data2021$day <- format(as.Date(Cyclistic_data2021$date), "%d")
Cyclistic_data2021$year <- format(as.Date(Cyclistic_data2021$date), "%y")
Cyclistic_data2021$days_of_week <- format(as.Date(Cyclistic_data2021$date), "%A")

## Before moving forward with our analysis let check to make sure our data frame has been updated with the new columns we added above using the "str()" function.
str(Cyclistic_data2021)

## Now let's create a column for ride_length for all trips. Our calculations will be in seconds.
Cyclistic_data2021$ride_length <- difftime(Cyclistic_data2021$ended_at, Cyclistic_data2021$started_at, units = "mins")

## Again, before moving forward with our analysis let check to make sure our data frame has been updated with the new column "ride_length" and the data is in seconds using the "str()" function.
str(Cyclistic_data2021)

## After looking at our data preview we notice that we need to convert our ride_length column to a numeric data type from a character data type.
Cyclistic_data2021$ride_length <- as.numeric(as.character(Cyclistic_data2021$ride_length))

## Again, before moving forward with our analysis let check to make sure our data frame has been updated with the new column "ride_length" being a numeric data type as opposed to a character data type using the "str()" function.
str(Cyclistic_data2021)

## When previewing our data we saw that there were some negative ride_lengths, let's check how many ride lengths are equal to or less than 0.
nrow(subset(Cyclistic_data2021, ride_length <=0))

## Now let's check for any test rides that were made by the company for quality check. We will run 3 different codes spelling "Test" 3 different ways to ensure we check all the test rides even if they were inputted differently. 
nrow(subset(Cyclistic_data2021, start_station_name %like% "TEST"))
nrow(subset(Cyclistic_data2021, start_station_name %like% "test"))
nrow(subset(Cyclistic_data2021, start_station_name %like% "Test"))

## We will now remove data that will affect our analysis. We will remove any data that has a negative ride_length entry and any data that is a "TEST" data in which the bike was taken out for testing purpose. Also, since we are removing rows from our dataset we will also create a new version of the dataset (Cyclic_data2021.v2) to maintain the integrity of our previous data set if we need to refer back to it. 
Cyclistic_data2021.v2 <- Cyclistic_data2021 |> 
  subset(!ride_length <= 0 & 
           !start_station_name %like% "TEST")

## Again, before moving forward with my analysis I want to check that all data with "ride_length <= 0" and "start_station_name %like% "TEST" "Test" "test"' has been removed in my new version (Cyclistic_data2021.v2).  
nrow(subset(Cyclistic_data2021.v2, ride_length <=0))
nrow(subset(Cyclistic_data2021.v2, start_station_name %like% "TEST"))
nrow(subset(Cyclistic_data2021.v2, start_station_name %like% "test"))
nrow(subset(Cyclistic_data2021.v2, start_station_name %like% "Test"))

## We will also take a look at our new data frame to check that our data has not lost its integrity. We will also cross check using the "dim()" function.
str(Cyclistic_data2021.v2)
dim(Cyclistic_data2021.v2)
## The following codes are run to find descriptive analysis on ride_length (all results are in seconds).
mean(Cyclistic_data2021.v2$ride_length) ## Average ride_length in seconds
median(Cyclistic_data2021.v2$ride_length) ## Midpoint value in the range of values in seconds
max(Cyclistic_data2021.v2$ride_length) ## Maximum (longest) ride_length in seconds
min(Cyclistic_data2021.v2$ride_length) ## Minimum (shortest) ride_length in seconds

## Let's compare annual members and casual members MEAN ride length.
aggregate(Cyclistic_data2021.v2$ride_length ~ Cyclistic_data2021.v2$member_casual, FUN = mean)

## Let's compare annual members and casual members MEADIAN ride length.
aggregate(Cyclistic_data2021.v2$ride_length ~ Cyclistic_data2021.v2$member_casual, FUN = median)

## Let's compare annual members and casual members MAXIMUM ride length.
aggregate(Cyclistic_data2021.v2$ride_length ~ Cyclistic_data2021.v2$member_casual, FUN = max)

## Let's compare annual members and casual members MINIMUM ride length.
aggregate(Cyclistic_data2021.v2$ride_length ~ Cyclistic_data2021.v2$member_casual, FUN = min)

## Let's create a table comparing user type by ride_length. 
ride_length_user_type <- Cyclistic_data2021.v2 |> 
  group_by(member_casual) |> ## This code will group our results by user type
  summarise(average_ride_length = mean(ride_length), median_ride_length = median(ride_length), max_ride_length = max(ride_length), min_ride_length = min(ride_length)) ## This code will calculate the mean, median, max.
View(ride_length_user_type)

## Let's calculate the average ride time for members and casual members per a day. 
aggregate(Cyclistic_data2021.v2$ride_length ~ Cyclistic_data2021.v2$member_casual + Cyclistic_data2021.v2$days_of_week, FUN = mean)
