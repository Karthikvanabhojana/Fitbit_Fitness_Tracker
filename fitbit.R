library(tidyverse)  #helps wrangle data
library(lubridate)  #helps wrangle date attributes
library(ggplot2)  #helps visualize data

#import dailyActivity_merged.xlsx
library(readxl)

dailyActivity_merged <- read.csv("Fitabase Data 4.12.16-5.12.16/dailyActivity_merged.csv")
daily_calories <- read.csv("Fitabase Data 4.12.16-5.12.16/dailyCalories_merged.csv")
sleep_day <- read.csv("Fitabase Data 4.12.16-5.12.16/sleepDay_merged.csv")
daily_intensities <- read.csv("Fitabase Data 4.12.16-5.12.16/dailyIntensities_merged.csv")
weight_log <- read.csv("Fitabase Data 4.12.16-5.12.16/weightLogInfo_merged.csv")
head(dailyActivity_merged)

colnames(dailyActivity_merged) #know coloun names

glimpse(dailyActivity_merged) #This is like a transposed version of print: columns run down the page, and data runs across

head(daily_calories)
colnames(daily_calories)

head(sleep_day)
colnames(sleep_day)

head(daily_intensities)
colnames(daily_intensities)

head(weight_log)
colnames(weight_log)


# all the datasets has Id as common field so can use id as primary field for this.
#It looks like the daily_activity, daily_calories, and daily_intensities have the exact same number of observations.

#Furthermore, it seems the daily_activity table might have a log of calories and intensities already, so we should confirm that the values actually match for any given ‘ID’ number.
#Lets write the SQL query /syntax to see if there are any values in daily_calories that are in dailyActivity_merged
#so created temp data frame

dailyActivity_merged2<- dailyActivity_merged %>%
  select(Id,ActivityDate,Calories)

head(dailyActivity_merged2)

install.packages('sqldf')
library(sqldf)

#finding similar elements from 2 tables
sql_check <- sqldf('Select * From dailyActivity_merged2 INTERSECT SELECT * FROM daily_calories')
head(sql_check)

nrow(sql_check)#number of rows

#creating temporary data set to store values
dailyActivity_merged3<- dailyActivity_merged %>%
  select(Id, ActivityDate, SedentaryMinutes, LightlyActiveMinutes, FairlyActiveMinutes, VeryActiveMinutes, SedentaryActiveDistance, LightActiveDistance, ModeratelyActiveDistance, VeryActiveDistance)

head(dailyActivity_merged3)


sql_check1 <- sqldf('Select * from dailyActivity_merged3 INTERSECT select * from daily_intensities')
head(sql_check1)
nrow(sql_check1)


n_distinct(dailyActivity_merged$Id)
n_distinct(sleep_day$Id)
n_distinct(weight_log$Id)

#How many observations are there in each dataframe?
nrow(dailyActivity_merged)
nrow(sleep_day)
nrow(weight_log)

#summary for tables
dailyActivity_merged %>%
  select(TotalSteps,
         TotalDistance,
         SedentaryMinutes,
         VeryActiveMinutes) %>%
  summary()

sleep_day %>%  
  select(TotalSleepRecords,
         TotalMinutesAsleep,
         TotalTimeInBed) %>%
  summary()

weight_log %>%  
  select(WeightPounds,
         BMI) %>%
  summary()

ggplot(data=dailyActivity_merged, aes(x=TotalSteps, y=SedentaryMinutes, color = Calories)) + geom_point()
ggplot(data=dailyActivity_merged, aes(x=TotalSteps, y = Calories))+ geom_point() + stat_smooth(method~lm)

#differences between the observed values and the the estimated value.


calories.lm <- lm(Calories ~ TotalSteps, data = dailyActivity_merged)
calories.res <- resid(calories.lm)

plot(dailyActivity_merged$TotalSteps, calories.res, ylab="Residuals",
     xlab = "Total Steps", main = "Calories Burned")
abline(0,0)
#mess
plot(density(calories.res))


#Let’s look at our sleep data, we should see a practically 1:1 trend from the amount of time slept and the total time someone spends in bed.

ggplot(data=sleep_day,aes(x=TotalMinutesAsleep,y=TotalTimeInBed))+geom_point()

combined_sleep_day_data <- merge(sleep_day,dailyActivity_merged,by="Id")
head(combined_sleep_day_data)

n_distinct(combined_sleep_day_data$Id)

combined_sleep_day_data2 <- merge(sleep_day,dailyActivity_merged,by="Id",all=TRUE)
head(combined_sleep_day_data2)

n_distinct(combined_sleep_day_data2$Id)

#Sedentary Time vs Time In Bed
sedentary.lm <- lm(SedentaryMinutes ~ TotalTimeInBed, data = combined_sleep_day_data)
sedentary.lm

#And now a pearson correlation coefficient:
cor(combined_sleep_day_data$TotalTimeInBed,combined_sleep_day_data$SedentaryMinutes, method = "pearson")

ggplot(data = combined_sleep_day_data, aes(x=VeryActiveMinutes, y=Calories)) + geom_point() + stat_smooth(method = lm)

#The lm() function is used to fit linear models to data frames in the R Language. It can be used to carry out regression, single stratum analysis of variance, and analysis of covariance to predict the value corresponding to data that is not in the data frame.
lm(Calories ~ VeryActiveMinutes, data = combined_sleep_day_data)

ggplot(data = combined_sleep_day_data, aes(x=TotalSteps, y=Calories)) + geom_point() +stat_smooth(method = lm)

lm(Calories ~ TotalSteps, data = combined_sleep_day_data)

ggplot(data = combined_sleep_day_data, aes(x=FairlyActiveMinutes, y=Calories)) + geom_point() + stat_smooth(method = lm)

lm(Calories ~ FairlyActiveMinutes, data = combined_sleep_day_data)

