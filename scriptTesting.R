install.packages("dplyr")
install.packages("lubridate")
install.packages("lattice")

library(dplyr)
library(lubridate)
library(lattice)

setwd("C:/Users/Jimmy/Documents/GitHub/RepData_PeerAssessment1")
unzip("./activity.zip", exdir = "./DataSets")

fileName <- "./DataSets/activity.csv"

df <- read.csv(fileName)

df$date <- as.Date(df$date, format = "%Y-%m-%d")
df$DayPart <- day(df$date)
df$DayText <- weekdays(df$date)

df <- mutate(df, weekpart = ifelse(df$DayText %in% c("Saturday", "Sunday") , "Weekend", "Weekday"))

meanPerDay <-
df %>%
  select(date, steps) %>%
  group_by(date) %>%
  summarise_all(funs(mean(., na.rm=TRUE)))

medianPerDay <-
  df %>%
  select(date, steps) %>%
  group_by(date) %>%
  summarise_all(funs(median(., na.rm=TRUE)))

sumPerDay <-
  df %>%
  select(date, steps) %>%
  group_by(date) %>%
  summarise_all(funs(sum(., na.rm=TRUE)))

histogram(~steps|DayText, data=na.omit(df), main="Distribution of steps by day of week")

mean(na.omit(sumPerDay$steps))
median(na.omit(sumPerDay$steps))

#What is the average daily activity pattern?

meanPerInterval <-
  df %>%
  select(interval, steps) %>%
  group_by(interval) %>%
  summarise_all(funs(mean(., na.rm=TRUE)))

plot(x=meanPerInterval$interval, y=meanPerInterval$steps, type = "l")

#Interval with maximum average steps
subset(meanPerInterval, meanPerInterval$steps==max(meanPerInterval$steps))



#Number of missing valuese
sum(is.na(df$steps))

#Use the mean for the interval to fill in missing values

dfMissing <- subset(df, is.na(steps))
dfMissing$steps <- round(meanPerInterval[match(dfMissing$interval, meanPerInterval$interval),2])$steps

df2 <- rbind(dfMissing, subset(df, !is.na(steps)))

meanPerDay2 <-
  df2 %>%
  select(date, steps) %>%
  group_by(date) %>%
  summarise_all(funs(mean(., na.rm=TRUE)))

medianPerDay2 <-
  df2 %>%
  select(date, steps) %>%
  group_by(date) %>%
  summarise_all(funs(median(., na.rm=TRUE)))

sumPerDay2 <-
  df2 %>%
  select(date, steps) %>%
  group_by(date) %>%
  summarise_all(funs(sum(., na.rm=TRUE)))

histogram(~steps|DayText, data=na.omit(df2), main="Distribution of steps by day of week")


mean(na.omit(sumPerDay$steps)) - mean(na.omit(sumPerDay2$steps))
median(na.omit(sumPerDay$steps)) - median(na.omit(sumPerDay2$steps))

#time series plot


meanPerIntervalWeekPart <-
  df2 %>%
  select(interval, weekpart, steps) %>%
  group_by(interval, weekpart) %>%
  summarise_all(funs(mean(., na.rm=TRUE)))

summary(meanPerIntervalWeekPart)

xyplot(steps ~ interval|factor(weekpart), data=meanPerIntervalWeekPart, type="l")

