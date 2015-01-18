---
title: "Reproducible Research Peer Assessment 1"
author: "Rodney Joujoute"
date: "January 16, 2015"
output: html_document
---
Peer Assessment 1

knitr::opts_chunk$set(echo=TRUE)
setwd("/Users/rodneyjoujoute")

unzip("repdata-data-activity.zip")
activity<-read.csv("activity.csv", stringsAsFactors = FALSE)
str(activity)

#change the date which is a string variable now, to a date type
activity$date<-as.Date(activity$date, format = '%Y-%m-%d')
head(activity)

# aggrigating by date
total_daily_steps <- aggregate(steps~date, activity,sum, na.rm=TRUE)
# 1. Make a histogram of the total number of steps taken each day
histogram <-barplot(total_daily_steps$steps, names.arg =total_daily_steps$date, xlab = "Date",ylab="Total Daily Steps", main="Number of Steps per Day")

# 2. Calculate and report the mean and median total number of steps taken per day
daily_mean_steps<- mean(total_daily_steps$steps,na.rm=TRUE)
daily_median_steps<-median(total_daily_steps$steps,na.rm=TRUE)

#1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
average_interval <- aggregate(steps ~ interval, activity, mean)

plot(average_interval, type = "l", xlab="Intervals", ylab="Average Steps per interval", main="Average steps per interval")

#2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
average_interval$interval[which.max(average_interval$steps)]

#1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
sum(!complete.cases(activity))

#2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
    #I prefer to use mean to fill the missing value

#3.Create a new dataset that is equal to the original dataset but with the missing data filled in.
activity <- merge(activity,average_interval, by = "interval", suffixes = c("", 
    ".y"))
nas <- is.na(activity$steps)
activity$steps[nas] <- activity$steps.y[nas]
activity <- activity[, c(1:3)]

#4. Make a histogram of the total number of steps taken each day.
total_steps_per_day <- aggregate(steps ~ date,activity,sum)
barplot(total_steps_per_day$steps, names.arg = total_steps_per_day$date, xlab = "Date", ylab = "Total number of Steps",main="Total Steps per Day")

#Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
#Mean
mean(total_steps_per_day$steps)

#Median
median(total_steps_per_day$steps)

#1. Create a new factor variable in the dataset with two levels weekday and weekend indicating whether a given date is a weekdat or weekend day. 
activity$dayType <- ifelse(weekdays(activity$date) %in%  c("Saturday", "Sunday"),'weekend','weekday')

head(activity)

table(activity$dayType)

#2. Make a panel plot containng a time series plot (i.e type "i") of the 5-minute interval (x-axis) and the average number of steps taken, averaged aross all weekday dats or weekend days (y-axis)
library(ggplot2)
qplot(x=interval, y=steps,data=subset(activity, complete.cases(activity)),geom='smooth', stat='summary', fun.y=mean) + facet_grid(dayType~.) + facet_wrap(~dayType,nrow=2) + theme(strip.background = element_rect(fill="#ffe5cc")) + labs(title=' Average steps per days, analyzing weekdays and weekend patterns')

