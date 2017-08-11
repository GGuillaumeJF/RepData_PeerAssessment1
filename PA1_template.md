# Reproducible Research : Peer Assessment 1


<style>
body {
text-align: justify}
</style>

## Introduction

In this assignment, a dataset from a personal activity monitoring device is explored. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

## Loading and preprocessing the data

The first step is to load data by the following code : 

```r
activity <- read.csv("activity.csv")
```
The first six columns are shown in order to have a look of the dataset.

```r
head(activity)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```
There are a total of 17568 observations in this dataset. The variables included in this dataset are:

* **steps** : Number of steps taking in a 5-minute interval (missing values are coded as NA)
* **date**: The date on which the measurement was taken in YYYY-MM-DD format
* **interval**: Identifier for the 5-minute interval in which measurement was taken

Finally, in order to make the desired analysis, some preprocessing is performed. 

```r
# The package dplyr is loaded
library(dplyr)
# The data frame "activity" is grouped by date
activity <- group_by(activity,date)
# In order to have English weekdays
Sys.setlocale("LC_TIME","English_Canada.1252")
```

```
## [1] "English_Canada.1252"
```

## What is mean total number of steps taken per day?

In this section, the total number of steps taken per day is investigated. At first, the amounts of steps are computed.

```r
daily_steps <- summarise(activity,Day_steps = sum(steps))
```
Then a histogram is presented giving the total number of steps taken each day.

```r
#pdf(file = "Histogram1.pdf",width = 12)
hist(daily_steps$Day_steps,breaks = 10,xlab="Daily Steps", main = 'Histogram of Daily Steps')
```

![](PA1_template_files/figure-html/histogram daily steps-1.png)<!-- -->

```r
#dev.off()
```

Finally, the mean and the median of the total number of steps taken per day are computed. The code is: 

```r
mean_daily_steps <- round(mean(daily_steps$Day_steps,na.rm = TRUE))
median_daily_steps <- median(daily_steps$Day_steps,na.rm = TRUE)
```
Hence, the total number of steps taken per day during two months from the monitoring device has a mean value of 1.0766\times 10^{4} and a median value of 10765.  

## What is the average daily activity pattern?

The average daily activity is studied by computing the mean of the number of steps for each 5-minute interval identifier across all days. The code used is:

```r
activity <- group_by(activity,interval)
mean_steps_interval <- summarise(activity,steps_mean = mean(steps,na.rm = TRUE))
```

Then, a time series plot is generated:

```r
#pdf(file = "Plot1.pdf",width = 12)
plot(mean_steps_interval$interval,mean_steps_interval$steps_mean,type='l',xlab='5-minute interval identifier',ylab='average number of steps', main = 'Average number of steps at each 5-minutes interval during 2 months')
```

![](PA1_template_files/figure-html/plot interval-1.png)<!-- -->

```r
#dev.off()
```

In order to answer the following question "Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?", the following code is performed:

```r
max_nbr_steps_index <- which.max(mean_steps_interval$steps_mean)
max_steps <- mean_steps_interval$steps_mean[max_nbr_steps_index]
interval_max_steps <- mean_steps_interval$interval[max_nbr_steps_index]
```

Consequently, the maximum number of steps has a mean of 206.1698113 and the 5-minute interval has the identifier 835.

## Imputing missing values

There are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data. In order to calculate the total number of missing values in the dataset, the following code is used:

```r
Na_in_activity <- is.na(activity$steps)
nbre_NA <- sum(Na_in_activity)
```
Thus, we have 2304 missing values among 17568 observations. 

In the following, a strategy is used to fill all the missing values in the dataset. The chosen strategy is to fill the missing value at a 5-minute interval by the mean value of the number of steps at this 5-minute interval. The following code does the job:

```r
activity_rmNA <- activity 
activity_rmNA$steps[Na_in_activity] <- rep(round(mean_steps_interval$steps_mean),61)[Na_in_activity]
```
Again a histogram of the total number of steps taken each day is made. 

```r
activity_rmNA <- group_by(activity_rmNA,date)
daily_steps_noNA <- summarise(activity_rmNA,Day_steps = sum(steps))
#pdf(file = "Histogram2.pdf",width = 12)
hist(daily_steps_noNA$Day_steps,breaks = 10,xlab="Daily Steps", main = 'Histogram of Daily Steps (with filled-in missing values)')
```

![](PA1_template_files/figure-html/histogram daily steps without NA-1.png)<!-- -->

```r
#dev.off()
```

Finally, the mean and the median of the total number of steps taken per day are computed. The code is: 

```r
mean_daily_steps_noNA <- round(mean(daily_steps_noNA$Day_steps,na.rm = TRUE))
median_daily_steps_noNA <- median(daily_steps_noNA$Day_steps,na.rm = TRUE)
```
Hence, the total number of steps taken per day during two months from the monitoring device and with no 'NA' has a mean value of 1.0766\times 10^{4} and a median value of 1.0762\times 10^{4}.

We can see the histogram, the mean value and the median are similar with the original values. It can be explained by the fact the methodology used to replace the missing value. Indeed, the mean value for the 5-minute interval allows to keep the trends. 

## Are there differences in activity patterns between weekdays and weekends?

In order to determine potential differences in the number of steps between weekdays and weekends, a comparison has been made. The following code create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day. The dataset with the filled-in missing values is used for this part.

```r
activity_rmNA$type_of_days <- factor(weekdays(as.Date(activity_rmNA$date)) %in% c("Saturday","Sunday"),labels = c('weekdays','weekends'))
activity_rmNA <- group_by(activity_rmNA,type_of_days,interval)
mean_steps <- summarise(activity_rmNA,steps = mean(steps))
```

Then, a time series plot is generated:

```r
#pdf(file = "Plot2.pdf",width = 12)
plot(mean_steps$interval[1:288],mean_steps$steps[1:288],type='l',xlab='5-minute interval identifier',ylab='average number of steps', main = 'Average number of steps at each 5-minutes interval during 2 months (no NA)')
points(mean_steps$interval[299:576],mean_steps$steps[299:576],type="l",col = "red")
legend("topright", legend=c('weekdays','weekends'), lty=1, col=c("black","red"))
```

![](PA1_template_files/figure-html/plot interval Weekends-1.png)<!-- -->

```r
#dev.off()
```

The plot shows a difference between weekends and weekdays. For instance, there are more steps in the morning during weekdays mentioning that the person woke up earlier than in the weekends. 
