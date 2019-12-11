Reproducible Research. Project 1
=========================================


# Introduction
It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the “quantified self” movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

The data for this assignment can be downloaded from the course web site:  
- Dataset [Activity Monitoring data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip)  
The variables included in this dataset are:  
**steps**: Number of steps taking in a 5-minute interval (missing values are coded as NA)  
**date**: The date on which the measurement was taken in YYYY-MM-DD format  
**interval**: Identifier for the 5-minute interval in which measurement was taken  

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.  

## Loading and preprocessing the data
Extract the downloaded file into the working directory and load it

```r
setwd("C:/Users/S.kh/Desktop/Coursera/Reproducible project 1")
data <- read.csv("activity.csv")
```
## What is mean total number of steps taken per day?
1. Calculate the total number of steps taken per day  

```r
totalsteps <- with(data, tapply(steps, date, sum, na.rm = TRUE))
```
2. Make a histogram of the total number of steps taken each day

```r
hist(totalsteps, xlab = "Steps", main = "Total number of steps per day", col = "green")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png)

3. Calculate and report the mean and median of the total number of steps taken per day

```r
Meansteps <- mean(totalsteps, na.rm = T)
Mediansteps <- median(totalsteps, na.rm = T)
```
Mean = 9354.23 and Median = 10395  

## What is the average daily activity pattern?
1. Make a time series plot (i.e. type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
avgsteps <- aggregate(steps ~ interval , data, mean, na.rm = T)
plot(x= avgsteps$interval, y= avgsteps$steps, type = "l", main = "Average daily activity", xlab = "5-min Interval", ylab = "Average number of steps")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png)

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
avgsteps[which.max(avgsteps$steps),]
```

```
##     interval    steps
## 104      835 206.1698
```

## Imputing missing values
Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.  
1. Calculate and report the total number of missing values in the dataset  

```r
sum(is.na(data))
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.  

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.  

```r
## Filling in the missing values (NA) with the mean for the 5-min interval
datanew <- data
datanew[is.na(datanew)] <- avgsteps$steps
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?  

```r
totalstepsnew <- with(datanew, tapply(steps, date, sum))
hist(totalstepsnew, main = "Total number of steps taken each day", xlab = "Steps", col = "blue")
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-1.png)

```r
Meansteps_new <- mean(totalstepsnew)
Mediansteps_new <- median(totalstepsnew)
options(scipen = 999)
```
new Mean = 10766.19 and new Median = 10766.19

Mean and Median of the steps changed after imuting the missing values. mean and median both increased after imutation.
  
## Are there differences in activity patterns between weekdays and weekends?  
1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.  

```r
## using weekdays function
datanew$weekdays <- weekdays(as.Date(datanew$date))
datanew$daytype <- ifelse(datanew$weekdays %in% c("Saturday","Sunday"), "Weekend", "Weekday")
datanew$daytype <- as.factor(datanew$daytype)
```
2. Make a panel plot containing a time series plot (i.e. type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.  

```r
library(ggplot2)
avgstepsnew <- aggregate(steps ~ interval + daytype , datanew, mean)
ggplot(avgstepsnew, aes(x= interval, y= steps, color = daytype )) + geom_line()+ facet_wrap(daytype~., nrow = 2) + labs(title = "AVerage activity patterns Weekdays vs Weekends", x = "Interval", y= "Number of steps")  
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png)
