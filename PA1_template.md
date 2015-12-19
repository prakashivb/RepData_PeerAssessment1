---
title: "Reproducible Research - Peer Assessment 1 Report"
author: "Prakash Inuganti"
date: "December 19, 2015"
output: html_document
---




```r
activityData = read.table("activity.csv", header = TRUE, sep = ",",na.strings = "NA")

dataframe = as.data.frame(activityData)
totalSteps <- aggregate(steps ~ date, data = dataframe, sum, na.rm = TRUE)
head(totalSteps)
```

```
##         date steps
## 1 2012-10-02   126
## 2 2012-10-03 11352
## 3 2012-10-04 12116
## 4 2012-10-05 13294
## 5 2012-10-06 15420
## 6 2012-10-07 11015
```

What is mean total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in the dataset.

Calculate the total number of steps taken per day

If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day





```r
hist(totalSteps$steps, main = "Steps taken per day", xlab = "Day", col = "Blue")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png) 

Calculate and report the mean and median of the total number of steps taken per day


```r
mean(totalSteps$steps, na.rm = TRUE)
```

```
## [1] 10766.19
```

```r
median(totalSteps$steps, na.rm = TRUE)
```

```
## [1] 10765
```
What is the average daily activity pattern?

Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)



```r
dailySteps <- aggregate(dataframe$steps ~ dataframe$interval, FUN=mean, na.rm = TRUE)
names(dailySteps) <- c("interval", "mean")
head(dailySteps)
```

```
##   interval      mean
## 1        0 1.7169811
## 2        5 0.3396226
## 3       10 0.1320755
## 4       15 0.1509434
## 5       20 0.0754717
## 6       25 2.0943396
```

```r
plot(dailySteps$interval, dailySteps$mean, type = "l", col = "blue", lwd=2, xlab = "Interval in minutes", ylab = "Average number of steps", main = "Average daily activity pattern")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 
Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
maxMean <- max(dailySteps$mean, na.rm = TRUE)
maxInterval <- dailySteps[dailySteps$mean == maxMean,]
maxInterval
```

```
##     interval     mean
## 104      835 206.1698
```
Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
totalNas <- sum(is.na(dataframe$steps))
totalNas
```

```
## [1] 2304
```
Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
dataframe[is.na(dataframe)] <- mean(dataframe$steps, na.rm = TRUE)
head(dataframe)
```

```
##     steps       date interval
## 1 37.3826 2012-10-01        0
## 2 37.3826 2012-10-01        5
## 3 37.3826 2012-10-01       10
## 4 37.3826 2012-10-01       15
## 5 37.3826 2012-10-01       20
## 6 37.3826 2012-10-01       25
```


Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
newTotalSteps <- aggregate(steps ~ date, data = dataframe, sum, na.rm = TRUE)
head(newTotalSteps)
```

```
##         date    steps
## 1 2012-10-01 10766.19
## 2 2012-10-02   126.00
## 3 2012-10-03 11352.00
## 4 2012-10-04 12116.00
## 5 2012-10-05 13294.00
## 6 2012-10-06 15420.00
```

```r
hist(newTotalSteps$steps, main = "Steps taken per day", xlab = "Day", col = "Blue")
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png) 


```r
mean(newTotalSteps$steps, na.rm = TRUE)
```

```
## [1] 10766.19
```

```r
median(newTotalSteps$steps, na.rm = TRUE)
```

```
## [1] 10766.19
```

Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
dataframe$date <- as.POSIXct(dataframe$date, format="%Y-%m-%d")

#day <- weekdays(dataframe$date)
dataframe$whichDay <- ifelse(weekdays(dataframe$date) == "Saturday" | weekdays(dataframe$date) == "Sunday", "weekend", "weekday")
head(dataframe)
```

```
##     steps       date interval whichDay
## 1 37.3826 2012-10-01        0  weekday
## 2 37.3826 2012-10-01        5  weekday
## 3 37.3826 2012-10-01       10  weekday
## 4 37.3826 2012-10-01       15  weekday
## 5 37.3826 2012-10-01       20  weekday
## 6 37.3826 2012-10-01       25  weekday
```

Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```r
library(lattice)
stepsByDay <- aggregate(steps ~ interval + whichDay, data = dataframe, mean)
head(stepsByDay)
```

```
##   interval whichDay    steps
## 1        0  weekday 7.006569
## 2        5  weekday 5.384347
## 3       10  weekday 5.139902
## 4       15  weekday 5.162124
## 5       20  weekday 5.073235
## 6       25  weekday 6.295458
```

```r
names(stepsByDay) <- c("interval", "whichDay", "steps")

xyplot(steps ~ interval | whichDay, stepsByDay, type = "l", layout = c(1,2), xlab = "Interval", ylab = "Number of steps")
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png) 
