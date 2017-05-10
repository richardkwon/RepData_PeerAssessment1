# Reproducible Research: Peer Assessment 1
Omyung Richard Kwon  



## Loading and preprocessing the data
1. Load the data  
2. Process/transform the data (if necessary) into a format suitable for your analysis
  

```r
setwd("C:\\Users\\kwonr\\Projects\\ReproducibleResearch\\RepData_PeerAssessment1")
activity<-read.csv("activity.csv", header=TRUE)
activity$date <- as.Date(activity$date, "%Y-%m-%d")
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```


## What is mean total number of steps taken per day?

1. Calculate the total number of steps taken per day
2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day
3. Calculate and report the mean and median of the total number of steps taken per day
  

```r
library(ggplot2)
sumbydate <- aggregate(x=activity$steps, by=list(activity$date), FUN=sum, na.rm=TRUE)
names(sumbydate) <- c("date", "totalsteps")

q <- ggplot (sumbydate, aes(x=totalsteps)) + geom_histogram(binwidth=500)
q <- q + labs(title="Daily Steps", x="Total Steps", y = "Frequency")
print(q)
```

![](PA1_template_files/figure-html/unnamed-chunk-1-1.png)<!-- -->

```r
mean(sumbydate$totalsteps)
```

```
## [1] 9354.23
```

```r
median(sumbydate$totalsteps)
```

```
## [1] 10395
```
  


## What is the average daily activity pattern?


1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?



```r
intv.avg <- aggregate(activity$steps, by=list(activity$interval), FUN=mean, na.rm=TRUE)
names(intv.avg) <- c("interval", "avgsteps")

q <- ggplot(intv.avg, aes(x=interval, y=avgsteps))
q <- q + geom_line(color="red")
q <- q + labs(title="Average Daily Steps by Interval", x="Intervals", y="Avg. Steps")
print(q)
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
intv.avg[intv.avg$avgsteps == max(intv.avg$avgsteps), ]
```

```
##     interval avgsteps
## 104      835 206.1698
```



## Imputing missing values



Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
  

```r
sum(is.na(activity$steps))
```

```
## [1] 2304
```

```r
meanbyinterval <- aggregate(activity$steps, by=list(activity$interval), FUN=mean, na.r=TRUE)
names(meanbyinterval) <- c("interval", "avgsteps")

numrows <- NROW(activity)

activityCopy <- activity
for (i in 1:numrows) {
    if (is.na(activity$steps[i])) {
        index <- match(activity$interval[i], meanbyinterval$interval)
        activityCopy$steps[i] <- meanbyinterval$avgsteps[index]
    }
}

sumbydate2 <- aggregate(x=activityCopy$steps, by=list(activityCopy$date), FUN=sum, na.rm=TRUE)
names(sumbydate2) <- c("date", "totalsteps")

r <- ggplot (sumbydate2, aes(x=totalsteps)) + geom_histogram(binwidth=500)
r <- r + labs(title="Daily Steps", x="Total Steps", y = "Frequency")
print(r)
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
mean(sumbydate2$totalsteps)
```

```
## [1] 10766.19
```

```r
median(sumbydate2$totalsteps)
```

```
## [1] 10766.19
```
  
Both mean and median values increased but, overall, the histogram plot looks very similar in distribution.  There is a significant reduction in zero value from graph 1 to 2.



## Are there differences in activity patterns between weekdays and weekends?


For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
numrows <- NROW(activityCopy)

weekend <- c("Saturday","Sunday")
for (i in 1:numrows) {
    d <- weekdays(activityCopy$date[i])
    if (d %in% weekend) {
        activityCopy$daytype[i] <- "Weekend"
    } else {
        activityCopy$daytype[i] <- "Weekday"
    }
}
activityCopy$daytype <- as.factor(activityCopy$daytype)

s <- ggplot(activityCopy, aes(x=interval, y=steps))
s <- s + geom_line(color="blue")
s <- s + facet_grid(daytype~.)
s <- s + labs(title="Average Daily Steps by Interval", x="Intervals", y="Avg. Steps")
print(s)
```

![](PA1_template_files/figure-html/weekday-1.png)<!-- -->
