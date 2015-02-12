Peer Assessment 1
=================

This is an R Markdown document for peer assessment 1 of reproducible research.

## Loading and preprocessing the data

```r
unzip('activity.zip')
act <- read.csv('activity.csv')
str(act)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```


## What is mean total number of steps taken per day?
### 1. Total number of steps taken per day

```r
tapply(act$step, act$date, sum)
```

```
## 2012-10-01 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 
##         NA        126      11352      12116      13294      15420 
## 2012-10-07 2012-10-08 2012-10-09 2012-10-10 2012-10-11 2012-10-12 
##      11015         NA      12811       9900      10304      17382 
## 2012-10-13 2012-10-14 2012-10-15 2012-10-16 2012-10-17 2012-10-18 
##      12426      15098      10139      15084      13452      10056 
## 2012-10-19 2012-10-20 2012-10-21 2012-10-22 2012-10-23 2012-10-24 
##      11829      10395       8821      13460       8918       8355 
## 2012-10-25 2012-10-26 2012-10-27 2012-10-28 2012-10-29 2012-10-30 
##       2492       6778      10119      11458       5018       9819 
## 2012-10-31 2012-11-01 2012-11-02 2012-11-03 2012-11-04 2012-11-05 
##      15414         NA      10600      10571         NA      10439 
## 2012-11-06 2012-11-07 2012-11-08 2012-11-09 2012-11-10 2012-11-11 
##       8334      12883       3219         NA         NA      12608 
## 2012-11-12 2012-11-13 2012-11-14 2012-11-15 2012-11-16 2012-11-17 
##      10765       7336         NA         41       5441      14339 
## 2012-11-18 2012-11-19 2012-11-20 2012-11-21 2012-11-22 2012-11-23 
##      15110       8841       4472      12787      20427      21194 
## 2012-11-24 2012-11-25 2012-11-26 2012-11-27 2012-11-28 2012-11-29 
##      14478      11834      11162      13646      10183       7047 
## 2012-11-30 
##         NA
```

### 2. Make a histogram of the total number of steps taken each day
- A histogram try to use the a series of ranges in the steps per day and represent the distribution of steps in a specific range in a statistical manner (frequency, density or others).
- A barplot simply summarizes the steps on each day for comparison.


```r
step <- tapply(act$step, act$date, sum)
op <- par(mfrow = c(2, 1))
hist(step, col = 'gray', xlab = 'steps')
barplot(step, main = "Steps taken per day by barplot", xlab = 'date', ylab = 'steps')
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

```r
par(op)
```

### 3. Mean and mediean of the total number of steps per day

```r
mean(step, na.rm = T) ## For mean value
```

```
## [1] 10766.19
```

```r
median(step, na.rm = T) ## For median value
```

```
## [1] 10765
```


## What is the average daily activity pattern?
### 1. Make a time series plot

```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
## 
## The following object is masked from 'package:stats':
## 
##     filter
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
act <- tbl_df(act)
a_stp <- act %>%
    group_by(interval) %>%
    summarize(mean = mean(steps, na.rm = T))
plot(a_stp$interval, a_stp$mean, type = 'l', xlab = 'interval', ylab = 'average steps', main = 'average steps of 5 min interval')
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 

### 2. Maximum number of steps in 5 min interval

```r
a_stp[which.max(a_stp$mean),]
```

```
## Source: local data frame [1 x 2]
## 
##   interval     mean
## 1      835 206.1698
```

## Imputing missing values
### 1. Total number of rows with NAs

```r
sum(is.na(act$steps))
```

```
## [1] 2304
```

### 2. Filling gaps

```r
n <- length(levels(act$date))
na_idx <- is.na(act$steps); mean_5 <- rep(a_stp$mean, n)
mean_5[!na_idx] <- 0; act$steps[na_idx] <- 0
act$steps <- act$steps + mean_5
head(act)
```

```
## Source: local data frame [6 x 3]
## 
##       steps       date interval
## 1 1.7169811 2012-10-01        0
## 2 0.3396226 2012-10-01        5
## 3 0.1320755 2012-10-01       10
## 4 0.1509434 2012-10-01       15
## 5 0.0754717 2012-10-01       20
## 6 2.0943396 2012-10-01       25
```

### 3. New dataset

```r
act_new <- act
```

### 4. New histogram, mean and median
Imputing missing data does not change the mean of total steps, but reduces the median value. They become the same value in the end.

```r
t_step <- act_new %>%
    group_by(date) %>%
    summarize(sum = sum(steps))
hist(t_step$sum, col = 'blue', xlab = 'steps', main = 'Histogram of total steps')
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png) 

```r
mean(t_step$sum)
```

```
## [1] 10766.19
```

```r
median(t_step$sum)
```

```
## [1] 10766.19
```

## Are there differences in activity patterns between weekdays and weekends?
### 1. New factor variable (weekday)

```r
act_new <- mutate(act_new, week = weekdays(as.Date(act_new$date)))
act_new$week <- as.factor(ifelse(act_new$week %in% c('Saturday', 'Sunday'), 'weekend', 'weekday'))
head(act_new)
```

```
## Source: local data frame [6 x 4]
## 
##       steps       date interval    week
## 1 1.7169811 2012-10-01        0 weekday
## 2 0.3396226 2012-10-01        5 weekday
## 3 0.1320755 2012-10-01       10 weekday
## 4 0.1509434 2012-10-01       15 weekday
## 5 0.0754717 2012-10-01       20 weekday
## 6 2.0943396 2012-10-01       25 weekday
```

### 2. Make a panel plot (time series grouped by weekday and weekend)

```r
library(ggplot2)
step_w <- act_new %>%
    group_by(interval, week) %>%
    summarize(mean = mean(steps))

ggplot(step_w, aes(x = interval, y = mean)) + geom_line(aes(color = week)) + facet_wrap(~week, ncol = 1) + labs(x = 'interval', y = 'Number of steps') + ggtitle('Weekday vs. Weekend') + theme_linedraw() + theme(legend.position = 'none');
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12-1.png) 



