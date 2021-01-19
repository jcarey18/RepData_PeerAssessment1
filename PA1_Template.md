  
---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

## Loading and Processing Data

Unzip and load CSV file 


```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(ggplot2)

unzip(zipfile="./data/activity.zip")
activity <- read.csv("activity.csv")
```

View summary and structure of the data to see if transformations are needed


```r
summary(activity)
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##  NA's   :2304     (Other)   :15840
```

```r
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

Convert date from a factor to date


```r
activity$date <- as.Date(activity$date)
```


## What is mean total number of steps taken per day?

Calculate steps per day


```r
steps.day <- aggregate(steps ~ date, data = activity, FUN = sum, na.rm = TRUE)
head(steps.day)
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

Create histogram 


```r
ggplot(steps.day, aes(x = steps)) +
    geom_histogram(fill = "green4", binwidth = 1000) +
    labs(title = "Daily Steps", x = "Steps", y = "Frequency")
```

![](PA1_Template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

Calculate mean and median total number of steps taken per day


```r
(mean <- mean(steps.day$step, na.rm = TRUE))
```

```
## [1] 10766.19
```

```r
(median <- median(steps.day$step, na.rm = TRUE))
```

```
## [1] 10765
```

mean steps taken per day: **1.0766189\times 10^{4}**, median: **10765**

## What is the average daily activity pattern?

calculate average daily activity


```r
steps.mean <- aggregate(steps ~ interval, data = activity, FUN = "mean", na.rm = TRUE)
```

create time series plot


```r
ggplot(steps.mean, aes(x=interval, y=steps)) +
  geom_line(color="grey57", size=1) + 
  labs(title = "Average Daily Steps", y = "Avg Steps per Day", x = "Interval")
```

![](PA1_Template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

Calculate the interval with the max number of steps


```r
(max.steps <- max(steps.mean$steps))
```

```
## [1] 206.1698
```

```r
(max.interval <- steps.mean$interval[which(steps.mean$steps == max.steps)])
```

```
## [1] 835
```

The max number of steps was **206.1698113**, which was interval **835**

## Imputing missing values

Calculate the total number of missing values

```r
(missing <- sum(is.na(activity)))
```

```
## [1] 2304
```

Number of missing values: **2304**

fill in missing values with interval mean across days and create new data set 


```r
fill.data <- activity %>%
  mutate(
    steps = case_when(
      is.na(steps) ~ steps.mean$steps[match(activity$interval, steps.mean$interval)],      
      TRUE ~ as.numeric(steps)
    ))
```

Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day


```r
steps.day2 <- aggregate(steps ~ date, data = fill.data, FUN = sum, na.rm = TRUE)

ggplot(steps.day2, aes(x = steps)) +
    geom_histogram(fill = "green4", binwidth = 1000) +
    labs(title = "Daily Steps", x = "Steps", y = "Frequency")
```

![](PA1_Template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

Calculate mean and median


```r
(mean <- mean(steps.day2$step, na.rm = TRUE))
```

```
## [1] 10766.19
```

```r
(median <- median(steps.day2$step, na.rm = TRUE))
```

```
## [1] 10766.19
```

mean: **1.0766189\times 10^{4}**, median: **1.0766189\times 10^{4}**

Values are very similar, the mean in the same but median is slightly higher when NAs are filled in with interval mean 

## Are there differences in activity patterns between weekdays and weekends?

Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
steps.days <- fill.data %>%
  mutate(weekday = ifelse(weekdays(date) %in% c("Saturday", "Sunday"), "Weekend", "Weekday" ))
```

Create a factor variable


```r
steps.days$weekday <- as.factor(steps.days$weekday)
str(steps.days)
```

```
## 'data.frame':	17568 obs. of  4 variables:
##  $ steps   : num  1.717 0.3396 0.1321 0.1509 0.0755 ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
##  $ weekday : Factor w/ 2 levels "Weekday","Weekend": 1 1 1 1 1 1 1 1 1 1 ...
```

Create a time series plot, first grouping by weekday


```r
interval.weekday <- aggregate(steps ~ interval + weekday, data = steps.days, FUN = "mean", na.rm = TRUE)
head(interval.weekday)
```

```
##   interval weekday      steps
## 1        0 Weekday 2.25115304
## 2        5 Weekday 0.44528302
## 3       10 Weekday 0.17316562
## 4       15 Weekday 0.19790356
## 5       20 Weekday 0.09895178
## 6       25 Weekday 1.59035639
```

Create time series plot 


```r
ggplot(interval.weekday, aes(x = interval, y = steps, col = weekday)) + 
  geom_line() +
  facet_grid(rows = interval.weekday$weekday) + 
  labs(x = "Interval", 
       y = "Number of Steps", 
       title = "Average steps by time of day")
```

![](PA1_Template_files/figure-html/unnamed-chunk-17-1.png)<!-- -->



