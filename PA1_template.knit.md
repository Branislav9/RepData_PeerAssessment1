---
title: "Reproducible Research: Peer Assessment 1"
author: "Branislav Adamovic"
date: "July 20, 2017"
output: html_document

---

# Activity Data Analysis



This assignment makes use of data from a personal activity monitoring device.he data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

As a first step we will load the data and store it to a variable:


```r
activity<-read.csv("activity.csv")
```

then change column date from factor to date format:


```r
activity$date<-as.Date(as.character(activity$date))
```

and finally load necessary package:


```r
library(dplyr)
library(lattice)
```

### Now, we will go through given tasks:  

**1. Plot histogram of the total number of steps taken each day.**

Firstly we have to create sum of steps for each day and store it to variable called "perDay".


```r
perDay<- activity %>%
        group_by(date) %>%
        summarise(daySteps=sum(steps, na.rm=TRUE))
```

Secondly we draw the histogram.


```r
with(perDay, {
        hist(daySteps, breaks = seq(0,25000, by=1000), col = "grey", main = "Total number of steps taken each day", xlab = "Number of steps")
})
```

<img src="PA1_template_files/figure-html/plot histogram-1.png" width="672" />

**2. Calculate mean and median number of steps taken each day**

Mean


```r
mean(perDay$daySteps)
```

```
## [1] 9354.23
```

Median


```r
median(perDay$daySteps)
```

```
## [1] 10395
```

**3. Time series plot of the average number of steps taken**

Calculation of average steps taken accross all days.


```r
averageSteps <-activity %>%
        group_by(interval) %>%
        summarise(intervalMean=mean(steps, na.rm=TRUE))
```

And, plotting time series.


```r
with(averageSteps, {
        plot(interval, intervalMean, type="l", xlab="5-minute interval", ylab = "Average number of steps", col="blue", xlim = c(0, 2500))
})
```

<img src="PA1_template_files/figure-html/plot average steps-1.png" width="672" />

**4. The 5-minute interval that, on average, contains the maximum number of steps**


```r
 averageSteps[which.max(averageSteps$intervalMean),]$interval
```

```
## [1] 835
```

**5. Code to describe and show a strategy for imputing missing data**

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

Let's calculate the total number of missing values in the dataset (i.e. the total number of rows with NAs).


```r
sum(is.na(activity))
```

```
## [1] 2304
```

Now we will replace missing values by an average of the interval value (variable "mn") and create a new data set called "imputedActivity".


```r
mn<-mean(activity$steps, na.rm = TRUE) 
imputedActivity <- activity %>%
        mutate(steps=ifelse((is.na(steps)),mn,steps))
```

**6.Histogram of the total number of steps taken each day after missing values are imputed**

We create a histogram with filled missing data.


```r
perDay1<-imputedActivity %>%
        group_by(date) %>%
        summarise(daySteps=sum(steps, na.rm=TRUE))
with(perDay1, {
        hist(daySteps, breaks = seq(0,25000, by=1000), ylim=c(0,20), col = "grey", main = "Total number of steps taken each day", xlab = "Number of steps")
})
```

<img src="PA1_template_files/figure-html/plot histogram with imputed data-1.png" width="672" />

Mean


```r
mean(perDay1$daySteps)
```

```
## [1] 10766.19
```

Median


```r
median(perDay1$daySteps)
```

```
## [1] 10766.19
```

We can observe that the interval 10000 and 11000 has increased frequency which is a region also for mean and media. The value of mean and median is now same. From the previous calculation there is a higher increase of mean as we calculate with more valid data.

**7.Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends**

We will create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
weekPattern <- imputedActivity %>%
        mutate(week=as.factor(ifelse(factor(weekdays(date))=="Sunday"|factor(weekdays(date))=="Saturday","weekend","weekday"))) %>% 
        group_by(week, interval) %>%
        summarise(stepsMean=mean(steps))
```

and now we make a panel plot containing a time series plot of the 5-minute interval and the average number of steps taken, averaged across all weekday days or weekend days.


```r
xyplot(stepsMean~interval|week,
       data = weekPattern,
       layout=c(1,2),
       xlab = "Interval",
       ylab = "Number of steps",
       type="l")
```

<img src="PA1_template_files/figure-html/panel plot-1.png" width="672" />
