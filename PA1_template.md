---
title: "Reproducible Research Week2 Course Project 1"
output: html_document
---
set always "Echo" TRUE in this markdown.


Before analysis, install packages if it is nessasary.

```r
library(tidyverse)
```

Read the "activity.csv" without arguments.

```r
activity<-read.csv("/Users/yuki/Documents/R/ReproducibleResearch/Week2_C1/activity.csv",stringsAsFactors = F)
```
What is mean total number of steps taken per day?
1.Calculate the total number of steps taken per day
Omit the NA value in the dataset.

```r
Stepsbyday<-
activity %>%
    group_by(date)%>%
    summarise(sum(steps,na.rm=TRUE))
names(Stepsbyday)<-c("date","sumSteps")
```

2.Make a histogram of the total number of steps taken each day

```r
hist(Stepsbyday$sumSteps,main="a histogram of the total number of steps taken each day",breaks = 100)
```

![plot of chunk unnamed-chunk-68](figure/unnamed-chunk-68-1.png)

3.Calculate and report the mean and median of the total number of steps taken per day

```
## [1] 9354.23
```

```
## [1] 10395
```

What is the average daily activity pattern?
1.Make a time series plot (i.e. type = "l" ) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
AverageSteps<-
activity %>%
    group_by(interval) %>%
    summarise(mean(steps,na.rm=TRUE))

names(AverageSteps)<-c("interval","Ave")

plot(AverageSteps$interval,AverageSteps$Ave,type="l",main="the average daily activity pattern")
```

![plot of chunk unnamed-chunk-70](figure/unnamed-chunk-70-1.png)

2.Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
AverageSteps[which.max(AverageSteps$Ave),]
```

```
## # A tibble: 1 x 2
##   interval   Ave
##      <int> <dbl>
## 1      835  206.
```

Imputing missing values

1.Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with 
NA)


```
## 
## FALSE  TRUE 
## 15264  2304
```

2.Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
3.Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
activity_fill<-activity
activity_fill$steps<-ifelse(is.na(activity_fill$steps)==TRUE,AverageSteps$Ave[AverageSteps$interval],activity_fill$steps)
```

4.Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
Stepsbyday_fill<-
activity_fill %>%
    group_by(date)%>%
    summarise(sum(steps,na.rm=TRUE))
names(Stepsbyday_fill)<-c("date","sumSteps")

par(mfrow=c(1,2))
hist(Stepsbyday$sumSteps,main="the total number of steps taken each day with NA",breaks = 100)
hist(Stepsbyday_fill$sumSteps,main="a histogram of the total number of steps taken each day filled NA",breaks = 100)
```

![plot of chunk unnamed-chunk-74](figure/unnamed-chunk-74-1.png)

```r
#mean
mean(Stepsbyday_fill$sumSteps)
```

```
## [1] 9545.514
```

```r
#median
median(Stepsbyday_fill$sumSteps)
```

```
## [1] 10395
```

Are there differences in activity patterns between weekdays and weekends?
1.Create a new factor variable "weekday" in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
activity_fill$date<-as.Date(activity_fill$date)
mutate(activity_fill,day=weekdays(date) %in% c("土曜日","日曜日"))->mutated_activity
mutate(mutated_activity,weekdays=as.factor(ifelse(day=="TRUE","weekends","weekday")))->activity2
```

2.Make a panel plot containing a time series plot (i.e. type = “l”) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


```r
AveStepsbyWeek<-
activity2 %>%
    group_by(weekdays,interval)%>%
    summarise(AveStep=mean(steps,na.rm=TRUE))

ggplot(data=AveStepsbyWeek,aes(color=weekdays,x=interval,y=AveStep))+geom_line()+facet_grid(weekdays~.)
```

![plot of chunk unnamed-chunk-76](figure/unnamed-chunk-76-1.png)





