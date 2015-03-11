# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
##Set Working dir
setwd ("\\\\UPLCI084736/USERS$/gjf510/MyCloudDrive/Documents/Coursera/RepData_PeerAssessment1")
##Set local language to English (to use English Names)
Sys.setlocale("LC_TIME", "English")  
```

```
## [1] "English_United States.1252"
```

```r
act <- read.csv("activity.csv", header=TRUE, sep=",", skip = 0)
##Process the data
##Convert date to a Date class
act$date <- as.Date(act$date)
```


## What is mean total number of steps taken per day?

```r
##Filter data where complete cases = TRUE
compl_act <- subset(act,complete.cases(act)==TRUE)
##Group by day
compl_act_day <- split(compl_act, compl_act$date)
#What is the total number of steps taken per day?
totsteps <- sapply(compl_act_day,function(x) sum(x$steps))
##Create a Histogram of the total number of steps during each day
hist(totsteps, main="Total steps per day",xlab="Total steps", ylab="Frequency")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

```r
#What is mean total number of steps taken per day?
mean(totsteps)
```

```
## [1] 10766.19
```

```r
median(totsteps)
```

```
## [1] 10765
```


## What is the average daily activity pattern?
Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
##Filter data where complete cases = TRUE
compl_act <- subset(act,complete.cases(act)==TRUE)
##Split by interval
compl_act_int <- split(compl_act, compl_act$interval)
##Create Average
int_average <- sapply(compl_act_int,function(x) mean(x$steps))
##Create the plot
plot(int_average,type="l",ylab="Average Steps",xlab="5-Minute Interval",main="5-Minute Time Series Plot")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
##Print the interval with the max steps
names(which.max(int_average))
```

```
## [1] "835"
```

## Imputing missing values
Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


## Are there differences in activity patterns between weekdays and weekends?
