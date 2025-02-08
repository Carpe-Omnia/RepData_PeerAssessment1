---
title: "Reproducible Research: Peer Assessment 1. Tomaz Rodrigues"
output: 
  html_document:
    keep_md: true
---

## Loading and preprocessing the data


## What is mean total number of steps taken per day?

``` r
  myDays <- unique(myData$date)
  myStepsPerDay <- data.frame(date=character(), steps=numeric())
  
  for(i in myDays){
    dataOnMyDay <- subset(myData, date == i)
    totalStepsOnMyDay <- sum(dataOnMyDay$steps, na.rm=TRUE)
    mySteps <- data.frame(date=i, steps=totalStepsOnMyDay)
    myStepsPerDay <- rbind(myStepsPerDay, mySteps)
  }
  hist(myStepsPerDay$steps, breaks=length(myDays))
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

``` r
  meanSteps <- mean(myStepsPerDay$steps)
  medianSteps <- median(myStepsPerDay$steps)
  print(paste("mean steps are ", meanSteps, sep=""))
```

```
## [1] "mean steps are 9354.22950819672"
```

``` r
  print(paste("median steps are ", medianSteps, sep=""))
```

```
## [1] "median steps are 10395"
```

## What is the average daily activity pattern?

``` r
  myTimes <- unique(myData$interval)
  myStepsPerInterval <- data.frame(interval=times(), steps=numeric())
  for (i in myTimes){
    dataAtMyTime <- subset(myData, interval == i)
    meanSteps <- mean(dataAtMyTime$steps, na.rm=TRUE)
    myStepsAtInterval <- data.frame(interval=i, steps=meanSteps)
    myStepsPerInterval <- rbind(myStepsPerInterval, myStepsAtInterval)
  }
  
  ## calculating most active interval 
  ggplot(myStepsPerInterval, aes(x=interval, y=steps)) +geom_line() + xlab("")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

``` r
  myMostActiveIndex <- which.max(myStepsPerInterval$steps)
  myMostActiveInterval <- myStepsPerInterval[myMostActiveIndex, 1]
  minutes <- (myMostActiveInterval * 1440) %% 60
  hours <- (myMostActiveInterval * 1440) %/% 60
  myPrintableInterval <- paste(hours, minutes, sep=":")
  print(paste("my most active interval is", myPrintableInterval, sep=" "))
```

```
## [1] "my most active interval is 8:35"
```
## Imputing missing values
## I opted to replace the NA values with average step value for that interval

``` r
  myImputedData <- myData
  missingRows <- 0 ##calculating Missing Rows
  for( i in 1:length(myImputedData$steps)){
    if(is.na(myImputedData[i, 1])){
      missingRows <- missingRows + 1
      myImputedSteps <- subset(myStepsPerInterval, chron(interval) == myData[i, 3])[1,2]
      myImputedData[i,1] <- myImputedSteps
    }
  }
  print(paste("Missing rows are", missingRows, sep=" "))
```

```
## [1] "Missing rows are 2304"
```

``` r
  ##recalculating mean and median with newly imputed data
  myStepsPerDayImputed <- data.frame(date=character(), steps=numeric())
  myDaysImputed <- unique(myImputedData$date)
  for(i in myDaysImputed){
    dataOnMyDay <- subset(myImputedData, date == i)
    totalStepsOnMyDay <- sum(dataOnMyDay$steps, na.rm=TRUE)
    mySteps <- data.frame(date=i, steps=totalStepsOnMyDay)
    myStepsPerDayImputed <- rbind(myStepsPerDay, mySteps)
  }
  meanStepsImputed <- mean(myStepsPerDayImputed$steps)
  medianStepsImputed <- median(myStepsPerDayImputed$steps)
  print(paste("mean steps w/ imputed data are ", meanStepsImputed, sep=""))
```

```
## [1] "mean steps w/ imputed data are 9377.00304321363"
```

``` r
  print(paste("median steps w/ imputed data are ", medianStepsImputed, sep=""))
```

```
## [1] "median steps w/ imputed data are 10417"
```

``` r
  ##according to this imputing the missing data makes very little difference in the mean and median steps taken
  hist(myStepsPerDayImputed$steps, breaks=length(myDaysImputed))
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->
## Are there differences in activity patterns between weekdays and weekends?

``` r
  ##checking to see if each day is a weekend or weekday 
  myImputedData <- cbind(myImputedData, wday=NA)
  for( i in 1:length(myImputedData$date)){
      dayOfTheWeek <- weekdays(as.Date(myImputedData[i, 2]))
      if (dayOfTheWeek == "Saturday" | dayOfTheWeek == "Sunday"){
        myImputedData[i, 4] <- "weekend"
      }
      else {myImputedData[i, 4] <- "weekday"}
  }
  #View(myImputedData)
  
  myStepsPerIntervalWday <- data.frame(interval=times(), steps=numeric(), wkday=character())
  for (i in myTimes){
    dataAtMyTimeWeekday <- subset(myImputedData, interval == i & wday == "weekday")
    dataAtMyTimeWeekend <- subset(myImputedData, interval == i & wday == "weekend")
    meanStepsWeekday <- mean(dataAtMyTimeWeekday$steps, na.rm=TRUE)
    meanStepsWeekend <- mean(dataAtMyTimeWeekend$steps, na.rm=TRUE)
    myStepsAtIntervalWeekday <- data.frame(interval=i, steps=meanStepsWeekday, wkday="weekday")
    myStepsAtIntervalWeekend <- data.frame(interval=i, steps=meanStepsWeekend, wkday="weekend")
    myStepsPerIntervalWday <- rbind(myStepsPerIntervalWday, myStepsAtIntervalWeekday)
    myStepsPerIntervalWday <- rbind(myStepsPerIntervalWday, myStepsAtIntervalWeekend)
  }
  View(myStepsPerIntervalWday)
  ggplot(myStepsPerIntervalWday, aes(y=steps, x=interval)) +geom_line() + xlab("") + facet_grid(rows = vars(wkday))
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->
