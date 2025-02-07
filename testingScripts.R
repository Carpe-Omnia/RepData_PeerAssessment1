dt <- function() {
  library(ggplot2)
  library(chron)
  library(lubridate)
  
  #part 1. Getting and cleaning the data
  myData <- read.csv("./activity.csv")
  myData$date <- as.Date(myData$date)

  ##running through the intervals to make them reasonable time objects
  ##hate that I have to do this
  for(i in 1:length(myData$interval)){
    stringVal <- myData[i, 3]
    myString <- ""
    if(nchar(stringVal) == 1){
      myString <- paste("00:0", stringVal, ":00", sep="")
    }
    else if (nchar(stringVal) == 2){
      myString <- paste("00:", stringVal, ":00", sep="")
      }
    else if (nchar(stringVal) == 3){
      myString <- paste("0", substring(stringVal,1,1), ":", substring(stringVal, 2, 3), ":00", sep="")
    }
    else{
      myString <- paste(substring(stringVal,1,2), ":", substring(stringVal, 3, 4), ":00", sep="")
    }  
    myData[i, 3] <- chron(times=myString)
  }
  #end part 1
  
  
  #part 2 
  myDays <- unique(myData$date)
  myStepsPerDay <- data.frame(date=character(), steps=numeric())
  
  for(i in myDays){
    dataOnMyDay <- subset(myData, date == i)
    totalStepsOnMyDay <- sum(dataOnMyDay$steps, na.rm=TRUE)
    mySteps <- data.frame(date=i, steps=totalStepsOnMyDay)
    myStepsPerDay <- rbind(myStepsPerDay, mySteps)
  }
  hist(myStepsPerDay$steps, breaks=length(myDays))
  meanSteps <- mean(myStepsPerDay$steps)
  medianSteps <- median(myStepsPerDay$steps)
  print(paste("mean steps are ", meanSteps, sep=""))
  print(paste("median steps are ", medianSteps, sep=""))
  #end part 2
  
  #part 3 
  myTimes <- unique(myData$interval)
  myStepsPerInterval <- data.frame(interval=times(), steps=numeric())
  for (i in myTimes){
    dataAtMyTime <- subset(myData, interval == i)
    meanSteps <- mean(dataAtMyTime$steps, na.rm=TRUE)
    myStepsAtInterval <- data.frame(interval=i, steps=meanSteps)
    myStepsPerInterval <- rbind(myStepsPerInterval, myStepsAtInterval)
  }
  #end part 3 
  
  ## calculating most active interval 
  ggplot(myStepsPerInterval, aes(x=interval, y=steps)) +geom_line() + xlab("")
  myMostActiveIndex <- which.max(myStepsPerInterval$steps)
  myMostActiveInterval <- myStepsPerInterval[myMostActiveIndex, 1]
  print(paste("my most active interval is", myMostActiveInterval, sep=" "))
  
  #imputing missing values
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
  print(paste("median steps w/ imputed data are ", medianStepsImputed, sep=""))
  ##according to this imputing the missing data makes very little difference in the mean and median steps taken
  hist(myStepsPerDayImputed$steps, breaks=length(myDaysImputed))
  
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
  
}