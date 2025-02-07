dt <- function() {
  #part 1 
  myData <- read.csv("./activity.csv")
  myData$date <- as.Date(myRawData$date)
  library(ggplot2)
  ##running through the intervals to make them reasonable time objects
  ##hate that I have to do this
  library(chron)
  for(i in 1:length(myData$interval)){
    stringVal <- myRawData[i, 3]
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
  #View(myData)
  #end part 1
  
  
  #part 2 
  library(lubridate)
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
  ggplot(myStepsPerInterval, aes(x=interval, y=steps)) +geom_line() + xlab("")
  myMostActiveIndex <- which.max(myStepsPerInterval$steps)
  myMostActiveInterval <- myStepsPerInterval[myMostActiveIndex, 1]
  print(myMostActiveInterval)
  #View(myStepsPerInterval)
}