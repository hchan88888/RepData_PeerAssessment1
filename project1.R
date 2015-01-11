repro1 <- function(){
    library(fields)
    
    # load data
    file <- read.csv("activity.csv")
    
    #process/transform data
    file$steps <- as.numeric(as.character(file$steps))
    file$date <- as.Date(as.character(file$date))
    file$interval <- as.integer(as.character(file$interval))
    
    # select rows without NA
    valid <-na.omit(file)
    
    # find total number of steps taken each day
    by_date <- aggregate(steps ~ date, data=valid, FUN = sum)
    
    #histogram
    hist(by_date$steps, breaks=8, main = "Histogram of the total number of steps taken each day", xlab = "Steps")
    
    # mean and median total number of steps taken each day
    mean(by_date$steps)
    median(by_date$steps)
    
    # group records by interval
    by_interval <- aggregate(steps ~ interval, data=valid, FUN = mean)
    
    #plot a time series plot
    plot(by_interval, type="l")
    
    # find interval with maximum number of steps
    cat("Maximum number of steps:")
    by_interval$interval[by_interval$steps == max(by_interval$steps)]
    
    #check where NA is
    sum(is.na(file$steps))
    sum(is.na(file$date))
    sum(is.na(file$interval))
    #conclusion: only steps has NA
    
    #select all records with NA
    isNA <- file[is.na(file$steps) == TRUE,]
    
    #number of NA rows
    cat("Total number of missing values:")
    dim(isNA)[1]
    
    #replace NA with average steps across all days of the corresponding interval
    interval <- by_interval[,1]
    for (i in interval){
        isNA$steps[isNA$interval == i] <- by_interval$steps[by_interval$interval== i]
    }
    # recombine the records with and without NA and then sort
    recombine <- rbind(valid, isNA)
    recombine <- recombine[order(recombine$date, recombine$interval),]
    
    # find total number of steps taken each day
    by_date_r <- aggregate(steps ~ date, data=recombine, FUN = sum)
    
    #create histogram
    hist(by_date_r$steps, breaks=8, main = "Histogram of the total number of steps taken each day", xlab="Steps")
    
    #mean and median
    mean(by_date_r$steps)
    median(by_date_r$steps)
    #conclusion: no change from original data because the mean is used
    
    #convert date into day of the week
    recombine$weekday <- weekdays(recombine$date)
    
    #convert date of the week into either weekday or weekend
    recombine$weekday[recombine$weekday == "Monday"] <- "weekday"
    recombine$weekday[recombine$weekday == "Tuesday"] <- "weekday"
    recombine$weekday[recombine$weekday == "Wednesday"] <- "weekday"
    recombine$weekday[recombine$weekday == "Thursday"] <- "weekday"
    recombine$weekday[recombine$weekday == "Friday"] <- "weekday"
    recombine$weekday[recombine$weekday == "Saturday"] <- "weekend"
    recombine$weekday[recombine$weekday == "Sunday"] <- "weekend"
    
    # group records by weekday and weekend
    weekday <- recombine[which(recombine$weekday == "weekday"),]
    weekend <- recombine[which(recombine$weekday == "weekend"),]
    
    # group records by interval
    weekend_interval <- aggregate(steps ~ interval, data=weekend, FUN = mean)
    weekday_interval <- aggregate(steps ~ interval, data=weekday, FUN = mean)
    
    #create panel plots
    set.panel(2,1)
    plot(weekend_interval$steps ~weekend_interval$interval, type="l", xlab = "Interval", ylab = "Number of steps", main="Weekend")
    plot(weekday_interval$steps ~weekday_interval$interval, type="l", xlab = "Interval", ylab = "Number of steps", main ="Weekday")
    #conclusion: the subject walks more over the weekend. The subject's job during the weekdays is sedentary. 
    
}
