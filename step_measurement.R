##
##  Dependancies Checking
##
require(dplyr)
require(lattice)

##
##  Libraries Loading
##
library(dplyr)
library(lattice)

## 
## 1.Data Loading
##

activities <- read.csv("activity.csv", stringsAsFactors = FALSE)
activities$date <- as.Date(activities$date)

##
## 2.What is mean total number of steps taken per day?
##

totalNumberStepsPerDay <- activities %>% group_by(date)  %>% summarise(daily = sum(steps, na.rm = TRUE)) 
hist(totalNumberStepsPerDay$daily, breaks=20)
abline(v = mean(totalNumberStepsPerDay$daily, na.rm = TRUE), col = "blue", lwd = 2)
abline(v = median(totalNumberStepsPerDay$daily, na.rm = TRUE), col = "red", lwd = 2)

##
## 3.What is the average daily activity pattern?
##

averageStepsPerInterval <- activities %>% group_by(interval)  %>% summarise(averageInterval = mean(steps, na.rm = TRUE)) 
plot(as.numeric(averageStepsPerInterval$interval), averageStepsPerInterval$averageInterval, type="l")
maxInterval <- averageStepsPerInterval[which.max(averageStepsPerInterval$averageInterval), "interval"][[1]]
abline(v = maxInterval, col = "blue", lwd = 2)

##
## 4.Imputing missing values
##

sum(is.na(activities))

## mean for each interval 

emptyIndexes <- which(is.na(activities$steps))
nullInterval <- activities[emptyIndexes,]$interval
addActivities <- activities
addActivities[emptyIndexes,]$steps <- merge(addActivities[emptyIndexes,], averageStepsPerInterval)$averageInterval

totalNumberStepsPerDay <- addActivities %>% group_by(date)  %>% summarise(daily = sum(steps, na.rm = TRUE)) 
hist(totalNumberStepsPerDay$daily, breaks=20)
abline(v = mean(totalNumberStepsPerDay$daily, na.rm = TRUE), col = "blue", lwd = 2)
abline(v = median(totalNumberStepsPerDay$daily, na.rm = TRUE), col = "red", lwd = 2)

##
## 5. Are there differences in activity patterns between weekdays and weekends?
##

getWeekPeriod <- function(d) {
    if(weekdays(d) %in% c("Saturday", "Sunday")) {
        return("weekend")
    }
    else {
        return("weekday")
    }
}

addActivities <- cbind(addActivities, weekPeriod = mapply(getWeekPeriod, addActivities$date))
addActivities <- addActivities %>% 
                 group_by(interval, weekPeriod)  %>% 
                 summarise(averageInterval = mean(steps, na.rm = TRUE))

panelPlot <- xyplot(averageInterval~interval | weekPeriod,
                    type="l", layout=c(1, 2), 
                    data = addActivities, as.table = TRUE,
                    xlab = "Interval", ylab = "Number of steps")

