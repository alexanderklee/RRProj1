## -- Load the data -- ##
data <- read.csv("activity.csv", header=TRUE)

## -- load required packages to run this program -- ##
install.packages("timeDate")
library(timeDate)

## -- subsetting data to extract steps per day -- ##
stepsByDay <- aggregate(steps~date, data, sum)
head(stepsByDay)

## -- plotting a basic histogram -- ##
hist(stepsByDay$steps, col = "blue",xlab = "Steps", main = "Total Number of Steps Per Day")

## -- calculate mean and median steps per day
mean1 <- mean(stepsByDay$steps)
median1 <- median(stepsByDay$steps)

## What is the average daily activity pattern?

## 3- extracting 5 minute sample -- ##
averageSteps <- aggregate(steps~interval, data, mean)
head(averageSteps)

## -- use a time-series to plot average steps with type equal to 1 -- ##
with(averageSteps, plot(interval, steps ,type="l", main="Average number of steps taken in 5-min interval",col = "blue")) 

## -- obtain the row that contains avg step max -- ##
max <- averageSteps[which.max(averageSteps$steps),]

## -- print out the steps with steps intervals equalling max -- ##
subsetData <- subset(averageSteps, steps == max)
subsetData$interval

## -- Calculate and report the total number of missing values (per row) -- ##
rowSums(is.na(data))

## -- create a new column "newSteps" -- ##
data$newSteps <- averageSteps$steps
head(data)

## -- fill in any missing data with new data -- ##
data$steps <- ifelse(is.na(data$steps),data$newSteps,data$steps)


## -- create a new data set -- ##
data$newSteps <- NULL
newdata <- data
head(newdata)

## -- add new data columns -- ##
sumdata <- aggregate(steps~date, newdata, sum)
head(sumdata)

## -- use histogram to plot new data set steps -- ##
hist(sumdata$steps, col = "blue" ,xlab = "Steps" ,main = "Total Number of Steps Per Day (with the missing data filled in)")

## -- calculate new means and medians using new data set -- ##
mean2 <- mean(sumdata$steps)
median2 <- median(sumdata$steps)

## -- calculate different of medians and means from new vs. old data set -- ##
mean2 - mean1
median2 - median1

## Are there differences in activity patterns between weekdays and weekends?

## -- create new column and determine weekday vs. weekend -- ##
newdata$Weekday <- isWeekday(newdata$date)
head(newdata)

## -- calculate mean steps taken during weekday vs. weekend -- ##
weekday <- subset(newdata, newdata$Weekday == "TRUE")
weekdayMean <- aggregate(steps ~ interval, data = weekday, mean)
head(weekdayMean)

weekend <- subset(newdata, newdata$Weekday == "FALSE")
weekendMean <- aggregate(steps ~ interval, data = weekend, mean)
head(weekendMean)

## -- (panel) plot the weekday vs. weekend step means -- ##
layout(matrix(c(1,1,2,2), 2, 2, byrow = TRUE))
plot(weekdayMean$interval, weekdayMean$steps ,xlab = "interval", ylab = "Number of steps" ,main ="Weekday", col ="blue", type="l") 
plot(weekendMean$interval, weekendMean$steps ,xlab = "interval", ylab = "Number of steps" ,main ="Weekend", col ="red", type="l")

