setwd("C:/Coursera/Course_5/Assignment_1")

library(data.table)

if(!file.exists("activity.csv")){
    fileURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
    download.file(fileURL, destfile = "activity.zip", method = "curl")
    unzip("activity.zip")
}

stepsdata <- read.table("activity.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
stepsdata$date <- as.Date(stepsdata$date, "%Y-%m-%d")

dailysum <- with(stepsdata, tapply(steps, date, sum, na.rm = TRUE))
# dailymean <- with(stepsdata, tapply(steps, date, mean, na.rm = TRUE))
# dailymedian <- with(stepsdata, tapply(steps, date, median, na.rm = TRUE))

hist(dailysum, breaks = 15, xlab = "Sum of Daily Steps", main = "")
totalmean <- mean(dailysum)
totalmedian <- median(dailysum)

# stepsdata <- transform(stepsdata, interval = factor(interval))
meanbyinterval <- with(stepsdata, aggregate(steps ~ interval, FUN = "mean"))
# plot(meanbyinterval$interval, meanbyinterval$steps, type = "n")
lines(meanbyinterval$interval, meanbyinterval$steps)
maxinterval <- meanbyinterval$interval[which.max(meanbyinterval$steps)]

Narows <- !complete.cases(stepsdata)
nNa <- sum(Narows)
rownames(meanbyinterval) <- as.character(meanbyinterval$interval)
meanbyinterval$interval <- NULL
stepsdata$steps[Narows] <- meanbyinterval[as.character(stepsdata$interval[Narows]),1]
dailysum_2 <- with(stepsdata, tapply(steps, date, sum, na.rm = TRUE))

hist(dailysum_2, breaks = 15, xlab = "Sum of Daily Steps", main = "")
totalmean_2 <- mean(dailysum_2)
totalmedian_2 <- median(dailysum_2)

wd_levels <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
wd_labels <- c("Weekday", "Weekday", "Weekday", "Weekday", "Weekday", "Weekend", "Weekend")
stepsdata$WeekDay <- factor(weekdays(stepsdata$date, abbreviate = FALSE), levels = wd_levels, labels = wd_labels)

meanbyintervalbyweekday <- with(stepsdata, aggregate(steps ~ interval + WeekDay, FUN = "mean"))
library(ggplot2)
qplot(interval, steps, data = meanbyintervalbyweekday, facets = WeekDay ~ ., geom = "line")
ggsave("Plot4.png")



