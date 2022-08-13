## Download and Unzip Dataset
if(!file.exists("./data")){dir.create("./data")}
fileURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(fileURL,"./data/dataset.zip")
unzip("./data/dataset.zip")

## Read csv data
data <- read.csv("activity.csv",header = TRUE)

## Calculate mean total number of steps taken each day
### Total number of steps each day
totalstepsday <- aggregate(steps ~ date, data, FUN = sum)
hist(totalstepsday$steps, xlab = "Sum of Steps", main = "Histogram of Total Sum of Steps each day")
## Mean and Median
mean(totalstepsday$steps)
median(totalstepsday$steps)

## Average daily activity pattern
library(ggplot2)
averagestepsi <- aggregate(steps ~ interval, data, FUN = mean)
ggplot(averagestepsi, aes(x = interval, y = steps)) + 
        geom_line() + 
        ggtitle("Average Daily Activity Pattern") + 
        xlab("5 Minute Interval") + 
        ylab("Average Number of Steps") +
        theme(plot.title = element_text(hjust = .5))
## Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
averagestepsi[which.max(averagestepsi$steps),]

## Total Number of NAs
newdata <- data
sum(is.na(newdata$steps))

## Inpute missing values by replacing NAs with the average steps by interval
library(dplyr)
newdata <- newdata %>% left_join(averagestepsi, by = "interval")
newdata$newsteps <- ifelse(is.na(newdata$steps.x), newdata$steps.y, newdata$steps.x)
newdata$steps.x = NULL
newdata$steps.y = NULL
totalnewstepsday <- aggregate(newsteps ~ date, newdata, FUN = sum)
hist(totalnewstepsday$newsteps, xlab = "Sum of Steps", main = "Histogram of Total Sum of Steps each day")
## Mean and Median
mean(totalnewstepsday$newsteps)
median(totalnewstepsday$newsteps)

## Are there differences in activity patterns between weekdays and weekends?

## Change type of date from chr to Date
newdata$date <- as.Date(newdata$date)
## Create new factor variable with levels - "Weekend" and "Weekday"
newdata$date <- ifelse(weekdays(newdata$date) %in% c("Saturday","Sunday"), "Weekend", "Weekday")
## Make panel plot containing a time series plot of the 5-minute interval(x) and the average number of steps taken(y),
## averaged across all weekday days or weekend days.
averagestepsday <- aggregate(newsteps ~ date + interval, newdata, FUN = mean)
ggplot(averagestepsday, aes(x = interval, y = newsteps)) + 
        geom_line() + 
        ggtitle("Average Daily Activity Pattern across Weekday or Weekend") + 
        facet_grid(date ~ .) +
        xlab("5 Minute Interval") + 
        ylab("Average Number of Steps") +
        theme(plot.title = element_text(hjust = .5))