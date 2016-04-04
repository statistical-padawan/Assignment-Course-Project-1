##############################################################################
#
# First, Set working directory to the .csv file
#

setwd("F:/Data Science/Reproducible Research/repdata_data_activity")

##############################################################################
#
# 1. Code for reading in the dataset and/or processing the data
#
##############################################################################

fitbit = read.csv("activity.csv")

#
#
# Take a look at the data to see what were dealing with.

str(fitbit)

#'data.frame':	17568 obs. of  3 variables:
#$ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
#$ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
#$ interval: int  0 5 10 15 20 25 30 35 40 45 ...
# Loads of "NA"s to deal with.
#

head(fitbit)

#steps       date interval
#1    NA 2012-10-01        0
#2    NA 2012-10-01        5
#3    NA 2012-10-01       10
#4    NA 2012-10-01       15
#5    NA 2012-10-01       20
#6    NA 2012-10-01       25
#

library(plyr)
library(dplyr)
library(ggplot2)
library(lubridate)

###1. What is mean total number of steps taken per day?
# 
# Calculate the total number of steps per day.

steps <- tapply(fitbit$steps, fitbit$date, FUN = sum, na.rm = TRUE)
fitbit$date <- ymd(fitbit$date)

##############################################################################
# 2. What is mean total number of steps taken per day? 
##############################################################################
#Calculate and report the mean and median of the total number of steps taken 
#per day.

mean(steps)
#[1] 9354.23

median(steps)
#[1] 10395
#
#For this part of the assignment, you can ignore the missing values in the dataset.
#
#Calculate the total number of steps taken per day
#
#
steps <- fitbit %>%
filter(!is.na(steps)) %>%
        group_by(date) %>%
        summarize(steps = sum(steps)) %>%
        print      
#
#Source: local data frame [53 x 2]
#
#date steps
#(time) (int)
#1  2012-10-02   126
#2  2012-10-03 11352
#3  2012-10-04 12116
#4  2012-10-05 13294
#5  2012-10-06 15420
#6  2012-10-07 11015
#7  2012-10-09 12811
#8  2012-10-10  9900
#9  2012-10-11 10304
#10 2012-10-12 17382
#..        ...   ...
#
# Make a histogram of the total number of steps taken each day
# I couldn't get "geom_histogram" to work
# I kept erroring with "Error: Unknown parameters: binwidth, bins, pad"
# So I improvised with "geom_bar"

ggplot(steps, aes(x=date, y=steps))+geom_bar(stat="identity", fill="Red")
+ xlab("Dates")+ ylab("Steps")+ labs(title= "Total numbers of Steps per day")


##############################################################################
# 3. What is the average daily activity pattern?
##############################################################################
#Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) 
#and the average number of steps taken, averaged across all days (y-axis)

daily <- fitbit %>%
        filter(!is.na(steps)) %>%
        group_by(interval) %>%
        summarize(steps=mean(steps)) %>%
        print

#Source: local data frame [288 x 2]
#
#interval     steps
#(int)     (dbl)
#1         0 1.7169811
#2         5 0.3396226
#3        10 0.1320755
#4        15 0.1509434
#5        20 0.0754717
#6        25 2.0943396
#7        30 0.5283019
#8        35 0.8679245
#9        40 0.0000000
#10       45 1.4716981
#..      ...       ...
#

plot(daily, type = "l", lty=1, col=4)

#Which 5-minute interval, on average across all the days in the dataset, 
#contains the maximum number of steps?

daily[which.max(daily$steps), ]$interval
#[1] 835
#
#Calculate and report the total number of missing values in the dataset 
#(i.e. the total number of rows with NAs)

total.missing <- sum(is.na(fitbit))
total.missing

#[1] 2304
##############################################################################
# 4. Inputting Missing Values
##############################################################################
# Devise a strategy for filling in all of the missing values in the dataset. 
# The strategy does not need to be sophisticated. For example, you could use 
# the mean/median for that day, or the mean for that 5-minute interval, etc.
# Create a new dataset that is equal to the original dataset but with the 
# missing data filled in.

strategy <- fitbit %>%
        group_by(interval) %>%
        mutate(steps = ifelse(is.na(steps), mean(steps, na.rm=TRUE), steps))
summary(strategy)

#    steps             date               interval     
#Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0  
#1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8  
#Median :  0.00   Median :2012-10-31   Median :1177.5  
#Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5  
#3rd Qu.: 27.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2  
#Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0  
#
#Make a histogram of the total number of steps taken each day
#


new.strategy <- strategy %>%
        group_by(date) %>%
        summarize(steps = sum(steps)) %>%
        print    

#Source: local data frame [61 x 2]
#
#date    steps
#(time)    (dbl)
#1  2012-10-01 10766.19
#2  2012-10-02   126.00
#3  2012-10-03 11352.00
#4  2012-10-04 12116.00
#5  2012-10-05 13294.00
#6  2012-10-06 15420.00
#7  2012-10-07 11015.00
#8  2012-10-08 10766.19
#9  2012-10-09 12811.00
#10 2012-10-10  9900.00
#..        ...      ...
# I couldn't get geom_histogram to work
# I kept erroring with "Error: Unknown parameters: binwidth, bins, pad"
# So I improvised with "geom_bar"

ggplot(new.strategy, aes(x=date, y=steps))+geom_bar(stat="identity", 
fill="Purple")+ xlab("Dates")+ ylab("Imputed Steps")
+ labs(title= "Total numbers of Steps per day (missing data imputed)")

#Calculate and report the mean and median total number of steps taken per day.
#

fitbit.steps <- tapply(strategy$steps, strategy$date, FUN = sum, na.rm = TRUE)
strategy$date <- ymd(strategy$date)
mean(fitbit.steps)

# [1] 10766.19

#Do these values differ from the estimates from the first part of the assignment?

mean(steps)==mean(fitbit.steps)

#[1] FALSE

median(steps)==median(fitbit.steps)

#[1] FALSE

summary(steps)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0    6778   10400    9354   12810   21190

summary(fitbit.steps)

#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#41    9819   10770   10770   12810   21190

#What is the impact of imputing missing data on the estimates of the total 
#daily number of steps?
# The summary numbers increased by 41, 3041, 370, 1416, 0, 0 respectively.

summary(steps) - summary(fitbit.steps)

#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-41   -3041    -370   -1416       0       0 

par(mfrow=c(2,1))
hist(steps,col="red")
hist(fitbit.steps,col="purple")
##############################################################################
# 5. Are there differences in activity patterns between weekdays and weekends?
##############################################################################
#
#  I need two more variables now, "weekend" and "weekday" using the filled in
# missing values.
#
dayofweek <- function(date) {
        if (weekdays(as.Date(date)) %in% c("Saturday", "Sunday")) {
                "weekend"
        } else {
                "weekday"
        }
}
new.strategy$daytype <- as.factor(sapply(new.strategy$date, dayofweek))

#  Make a time series plot (i.e. type = "l") of the 5-minute interval 
# (x-axis) and the average number of steps taken, averaged across all days 
# (y-axis)

par(mfrow = c(2, 1))
for (type in c("weekend", "weekday")) {
        steps.type <- aggregate(steps ~ date, data = new.strategy, subset = new.strategy$daytype == 
                type, FUN = mean)
        plot(steps.type, type = "l", main = type)
}



