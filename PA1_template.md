Reproducible Research PA 1
=================================================================================




Loading Data
=================================================================================

```r
activity <- read.csv("C:/Users/Admin/Downloads/activity.csv")
```


Removing Missing Values
=================================================================================


```r
ActualCase <- complete.cases(activity)
nMissing <- length(ActualCase[ActualCase == FALSE])   # Number of cases with Missing Values
nComplete <- length(ActualCase[ActualCase==TRUE])     # Number of cases with no Missing values
table(ActualCase)
```

```
## ActualCase
## FALSE  TRUE 
##  2304 15264
```


Mean Total Number of Steps Taken per day
=================================================================================





```r
CompleteCases <- subset(activity, complete.cases(activity) == TRUE)      #A subset of Complete Values

splitByDate <- split(CompleteCases, CompleteCases$date, drop=TRUE)      # splits complete cases by Date

dailySteps <- sapply(splitByDate, function(x) sum(x$steps))    #A vector containing total number of daily steps
```


Histogram of Total Number of Steps taken per day
=================================================================================



```r
hist(dailySteps, main = "Hist of Total Steps per Day", xlab = "Steps", ylab="Frequency", col = "yellow")

# plot a histogram

rug(dailySteps, col = "red")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

```r
# place a rug underneath the histogram
```

Mean and Median of the Total number of Steps taken per day
=================================================================================


```r
mean(dailySteps)
```

```
## [1] 10766.19
```

```r
median(dailySteps)
```

```
## [1] 10765
```


Average Daily Activity Pattern
=================================================================================


```r
splitByInterval <- split(CompleteCases, CompleteCases$interval, drop=TRUE)
# split complete cases by interval
AvgSteps <- sapply(splitByInterval, function(x) mean(x$steps)) #Vector of Avg. Steps per Interval

plot(AvgSteps, type = "l", main= "5' Time Series Plot", xlab = "Interval Index", ylab = "Average Steps", col = "blue")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png) 

Time Interval with Max. Number of Steps
================================================================================


```r
max(AvgSteps)
```

```
## [1] 206.1698
```

```r
names(which.max(AvgSteps))
```

```
## [1] "835"
```

Imputing Missing Values
=================================================================================


```r
nMissing
```

```
## [1] 2304
```

```r
newData <- cbind(activity, ActualCase)  #Adding a column 'ActualCase' to the dataset

splitByActual <- split(newData, newData$ActualCase, drop=TRUE) #Splitting the data by ActualCase

for (row in 1:nrow(splitByActual[["FALSE"]])){                # For each row in the split data frame where ActualCase == FALSE, 
# Replace NA with AvgSteps (rounded to the nearest integer)
# the impute value is found in the lookup from the AgSteps vector created earlier  
  splitByActual[["FALSE"]][row,1] <- round(subset(AvgSteps, names(AvgSteps)==as.character(splitByActual[["FALSE"]][row,3])))
}

newData <- rbind(splitByActual[["FALSE"]], splitByActual[["TRUE"]]) #Combining True and False dataframes

newData <- newData[with(newData, order(date, interval))] #re-ordering with date and interval
```

```
## Error in `[.data.frame`(newData, with(newData, order(date, interval))): undefined columns selected
```



Total number of steps taken per day - with Imputed missing Values
================================================================================


```r
splitNewByDate <- split(newData, newData$date, drop=TRUE)    # Split the new data by Date

NewDailySteps <- sapply(splitNewByDate, function(x) sum(x$steps)) # Calculate the total number of steps taken per day
```

Plotting a Histogram of Total Number of Steps Per Day, including Imputed Values
================================================================================


```r
hist(NewDailySteps, main = " New Hist of Total Number of Steps per day", xlab = "Steps", ylab = "Frequency", col = "yellow")  # Histogram of Total Number of steps with imputed values
rug(NewDailySteps, col = "red")
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png) 
Mean and Median of New Daily Steps
================================================================================

```r
mean(NewDailySteps)
```

```
## [1] 10765.64
```

```r
median(NewDailySteps)
```

```
## [1] 10762
```

```r
summary(dailySteps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    8841   10760   10770   13290   21190
```

```r
summary(NewDailySteps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    9819   10760   10770   12810   21190
```
The Quartiles vary a bit, but the mean and median of dailySteps and NewDailySteps are exactly the same.

Impact Of Imputing Missing Data
================================================================================


```r
par(mfrow = c(1,2), cex = 0.66, mex = 2)

##Plotting the original Histogram

hist(dailySteps, main = "Hist of Total Steps per Day", xlab = "Steps", ylab="Frequency", col = "yellow")
abline(v = mean(dailySteps), lwd = 4, col = "blue")
abline(v = median(dailySteps), lty = 5, lwd = 2, col = "red")
text(mean(dailySteps), 25, pos = 2, col = "blue", labels = "Mean", cex=1.5)
text(median(dailySteps), 25, pos = 4, col = "red", labels = "Median", cex = 1.5)


# plot a histogram

rug(dailySteps, col = "red")

# place a rug underneath the histogram


### Plotting the new Histogram
hist(NewDailySteps, main = " New Hist:Total Number of Steps per day", xlab = "Steps", ylab = "Frequency", col = "yellow", )  # Histogram of Total Number of steps with imputed values
abline(v = mean(NewDailySteps), lwd = 4, col = "blue")
abline(v = median(NewDailySteps), lty = 5, lwd = 2, col = "red")
text(mean(NewDailySteps), 35, labels = "Mean",pos = 2, col = "blue", cex = 1.5)
text(median(NewDailySteps), 35, labels = "Median", pos = 4, col = "red", cex = 1.5)
rug(NewDailySteps, col = "red")
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12-1.png) 


Differences in Activity patterns between Weekdays and Weekends
================================================================================


```r
newData$date <- as.Date(strptime(newData$date, format = "%Y-%m-%d"))#convert data to date() class type variable
newData$day <- weekdays(newData$date)   #build a day factor to hold weekday or weekend

for (i in 1:nrow(newData)) {                             #for each day
  if (newData[i,]$day %in% c("Saturday", "Sunday")){     #if Saturday or Sunday
                                                          
    
  
    newData[i,]$day <- "weekend"                         #then 'weekend'
  
}

else {
  newData[i,]$day <- "weekday"                           #else 'weekday'
  
}

}
```

Time Series Plot : Activity - Weekday Vs. Weekend
================================================================================


```r
# aggregate newData with average of steps and interval across weekends and weekdays
stepsByDay <- aggregate(newData$steps ~ newData$interval + newData$day, newData, mean)

# Set clear column names
names(stepsByDay)<- c("interval", "day", "steps")

#Plot weekday over weekend Time Series

par(mfrow=c(1,1), cex = 1, mex = 2)  
with(stepsByDay, plot(steps ~ interval, type="n", main="Weekday vs. Weekend Avg."))  
with(stepsByDay[stepsByDay$day == "weekday",], lines(steps ~ interval, type="l", col="red"))  
with(stepsByDay[stepsByDay$day == "weekend",], lines(steps ~ interval, type="l", col="16" ))  
legend("topright", lty=c(1,1), col = c("red", "16"), legend = c("weekday", "weekend"), seg.len=3)
```

![plot of chunk unnamed-chunk-14](figure/unnamed-chunk-14-1.png) 

Looks like this person walks a lot on the weekends!



