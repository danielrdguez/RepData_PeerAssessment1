# Reproducible Research: Peer Assessment 1

## Loading and preprocessing the data

### Load the data
There are 3 field data in csv file: steps, date and interval. Date will be loaded as character field to be transformed later.

```r
act <- read.csv("activity.csv", colClasses = c("numeric", "character", "numeric"))
```

### Process/transform the data into a format suitable
Interval is not a magnitude in which we are going to do calculations so it will be converted to factor.

```r
act$interval <- factor(act$interval)
act$date <- as.Date(act$date, format = "%Y-%m-%d")
```


## What is mean total number of steps taken per day?
We need obtain the sum of steps each day so:

```r
total_steps <- aggregate(steps ~ date, data = act, FUN = sum)
colnames(total_steps) <- c("date", "steps")
```


###  Histogram of the total number of steps taken each day

```r
hist(total_steps$steps, xlab = "steps per day", main = "total number of steps taken each day")
```

![plot of chunk histogram](figure/histogram.png) 

###   Mean and median total number of steps taken per day
With summary function we have both mean and median of steps per day. 

I have spent lot of time with this, although in console results are ok, knitr rounds to 10800. With digits option, at least it shows a better approximation.


```r
summary(total_steps$steps, digits = 12)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    8841   10765   10766   13294   21194
```


## What is the average daily activity pattern?

### Time series plot of the 5-minute interval and the average number of steps taken, averaged across all days


```r
interval_steps <- aggregate(steps ~ interval, data = act, FUN = mean)
colnames(interval_steps) <- c("interval", "steps")
plot(interval_steps$interval, interval_steps$steps, type = "n", xlab = "interval", 
    ylab = "steps average", main = "steps average each 5 min interval")
lines(interval_steps$interval, interval_steps$steps, type = "l")
```

![plot of chunk aggregate_5min_data](figure/aggregate_5min_data.png) 



### Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
max_steps <- max(interval_steps$steps, na.rm = T)
max_interval <- interval_steps[interval_steps$steps == max_steps, ]
max_interval[1]
```

```
##     interval
## 104      835
```


## Imputing missing values

### Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
sum(is.na(act$steps))
```

```
## [1] 2304
```


### Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

The strategy will consist in assign the NA values the mean of its interval.


### Create a new dataset that is equal to the original dataset but with the missing data filled in.

A new dataset is created as new_act


```r
new_act <- act
for (i in 1:nrow(new_act)) {
    int <- new_act[i, ]
    if (is.na(int$steps)) {
        z <- new_act[new_act$interval == int$interval, ]
        x <- mean(z$steps, na.rm = TRUE)
        new_act[i, 1] = x
    }
}
```


### Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

The first and third quartile seemed to be affected after imputation of NA values. However min, max, mean and median values remained in same levels.


```r
total_steps <- aggregate(steps ~ date, data = new_act, FUN = sum)
colnames(total_steps) <- c("date", "steps")
```


```r
hist(total_steps$steps, xlab = "steps per day", main = "total number of steps taken each day")
```

![plot of chunk histogram_new_data](figure/histogram_new_data.png) 



```r
summary(total_steps$steps, digits = 12)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    9819   10766   10766   12811   21194
```


## Are there differences in activity patterns between weekdays and weekends?



### Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r

new_act_wd <- new_act
new_act_wd$wd <- rep("weekday", nrow(new_act_wd))

for (i in 1:nrow(new_act_wd)) {
    if (weekdays(as.Date(new_act_wd[i, "date"])) == "Saturday" | weekdays(as.Date(new_act_wd[i, 
        "date"])) == "Sunday") {
        new_act_wd[i, 4] <- "weekend"
    }
}

new_act_wd$wd <- factor(new_act_wd$wd)
```


### Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was creating using simulated data:

First activity data are filtered by weekday or weekend


```r

act_wd <- new_act_wd[new_act_wd$wd == "weekday", ]
act_we <- new_act_wd[new_act_wd$wd == "weekend", ]
```


Then steps are aggregated by interval:

```r
int_steps_wd <- aggregate(steps ~ interval, data = act_wd, FUN = mean)
colnames(int_steps_wd) <- c("interval", "steps")

int_steps_we <- aggregate(steps ~ interval, data = act_we, FUN = mean)
colnames(int_steps_we) <- c("interval", "steps")
```


The graph is rendered:

```r
par(mfrow = c(2, 1))

plot(int_steps_wd$interval, int_steps_wd$steps, type = "n", xlab = "interval", 
    ylab = "steps average", main = "weekdays steps average each 5 min interval")

lines(int_steps_wd$interval, int_steps_wd$steps, type = "l")

plot(int_steps_we$interval, int_steps_we$steps, type = "n", xlab = "interval", 
    ylab = "steps average", main = "weekend steps average each 5 min interval")

lines(int_steps_we$interval, int_steps_we$steps, type = "l")
```

![plot of chunk last_plot_3](figure/last_plot_3.png) 


The resultant measures are exposed:

```r

summary(act_wd)
```

```
##      steps            date               interval           wd       
##  Min.   :  0.0   Min.   :2012-10-01   0      :   45   weekday:12960  
##  1st Qu.:  0.0   1st Qu.:2012-10-16   5      :   45   weekend:    0  
##  Median :  0.0   Median :2012-10-31   10     :   45                  
##  Mean   : 35.6   Mean   :2012-10-31   15     :   45                  
##  3rd Qu.: 24.0   3rd Qu.:2012-11-15   20     :   45                  
##  Max.   :806.0   Max.   :2012-11-30   25     :   45                  
##                                       (Other):12690
```

```r

summary(act_we)
```

```
##      steps            date               interval          wd      
##  Min.   :  0.0   Min.   :2012-10-06   0      :  16   weekday:   0  
##  1st Qu.:  0.0   1st Qu.:2012-10-18   5      :  16   weekend:4608  
##  Median :  0.0   Median :2012-10-31   10     :  16                 
##  Mean   : 42.4   Mean   :2012-10-31   15     :  16                 
##  3rd Qu.: 35.5   3rd Qu.:2012-11-12   20     :  16                 
##  Max.   :785.0   Max.   :2012-11-25   25     :  16                 
##                                       (Other):4512
```


