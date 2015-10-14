Loading and preprocessing the data
----------------------------------

    if(!file.exists("activity.csv") ){
    unzip("repdata-data-activity.zip")}  

    act <- read.csv("activity.csv", stringsAsFactor=FALSE) 

What is mean total number of steps taken per day?
-------------------------------------------------

#### 1. Calculate the total number of steps taken per day

    actdata <- act[with (act, {!(is.na(steps))}),]
    steps <- as.numeric (actdata$steps)

    steps_day <- aggregate(steps, list(actdata$date),sum)
    colnames(steps_day) <- c ("date", "steps")
    head(steps_day,3)

    ##         date steps
    ## 1 2012-10-02   126
    ## 2 2012-10-03 11352
    ## 3 2012-10-04 12116

#### 2. A histogram of the total number of steps per day

    hist (steps_day$steps, main = "Histogram of total number of steps per day", 
         xlab = "Total numbers of steps per day") 

![](PA_template_files/figure-markdown_strict/unnamed-chunk-3-1.png)

#### 3. Show the mean and median of the total number of steps per day

    summary (steps_day) 

    ##      date               steps      
    ##  Length:53          Min.   :   41  
    ##  Class :character   1st Qu.: 8841  
    ##  Mode  :character   Median :10765  
    ##                     Mean   :10766  
    ##                     3rd Qu.:13294  
    ##                     Max.   :21194

The mean total number of steps per day is 10766 and the median total
number of steps per day is 10765.

What is the average daily activity pattern?
-------------------------------------------

#### 1. Time series plot (`type = "l"`) of the five-minute interval vs the average number of steps taken, averaged across all day

    interval<- as.numeric (actdata$interval)
    steps_interval <- aggregate(steps ~ interval, actdata, mean) 

    plot (steps_interval$interval, steps_interval$steps, type="l", 
          main = "Average number of steps across all day", 
          xlab = "Interval", 
          ylab = "Average number of steps") 

![](PA_template_files/figure-markdown_strict/unnamed-chunk-5-1.png)

#### 2. Find the interval with the maximum number of steps

    interval_desc <- steps_interval [order (steps_interval$steps,decreasing = TRUE),] 
    max_interval <- interval_desc[1,] 
    max_interval

    ##     interval    steps
    ## 104      835 206.1698

The interval with the maximum number of steps is 835 with 206.1698 steps

Imputing missing values
-----------------------

#### 1.Identify the total number of rows with `NA`s

    sum(is.na(act))

    ## [1] 2304

The total number of rows with `NA`s is 2304

#### 2. Replace `NA`s to the average daily activity pattern and

#### 3. Create a new dataset with no `NA`s

Used the average daily activity pattern for replacing `NA`s to some
values because there are the days with no values for steps (e.g.
2010-10-01).

    act_nona <- act
    for (i in 1:nrow(act_nona)){
      if(is.na(act_nona$steps[i])){
        interval_row <- act_nona$interval[i]
        steps_row <- steps_interval[steps_interval$interval == interval_row,]
        act_nona$steps[i] <- steps_row$steps
      }
    }

Check if there are no `NA`s

    sum(is.na(act_nona))

    ## [1] 0

#### 4-1. A histogram of the total number of steps per day

    nona_steps_day <- aggregate(steps ~date, act_nona, sum)

    hist(nona_steps_day$steps, 
         main = "Histogram of the total number of steps per day (no missing data)", 
         xlab = "Total number of steps per day")

![](PA_template_files/figure-markdown_strict/unnamed-chunk-10-1.png)

#### 4-2. Show the mean and median of the total number of steps per day

    summary (nona_steps_day)

    ##      date               steps      
    ##  Length:61          Min.   :   41  
    ##  Class :character   1st Qu.: 9819  
    ##  Mode  :character   Median :10766  
    ##                     Mean   :10766  
    ##                     3rd Qu.:12811  
    ##                     Max.   :21194

The mean total number of steps per day is 10766 and the median total
number of steps per day is 10766.

**What is the impact of imputing missing data on the estimates of the
total daily number of steps?**

The summary of output before imputing missing data

    summary (steps_day) 

    ##      date               steps      
    ##  Length:53          Min.   :   41  
    ##  Class :character   1st Qu.: 8841  
    ##  Mode  :character   Median :10765  
    ##                     Mean   :10766  
    ##                     3rd Qu.:13294  
    ##                     Max.   :21194

After imputing missing data,

-   The dataframe became (obviously) bigger,  
-   The median value became little higher,
-   The values of the first and third quartiles increased.

Are there differences in activity patterns between weekdays and weekends?
-------------------------------------------------------------------------

#### 1. Add a new variable with two levels (weekday and weekend) to the dataframe which indicates wheter a given date is a weekday or weekend day

    act_nona [, "day"] <- weekdays(as.Date(act_nona$date))
    act_nona [,"day"] <- gsub("Saturday|Sunday", "weekend", act_nona$day)
    act_nona [,"day"] <- gsub("Monday|Tuesday|Wednesday|Thursday|Friday", "weekday", act_nona$day)
    act_nona[,"day"] <- as.factor(act_nona[,"day"])

#### 2. A panel plot with a time series plot (`type = "l"`) of the five-minute interval vs the average number of steps taken acrooss all weekdays or weekends.

    nona_steps_int_days <- aggregate (steps ~interval + day, act_nona, mean)

    library (ggplot2)

    ## Find out what's changed in ggplot2 with
    ## news(Version == "1.0.1", package = "ggplot2")

    ggplot(data = nona_steps_int_days, 
           aes(x= interval, y= steps ))+
      geom_line()+
      facet_wrap(~day, ncol = 1)

![](PA_template_files/figure-markdown_strict/unnamed-chunk-14-1.png)
