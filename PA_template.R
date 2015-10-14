
if(!file.exists("activity.csv") ){
unzip("repdata-data-activity.zip")}  

act <- read.csv("activity.csv", stringsAsFactor=FALSE) 
str(act)


##Q1 The mean total number of steps per day
##1. Calculate the total number of steps taken per day

actdata <- act[with (act, {!(is.na(steps))}),]
steps <- as.numeric (actdata$steps)
steps_day <- aggregate(steps, list(actdata$date),sum)
colnames(steps_day) <- c ("date", "steps")
head(steps_day)
tail(steps_day)

##2. A histogram of the total number of steps per day

hist (steps_day$steps, main = "Histogram of total number of steps per day", 
     xlab = "Total numbers of steps per day") 

##3. show the mean and median of the total number of steps per day

summary (steps_day) 

##Q2 The average daily activity pattern
##1. Time series plot of the five-minute interval vs the average number of steps 
##taken, averaged across all day

interval<- as.numeric (actdata$interval)
steps_interval <- aggregate(steps ~ interval, actdata, mean) 
head(steps_interval)
tail (steps_interval)

plot (steps_interval$interval, steps_interval$steps, type="l", 
      main = "Average number of steps across all day", 
      xlab = "Interval", 
      ylab = "Average number of steps") 

##2. Find the interval with the maximum number of steps

interval_desc <- steps_interval [order (steps_interval$steps,decreasing = TRUE),] 
max_interval <- interval_desc[1,] 
max_interval

##Q3. Imputting missing values
#1.Identify the total number of rows with NAs

sum(is.na(act))

##2. Replace na to the average daily activity pattern 
## (because there are the days with no values for steps)
##3. and create a new dataset with no NA 

act_nona <- act
for (i in 1:nrow(act_nona)){
  if(is.na(act_nona$steps[i])){
    interval_row <- act_nona$interval[i]
    steps_row <- steps_interval[steps_interval$interval == interval_row,]
    act_nona$steps[i] <- steps_row$steps
  }
}
sum(is.na(act_nona))

##4. A histogram of the total number of steps per day and 
## show the mean and median of the total number of steps per day

nona_steps_day <- aggregate(steps ~date, act_nona, sum)
head(nona_steps_day,3)

hist(nona_steps_day$steps, 
     main = "Histogram of the total number of steps per day (no missing data)", 
     xlab = "Total number of steps per day")

summary (nona_steps_day)
summary (steps_day)

##Q4. Difference in activity patterns between weekdays and weekends
##1. Add a new variable with two levels (weekday and weekend) to the dataframe
## which indicates wheter a given date is a weekday or weekend day

act_nona [, "day"] <- weekdays(as.Date(act_nona$date))
act_nona [,"day"] <- gsub("Saturday|Sunday", "weekend", act_nona$day)
act_nona [,"day"] <- gsub("Monday|Tuesday|Wednesday|Thursday|Friday", "weekday", act_nona$day)
act_nona[,"day"] <- as.factor(act_nona[,"day"])

##2. A panel plot with a time series plot of the five-minute interval vs
## the average number of steps taken acrooss all weekdays or weekends.

nona_steps_int_days <- aggregate (steps ~interval + day, act_nona, mean)

library (ggplot2)

qplot(interval, steps, 
      data = nona_steps_int_days, 
      type = "l", 
      geom = c("line"), 
      xlab = "Five-minute interval", 
      ylab = "Number of steps",
      main = "") +
  facet_wrap(~day, ncol = 1)

ggplot(data = nona_steps_int_days, 
       aes(x= interval, y= steps ))+
  geom_line()+
  facet_wrap(~day, ncol = 1)
