# 1. Code for reading in the dataset and/or processing the data
# 2. Histogram of the total number of steps taken each day
# 3. Mean and median number of steps taken each day
# 4. Time series plot of the average number of steps taken
# 5. The 5-minute interval that, on average, contains the maximum number of steps
# 6. Code to describe and show a strategy for imputing missing data
# 7. Histogram of the total number of steps taken each day after missing values are imputed
# 8. Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
# 9. All of the R code needed to reproduce the results (numbers, plots, etc.) in the report

library(dplyr)
library(ggplot2)
library(gridExtra)
library(taRifx)

unzip("activity.zip")
activity <-  read.csv("activity.csv", sep = ",") %>%
  transform(steps <- as.numeric(steps)) %>%
  remove.factors()

activity$date <- as.Date(activity$date, "%Y-%m-%d")
  
complete_activity <- complete.cases(activity)
act_no_NA <- activity[complete_activity,]

sum_steps_day <- aggregate(act_no_NA$steps, list(act_no_NA$date), sum)
mean_steps_day <- aggregate(act_no_NA$steps, list(act_no_NA$date), mean)
median_steps_day <- aggregate(act_no_NA$steps, list(act_no_NA$date), median)

hist(sum_steps_day$x, breaks = 20, xlab = "Number of Steps per Day")

avg_steps_interval <- aggregate(act_no_NA$steps, list(factor(act_no_NA$interval)), median)
avg_steps_interval <- remove.factors(avg_steps_interval)

plot(avg_steps_interval$Group.1,avg_steps_interval$x, type = "l")


avg_steps_interval$Group.1[which.max(avg_steps_interval$x)]
avg_steps_interval$x[which.max(avg_steps_interval$x)]

#Calculando a quantidade de NA

nrow_dataset <- nrow(activity)
missing_values_dataset <- sum(!complete_activity)
(missing_values_dataset/nrow_dataset)*100

#with(avg_steps_interval, x[which(Group.1==25)])
# 
# filled_activity <- activity %>%
#   transform(activity[!complete_activity,"steps"] <- with(avg_steps_interval,x[which(Group.1==activity$interval)]))

filled_activity <- activity

for ( i in 1:nrow_dataset){
  if (is.na(filled_activity$steps[i])){
    filled_activity$steps[i] <- with(avg_steps_interval,x[which(Group.1==filled_activity$interval[i])])
  }
}

sum_steps_day_filled <- aggregate(filled_activity$steps, list(filled_activity$date), sum)

hist(sum_steps_day_filled$x, breaks = 20, xlab = "Number of Steps per Day")

filled_activity$weekday <- weekdays(filled_activity$date, abbreviate = TRUE)
filled_activity$weekend <- factor(filled_activity$weekday, levels = teste2, labels=c(rep("non-weekend",5),rep("weekend",2)))
filled_activity_weekend <- split(filled_activity, filled_activity$weekend)

filled_activity_weekend <- filled_activity %>%
                          select(interval,weekend,steps) %>%
                          group_by(interval, weekend) %>% 
                          summarise_each(funs(mean))

# avg_filled_activity_weekend <- aggregate(filled_activity_weekend$weekend$steps, list(filled_activity_weekend$weekend$interval), mean)
