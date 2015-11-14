
#changing time locale for R (windows)
Sys.setlocale("LC_TIME", "English")

library(ggplot2)

activity <- read.csv("reproducible_research/RepData_PeerAssessment1/activity/activity.csv")
activity2 <- activity[!is.na(activity$steps),]



#Calculate the total number of steps taken per day
total <- tapply(activity2$steps, factor(activity2$date), sum)
#saving to file
jpeg("reproducible_research/RepData_PeerAssessment1/figure/hist1.png",width=480,height=480)
hist(total,xlim = c(0,25000),ylim = c(0,30),main="Total number of steps taken per day",xlab = "steps")

dev.off()


#the mean/total of the number of the  number of steps taken per day
mean <- tapply(activity2$steps, factor(activity2$date), mean)
median <- tapply(activity2$steps, factor(activity2$date), median)

mean
median

#median/total of the total number of steps taken per day
median(total)
mean(total)



#saving to file
jpeg("reproducible_research/RepData_PeerAssessment1/figure/plot.png",width=480,height=480)

plot(tapply(activity2$steps, factor(activity2$interval), mean),type = 'l',xlab = "5-minute interval (normalized)", ylab = "average number of steps taken",main=)

dev.off()

#the real max interval and the normalized one
which.max(tapply(activity2$steps, factor(activity2$interval), mean))


#Imputing missing values

missing_values <- activity[is.na(activity$steps),]
# the total number of missing values in the dataset

num_missing_values <- length(missing_values[,1])
#missing value <- the median(total steps of that day)

length(median[missing_values[,2]])

missing_values[,1] <- median[missing_values[,2]]
#if the value of an entire month is na we put 0 instead
missing_values[is.na(missing_values$steps),1] <- 0

new_data <- rbind(missing_values,activity2)


new_total <- tapply(new_data$steps, factor(new_data$date), sum)


#saving hist to file
jpeg("reproducible_research/RepData_PeerAssessment1/figure/hist2.png",width=480,height=480)

hist(new_total,xlim = c(0,25000),ylim = c(0,30),main="Total number of steps taken per day",xlab = "steps")

dev.off()


#the mean/total of the number of the  number of steps taken per day
mean_1 <- tapply(new_data$steps, factor(new_data$date), mean)
median_1 <- tapply(new_data$steps, factor(new_data$date), median)

mean_1
median_1

#median/total of the total number of steps taken per day
median(new_total)
mean(new_total)



#differences in activity patterns between weekdays and weekends
new_data[,2] <- factor(weekdays(as.Date(new_data$date)) %in% c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday'),levels=c(FALSE,TRUE),labels = c('Weekend','Weekday'))
weekdays_1 <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')

#saving to file
jpeg("reproducible_research/RepData_PeerAssessment1/figure/weekday-weekend.png",width=480,height=480)

qplot(interval,steps,data=new_data,stat="summary",fun.y="mean",geom = 'line',facets =date ~ .)

dev.off()
rm(list=ls())