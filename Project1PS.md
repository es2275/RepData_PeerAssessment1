---
title: "PS 1"
output:
  pdf_document: default
  html_document: default
---




setwd("~/Desktop")
rawdata <- read.csv(file = "activity.csv")
rawdata$date <- as.Date(rawdata$date)

totalsteps <- aggregate(rawdata$steps, list(rawdata$date), sum, na.rm = TRUE)
colnames(totalsteps) <- c("date", "steps")

hist(totalsteps$steps, main = "Total number of steps taken each day", col = "pink", xlab = "Number of Steps")

resultmean <- totalsteps %>%
  summarise(mean = mean(steps))

resultmedian <- totalsteps %>%
  summarise(median = median(steps))

resultmean
resultmedian

stepsinterval <- aggregate(steps ~ interval, rawdata, mean)

plot2 <- ggplot(data = stepsinterval, aes(x = interval, y = steps)) +
  geom_line() +
  ylab(expression('Average Number of Steps')) +
  ggtitle('Time Series Plot - Average Number of Steps Taken by Interval') 

print(plot2)

maxinterval <- stepsinterval[which.max(stepsinterval$steps), 1]
maxvalue <- stepsinterval[which.max(stepsinterval$steps), 2]

print(paste("Interval with maximum steps:", maxinterval))
print(paste("Maximum steps:", maxvalue))

# Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NA NAs)
nomissingvalues <- sum(is.na(rawdata$steps))

# Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
meaninterval <- rawdata %>%
  group_by(interval) %>%
  summarise(steps = mean(steps))

# Create a new dataset that is equal to the original dataset but with the missing data filled in.
imputed <- merge(rawdata, meaninterval, by = "interval", all.x = TRUE)
imputed$steps <- ifelse(is.na(imputed$steps.x), imputed$steps.y, imputed$steps.x)
imputed <- imputed[c("steps", "date", "interval")]
colnames(imputed) <- c("steps", "date", "interval")

stepsintervalimputed <- aggregate(steps ~ date, data = imputed, sum)
hist(stepsintervalimputed$steps, main = "Total number of steps taken each day", col = "pink", xlab = "Number of Steps")

meansteps2 <- mean(stepsintervalimputed$steps)
mediansteps2 <- median(stepsintervalimputed$steps)

print(paste("Mean steps:", meansteps2))
print(paste("Median steps:", mediansteps2))

# Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
imputed$noday <- wday(imputed$date)
imputed$daytype <- ifelse(imputed$noday %in% c(1, 7), 'weekend', 'weekday')

# Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.
stepsday <- aggregate(steps ~ interval + daytype, data = imputed, mean)

weekdaydata <- subset(stepsday, daytype == "weekday")
weekenddata <- subset(stepsday, daytype == "weekend")

# Create a time series plot
plot3 <- ggplot(stepsday, aes(x = interval, y = steps, color = daytype)) +
  geom_line() +
  labs(x = "Interval", y = "Average step") +
  ggtitle("Time series plot of the average number of steps taken") +
  scale_color_manual(values = c("weekday" = "blue", "weekend" = "red"))

print(plot3)

knitr::knit2html("Untitled2.Rmd")
