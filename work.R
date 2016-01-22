# 1 Code to read the dataset
library(data.table)
library(ggplot2)

df <- read.csv('./activity.csv')
DT <- data.table(df)
summary(DT)
str(DT)

# 2 Histogram of total steps by day
DT[, sum(steps), by=date]$V1
hist(x=DT[, sum(steps), by=date]$V1, xlab = "Steps by date", main = "Histogram fo Steps by Date")


# 3 Mean and median of steps per day
DT[, list(as.double(median(steps)), mean(steps)), by=date]


plot(DT[date == '2012-10-03',list(interval, steps)])
boxplot(DT[date == '2012-10-03',steps + 1])

mean(DT[date == '2012-10-03',steps])
median(DT[date == '2012-10-03',steps])


# Average activity pattern
# 4 Time series plot of average number of steps by day
DT[, mean(steps), by=interval]

plot(DT[, mean(steps, na.rm = TRUE), by=date]$V1, ylab = "Average steps", 
     xlab = "Day", main = "Average steps per day plot", type = "l")

# 5 Five  minutes interval that, on average, contains the maximum number of steps
sample(DT)
head(DT, 20)
DT[!is.na(steps) &&  order(mean(steps, na.rm = TRUE), decreasing = TRUE), mean(steps), by = interval]
DT2 <- DT[!is.na(steps)]

str(DT2)
DT2$interval <- factor(DT2$interval)
summary(DT2)

intervalDT <- DT2[, mean(steps), by = interval][order(V1, decreasing = TRUE),]
str(intervalDT)
plot(intervalDT, type="l", main="Average steps by interval across all days", ylab="average steps")

intervalDT[V1 == max(intervalDT$V1),interval]
# 835 interval
abline()

summary(intervalDT)

# IMputing missing values

# 1 number of NAs
DT[is.na(steps),.N]

# 2, 3 Strategy to imputing missing values: remove NA values
DT2 <- DT[!is.na(steps)]

# histogram, mean and median without missing values
hist(x=DT2[, sum(steps), by=date]$V1, xlab = "Steps by date", main = "Histogram fo Steps by Date whitout missing values")


# 3 Mean and median of steps per day
DT2[, list(as.double(median(steps)), mean(steps)), by=date]


# Are there differences activity patterns between weekday and weekend
?weekdays()
weekdays(as.Date("2012-10-07"))
DT2[, day.type := NULL]
DT2[, day.type := "weekday"]
DT2[weekdays(as.Date(date)) %in% c("sabado", "domingo"), day.type := "weekend"]
DT2$day.type <- factor(DT2$day.type)


DT2[weekdays(as.Date(date)) %in% c("sabado", "domingo"),]
DT2[day.type == "weekend",]

par(mfrow=c(2,1))
plot(DT2[day.type == "weekend", mean(steps), by = interval], type="l", main="Average steps by interval in weekend", ylab="average steps")
plot(DT2[day.type == "weekday", mean(steps), by = interval], type="l", main="Average steps by interval in weekday", ylab="average steps", type ="l")



