library(data.table)
library(ggplot2)

df <- read.csv('./activity.csv')
DT <- data.table(df)
summary(DT)
str(DT)



DT[, .(as.double(median(steps, na.rm = TRUE)), mean(steps, na.rm = TRUE)), by=date]


plot(DT[date == '2012-10-03',.(interval, steps)])
boxplot(DT[date == '2012-10-03',steps + 1])

mean(DT[date == '2012-10-03',steps])
median(DT[date == '2012-10-03',steps])

DT[, sum(steps), by=date]$V1
hist(x=DT[, sum(steps), by=date]$V1)
