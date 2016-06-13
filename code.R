# Question 1
# read dataset into R
fileurl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
temp <- tempfile()
download.file(fileurl, temp) 
unzip(temp, list = T)
dataset1 <- read.table(unzip(temp, "activity.csv"), header = T, sep = ",", stringsAsFactors = F)
unlink(temp)

# To find the total steps per day, convert to data table and subset by date
# sum the steps in each subset. Use the dplyr package to do the grouping
# also get the mean and median of each group
library(data.table)
dt <- as.data.table(dataset1)
dt$date <- as.Date(dt$date)

# remove the NA's
dt2 <- na.omit(dt)

library(dplyr)
dtgroup <- group_by(dt2, date)
dtanalysis <- summarise(dtgroup, sum(steps), mean(steps), median(steps))

# check the column names and reset it to apprioprate names
names(dtanalysis)
setnames(dtanalysis, c("date", "sum(steps)"),
         c("date", "total_steps"))

# Question 2

# mean and median of steps derived from the total daily steps
firstmean <- mean(dtanalysis$total_steps)
firstmedian <- median(dtanalysis$total_steps)

# plot the histogram of the total daily steps
plot(dtanalysis$date, dtanalysis$total_steps, type = "h", lwd = 10,
     main = "Total Daily Steps", xlab = "Date", ylab = "Steps", col = 6)
hist(dtanalysis$total_steps, main = "Total Daily Steps", xlab = "Steps", 
     ylab = "Time Interval", col = 2)


# Question 3
# group by time interval and find thier averages
intvervalgroup <- group_by(dt2, interval)
dtinterval <- summarise(intvervalgroup, mean(steps))
plot(dtinterval$interval, dtinterval$`mean(steps)`, main = "Time Series of Average Steps (5 interval)",
     type = "l", xlab = "5-minute Interval", ylab = "Average Steps")

# set the interval as row names and get the corresponding value for the max mean
df <- as.data.frame(dtinterval)
row.names(df) <- dtinterval$interval
maxindex <- which.max(dtinterval$`mean(steps)`)
max5 <- rownames(df)[maxindex]

# Question 4
# load the mice package and view the pattern of the missing data
library(mice)
md.pattern(dataset1)

library(VIM)
aggr_plot <- aggr(dataset1, col=c('magenta','gray'), numbers=TRUE, sortVars=TRUE, 
                  labels=names(dataset1), cex.axis=.7, gap=3, 
                  ylab=c("Histogram of missing data","Pattern"))

# use the predictive mean matching method in imputing the missing data
tempData <- mice(dataset1,m=5,maxit=10,meth='pmm',seed=500)
summary(tempData)
completedData <- complete(tempData,2)

# To find the total steps per day, convert to data table and subset by date
# sum the steps in each subset. Use the dplyr package to do the grouping
completedData <- as.data.table(completedData)
comp <- group_by(dt, date)
ctotal <- summarise(comp, sum(steps))
cmean <- mean(ctotal$`sum(steps)`)
cmedian <- median(ctotal$`sum(steps)`)

# plot the histogram of the total daily steps after NA values are added
plot(ctotal$date, ctotal$`sum(steps)`, type = "h", lwd = 10,
     main = "Total Daily Steps", xlab = "Date", ylab = "Steps", col = 11)
hist(ctotal$`sum(steps)`, main = "Total Daily Steps", xlab = "Steps", 
     ylab = "Time Interval", col = 7)

# Question 5
# view date as weekdays
comp$date <- as.Date(comp$date)
comp$date <- weekdays(comp$date)

# create 2 subsets for weekday and weekend 
dtweekday <- comp[grep("Monday|Tuesday|Wednesday|Thursday|Friday", date, ignore.case =T)]
wkdy <- group_by(dtweekday, interval)
wkdymean <- summarise(wkdy, mean(steps))

dtweekend <- comp[grep("Saturday|Sunday", date, ignore.case = T)]
wknd <- group_by(dtweekend, interval)
wkndmean <- summarise(wknd, mean(steps))

# add the day_type column to both datasets
wkdymean <- data.table(wkdymean, day_type = "weekday")
wkndmean <- data.table(wkndmean, day_type = "weekend")

# row bind the 2 datasets to creat a new factor variable in the dataset with two levels - "weekday" and "weekend" 
dtgroup3 <- rbind(wkdymean, wkndmean)

# plot the graphs for 5-minute intervals

library(lattice)
wk <- xyplot(dtgroup3$`mean(steps)` ~ dtgroup3$interval | factor(dtgroup3$day_type),
             type = "l", main = "Average Steps per 5-minute Interval", 
             ylab = "Number of Steps", xlab = "Interval", layout = c(1,2))
print(wk)