Section 1
---------

### Loading and preprocessing the data

Read dataset into R

    fileurl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
    temp <- tempfile()
    download.file(fileurl, temp)
    unzip(temp, list = T)

    ##           Name Length                Date
    ## 1 activity.csv 350829 2014-02-11 10:08:00

    dataset1 <- read.table(unzip(temp, "activity.csv"), header = T, sep = ",", stringsAsFactors = F)
    unlink(temp)

Section 2
---------

Next, to find the total steps per day

-   Load the data table package.
-   Convert to data table and convert the data column variables
    to dates.
-   Remove the NA's

<!-- -->

    library(data.table)
    dt <- as.data.table(dataset1)
    dt$date <- as.Date(dt$date)

    # remove the NA's
    dt2 <- na.omit(dt)

-   Load the dply package and use it to group by date and create
    the subset.
-   Summarise the steps each and save in a new dataset

<!-- -->

    library(dplyr)

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:data.table':
    ## 
    ##     between, last

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

    dtgroup <- group_by(dt2, date)
    dtanalysis <- summarise(dtgroup, sum(steps))

Check the column names and reset it to apprioprate names

    names(dtanalysis)

    ## [1] "date"       "sum(steps)"

    setnames(dtanalysis, c("date", "sum(steps)"), c("date", "total_steps"))

Plot the histogram of the total daily steps

    plot(dtanalysis$total_steps, type = "h", lwd = 10, main = "Total Daily Steps", 
        xlab = "Days", ylab = "Steps", col = 6)
    abline(h = mean(dtanalysis$total_steps), col = 2)
    abline(h = median(dtanalysis$total_steps), col = 3)

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-5-1.png)
\#\#\# What is mean total number of steps taken per day?

Finding the mean and median of steps derived from the total daily steps.

    firstmean <- mean(dtanalysis$total_steps)
    firstmedian <- median(dtanalysis$total_steps)

The mean is 1.076618910^{4} and the median is 10765.

### Section 3

Group by time interval and find thier averages

    intvervalgroup <- group_by(dt2, interval)
    dtinterval <- summarise(intvervalgroup, mean(steps))

### What is the average daily activity pattern?

Plot time series of average steps taken

    plot(dtinterval$interval, dtinterval$`mean(steps)`, main = "Time Series of Average Steps (5 interval)", 
        type = "l", xlab = "5-minute Interval", ylab = "Average Steps")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-8-1.png)

Set the interval as row names and get the corresponding value for the
max mean

    df <- as.data.frame(dtinterval)
    row.names(df) <- dtinterval$interval
    maxindex <- which.max(dtinterval$`mean(steps)`)
    max5 <- rownames(df)[maxindex]

The 5-minute interval with the highest average steps is 835.

Section 4
---------

### Imputing missing values

load the mice package and view the pattern of the missing data

    library(mice)

    ## Loading required package: Rcpp

    ## mice 2.25 2015-11-09

    md.pattern(dataset1)

    ## Warning in data.matrix(x): NAs introduced by coercion

    ##       interval steps  date      
    ## 15264        1     1     0     1
    ##  2304        1     0     0     2
    ##              0  2304 17568 19872

    library(VIM)

    ## Loading required package: colorspace

    ## Loading required package: grid

    ## VIM is ready to use. 
    ##  Since version 4.0.0 the GUI is in its own package VIMGUI.
    ## 
    ##           Please use the package to use the new (and old) GUI.

    ## Suggestions and bug-reports can be submitted at: https://github.com/alexkowa/VIM/issues

    ## 
    ## Attaching package: 'VIM'

    ## The following object is masked from 'package:datasets':
    ## 
    ##     sleep

    aggr_plot <- aggr(dataset1, col = c("magenta", "gray"), numbers = TRUE, sortVars = TRUE, 
        labels = names(dataset1), cex.axis = 0.7, gap = 3, ylab = c("Histogram of missing data", 
            "Pattern"))

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-10-1.png)

    ## 
    ##  Variables sorted by number of missings: 
    ##  Variable     Count
    ##     steps 0.1311475
    ##      date 0.0000000
    ##  interval 0.0000000

use the predictive mean matching method in imputing the missing data

    tempData <- mice(dataset1, m = 5, maxit = 10, meth = "pmm", seed = 500)

    ## 
    ##  iter imp variable
    ##   1   1  steps
    ##   1   2  steps
    ##   1   3  steps
    ##   1   4  steps
    ##   1   5  steps
    ##   2   1  steps
    ##   2   2  steps
    ##   2   3  steps
    ##   2   4  steps
    ##   2   5  steps
    ##   3   1  steps
    ##   3   2  steps
    ##   3   3  steps
    ##   3   4  steps
    ##   3   5  steps
    ##   4   1  steps
    ##   4   2  steps
    ##   4   3  steps
    ##   4   4  steps
    ##   4   5  steps
    ##   5   1  steps
    ##   5   2  steps
    ##   5   3  steps
    ##   5   4  steps
    ##   5   5  steps
    ##   6   1  steps
    ##   6   2  steps
    ##   6   3  steps
    ##   6   4  steps
    ##   6   5  steps
    ##   7   1  steps
    ##   7   2  steps
    ##   7   3  steps
    ##   7   4  steps
    ##   7   5  steps
    ##   8   1  steps
    ##   8   2  steps
    ##   8   3  steps
    ##   8   4  steps
    ##   8   5  steps
    ##   9   1  steps
    ##   9   2  steps
    ##   9   3  steps
    ##   9   4  steps
    ##   9   5  steps
    ##   10   1  steps
    ##   10   2  steps
    ##   10   3  steps
    ##   10   4  steps
    ##   10   5  steps

    summary(tempData)

    ## Multiply imputed data set
    ## Call:
    ## mice(data = dataset1, m = 5, method = "pmm", maxit = 10, seed = 500)
    ## Number of multiple imputations:  5
    ## Missing cells per column:
    ##    steps     date interval 
    ##     2304        0        0 
    ## Imputation methods:
    ##    steps     date interval 
    ##    "pmm"    "pmm"    "pmm" 
    ## VisitSequence:
    ## steps 
    ##     1 
    ## PredictorMatrix:
    ##          steps date interval
    ## steps        0    0        1
    ## date         0    0        0
    ## interval     0    0        0
    ## Random generator seed value:  500

    completedData <- complete(tempData, 2)

To find the total steps per day, convert to data table and subset by
date. Then sum the steps in each subset. Use the dplyr package to do the
grouping.

    completedData <- as.data.table(completedData)
    comp <- group_by(completedData, date)
    ctotal <- summarise(comp, sum(steps))
    cmean <- mean(ctotal$`sum(steps)`)
    cmedian <- median(ctotal$`sum(steps)`)

After inputing the missing values, the new mean is 1.146654110^{4} and
the median is 11458.

There is a difference between the new mean and the old mean because when
the first one was calculated, the NA rows were omitted but the is no
difference in the median. Hence there were fewer observations to work
with but by replacing the NA's using the predictive mean matching
method, it the number of observations increased and the total daily
averages silghtly, resulting in a lower mean.

plot the histogram of the total daily steps after NA values are added.

    plot(ctotal$`sum(steps)`, type = "h", lwd = 7, main = "Total Daily Steps", xlab = "Days", 
        ylab = "Steps", col = 11)

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-13-1.png)

Section 5
---------

View date as weekdays

    comp$date <- as.Date(comp$date)
    comp$date <- weekdays(comp$date)

Create 2 subsets for weekday and weekend

    dtweekday <- comp[grep("Monday|Tuesday|Wednesday|Thursday|Friday", date, ignore.case = T)]
    wkdy <- group_by(dtweekday, interval)
    wkdymean <- summarise(wkdy, mean(steps))

    dtweekend <- comp[grep("Saturday|Sunday", date, ignore.case = T)]
    wknd <- group_by(dtweekend, interval)
    wkndmean <- summarise(wknd, mean(steps))

Add the day\_type column to both datasets

    wkdymean <- data.table(wkdymean, day_type = "weekday")
    wkndmean <- data.table(wkndmean, day_type = "weekend")

Row bind the 2 datasets to creat a new factor variable in the dataset
with two levels - "weekday" and "weekend"

    dtgroup3 <- rbind(wkdymean, wkndmean)

Use the weekday and weekend datasets to plot the graphs for 5-minute
intervals

    library(lattice)
    wk <- xyplot(dtgroup3$`mean(steps)` ~ dtgroup3$interval | factor(dtgroup3$day_type), 
        type = "l", main = "Average Steps per 5-minute Interval", ylab = "Number of Steps", 
        xlab = "Interval", layout = c(1, 2))
    print(wk)

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-18-1.png)

### Are there differences in activity patterns between weekdays and weekends?

From this panel plot, it is clear that the highest mean steps occurs on
the weekdays but the weekend has the higher median steps.
