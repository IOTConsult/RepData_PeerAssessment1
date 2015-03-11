##Loading and preprocessing the data
##Set Working dir
setwd ("\\\\UPLCI084736/USERS$/gjf510/MyCloudDrive/Documents/Coursera/RepData_PeerAssessment1")
##Set local language to English (to use English Names)
Sys.setlocale("LC_TIME", "English")  
act <- read.csv("activity.csv", header=TRUE, sep=",", skip = 0)
##Process the data
##Convert date to a Date class
act$date <- as.Date(act$date)


##Filter data where complete cases = TRUE
compl_act <- subset(act,complete.cases(act)==TRUE)
##Group by day
compl_act_day <- split(compl_act, compl_act$date)
#What is the total number of steps taken per day?
totsteps <- sapply(compl_act_day,function(x) sum(x$steps))
##Create a Histogram of the total number of steps during each day
hist(totsteps, main="Total steps per day",xlab="Total steps", ylab="Frequency")
#What is mean total number of steps taken per day?
mean(totsteps)
median(totsteps)



#What is the average daily activity pattern?
compl_act <- subset(act,complete.cases(act)==TRUE)
##Split by interval
compl_act_int <- split(compl_act, compl_act$interval)
##Create Average
int_average <- sapply(compl_act_int,function(x) mean(x$steps))
##Create the plot
plot(int_average,type="l",ylab="Average Steps",xlab="5-Minute Interval",main="5-Minute Time Series Plot")
##Print the interval with the max steps
names(which.max(int_average))

#Imputing missing values
##Count incomplete cases
compl <- !complete.cases(act)
sum(compl)

#Are there differences in activity patterns between weekdays and weekends?
