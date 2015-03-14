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
round(mean(totsteps))
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

compl <- complete.cases(act)
fulldata <- cbind(act,compl)
fulldatasplit <- split(fulldata,fulldata$compl)

##Fill all NA with the average of 
install.packages("Hmisc")
library(Hmisc)

#fulldata$imputed_steps <- with(fulldata, impute(steps, mean))
fulldata$steps <- with(fulldata, impute(steps, mean))

#Are there differences in activity patterns between weekdays and weekends?
act$day <- weekdays(act$date)
for (i in 1:nrow(act)) {
        if(act[i,]$day %in% c("Saturday","Sunday")) {
                act[i,]$day <-"Weekend"
        }
        else{
                act[i,]$day <- "Weekday"
        }
}

act_int <- split(act, act$interval)
#compl_act_int <- split(compl_act, compl_act$interval)
act_average <- sapply(act_int,function(x) mean(x$steps))
#int_average <- sapply(compl_act_int,function(x) mean(x$steps))

#plot(int_average,type="l",ylab="Average Steps",xlab="5-Minute Interval",main="5-Minute Time Series Plot")
act_average <- as.data.frame(act_average)
par(mfrow=c(1,1))  