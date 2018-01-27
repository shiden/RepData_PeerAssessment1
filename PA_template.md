knitr::opts_chunk$set(echo = TRUE, cache=TRUE)

#Reproducible Research: Project 1
#================================


#### load Libraries

   ```{r, echo = TRUE}
         library(ggplot2)
         library(plyr)
         library(dplyr)
         library(lubridate)
   ```
 
##Code for reading in the dataset and/or processing the data
  - unzip file and load the data

    ```{r, echo = TRUE}

    if (!file.exists('activity.csv')){
       unzip("activity.zip", exdir = getwd())
    }

     activity <- read.csv("activity.csv", stringsAsFactors=FALSE)
     activity <- as.data.frame(activity)

      head(activity)

      ```

 
### Histogram of the total number of steps taken each day

```{r, echo = TRUE}

total_steps <- with(activity, tapply(steps, date, sum, na.rm = TRUE))

png("numsteps.png",bg="white")
graph <- barplot(total_steps, main="Total no/ steps taken ea day", col ="slateblue2", xlab="Date", ylab="steps")

graph
print(graph)
dev.off()

```



###Mean and median number of steps taken each day

```{r, echo = TRUE}
    mean(total_steps)
   median(total_steps)


```

### Time series plot of the average number of steps taken

```{r, echo = TRUE}
average <- 
with(na.omit(activity), tapply(steps, interval, mean))

######head(average)

png("average.png",bg="white")

graph <- plot(average, type="s", main="Average steps taken",xlab = "Minutes", col = "dark blue")

print(graph)
dev.off()

```


### The 5-minute interval that, on average, contains the maximum number of steps


```{r, echo = TRUE}
max_5m_interval <- average[which(average == max(average))]

max_5m_interval

```


## Code to describe and show a strategy for imputing missing data

```{r, echo = TRUE}

   unique_interval <- unique(activity$interval)

NAint <-  activity[is.na(activity),3]
NAsteps <- activity[is.na(activity),1]

for (j in 1:2304) {
       for (i in 1:288){
              if (NAint[j] == unique_interval[i])
                     NAsteps[j] <- average[i]
       
       }
}

NAindex <- is.na(activity$steps)
activity$steps<- replace(activity$steps,NAindex, NAsteps)

###### confirm
head(activity)

```


### Histogram of the total number of steps taken each day after missing values are imputed

```{r, echo = TRUE}

new_total_steps <- with(activity, tapply(steps, date, sum))

png("eachday.png",bg="white")

graph <- barplot(new_total_steps, main="Total no/ steps taken ea day", col ="green", xlab="Date", ylab="steps")

print(graph)
dev.off()


```

### Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

```{r, echo = TRUE}

## add weekday/weekend to table
is_weekday <-function(date){
        if(wday(date)%in%c(1,7)) result<-"weekend"
        else
                result<-"weekday"
        result
}

activitya <- mutate(activity,date=ymd(date)) %>% mutate(day=sapply(date,is_weekday))

average <- aggregate(steps ~ interval + day, data=activitya, mean)

head(average)

png("weekend.png",bg="white")

graph <- ggplot(average, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) + xlab("5-minute interval") + ylab("Number of steps")

print(graph)
dev.off()

```



