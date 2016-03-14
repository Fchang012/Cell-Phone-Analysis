library(plyr)
library(ggplot2)

# Cell Phone Bill Script Post Process Analysis
# This will take the seperated bills and produce a variety of analysis on the bills


# Functions ---------------------------------------------------------------

timeHisto <- function(DF){
  DF$Time <- strptime(DF$Time, format="%H:%M:%S")
  DF$Hour <-  as.numeric(format(DF$Time, format="%H"))
  
  theHistogram <- ggplot(data=DF) + 
    geom_histogram(aes(x=Hour), breaks=seq(0, 24, by=1),colour="white", fill="blue") +
    scale_x_discrete(labels=seq(0, 24, by=1))
  
  return(theHistogram)
}

timeSeriesPlot <- function(DataUseDay){
  thePlot <- ggplot(DataUseDay, aes(Date, KB, group=1)) +
    geom_smooth(method="loess", se=F) + geom_point() +
    labs(x="", y="Daily Total(KB)")
}

timeSeriesPlotCall <- function(callUseDay){
  thePlot <- ggplot(callUseDay, aes(Date, Min.Total, group=1)) +
    geom_smooth(method="loess", se=F) + geom_point() +
    labs(x="", y="Daily Total(Min)")
}

lastTwoMonths <- function(DF, month=NULL, year=NULL){
  DF$Date <- as.Date(DF$Date)
  DF$Month <- as.integer(format(DF$Date, "%m"))
  DF$Year <- as.integer(format(DF$Date, "%Y"))
  
  if(is.null(year)){
    year <- as.integer(max(DF$Year))
    DF <- DF[DF$Year==year,]
  }
  
  if(is.null(month)){
    month <- as.integer(max(DF$Month))
  }
  
  if(month==1){
    DF <- DF[((DF$Month==month & DF$Year==year)|(DF$Month==12 & DF$Year==year-1)),]
  } else {
    DF <- DF[((DF$Month==month & DF$Year==year)|(DF$Month==month-1 & DF$Year==year)),]
  }
  
  DF <- subset(DF, select = -c(Year, Month))
  
  return(DF)
}

arrangeWeek <- function(DF){
  DF$Day <- factor(DF$Day, levels(DF$Day)[c(2,6,7,5,1,3,4)])
  return(DF)
}

cBoxPlot <- function(DF){
  thePlot <- ggplot(DF, aes(Day,Min.Total, fill=Day)) +
    geom_boxplot() +
    scale_y_log10()
  return(thePlot)
}

dBoxPlot <- function(DF){
  thePlot <- ggplot(DF, aes(Day,KB, fill=Day)) +
    geom_boxplot() +
    scale_y_log10()
  return(thePlot)
}

#Not Useful, use for reference for scatterplots
sPlotCallData <- function(cDF, dDF){
  tempDF <- merge(cDF, dDF, by='Date')
  plot <- ggplot(tempDF, aes(x = Min.Total, y = KB)) +
    geom_point() +
    scale_x_log10() +
    scale_y_log10() +
    geom_smooth(method = "lm") +
    facet_wrap(~Day.x)
  return(plot)
}

#Setting WD
setwd('C:/Users/Frank/Desktop/Fooling Around With R/Cell Phone Analysis')

## Analysis of Phone One --------------------------------------------

mCallDF <- read.csv("./CodeSample/PhoneOne/mCallData.csv", sep=";")
mDataDF <- read.csv("./CodeSample/PhoneOne/mDataData.csv", sep=";")

#Reduce Data to Last 2 Months
mCallDF <- lastTwoMonths(mCallDF)
mDataDF <- lastTwoMonths(mDataDF)

#Arrange the days to M T W TH F SA SU
mCallDF <- arrangeWeek(mCallDF)
mDataDF <- arrangeWeek(mDataDF)

#Finding the most called number
sort(table(unlist(lapply(mCallDF$Number.Called, paste, collapse = " "))), decreasing=TRUE)

#Plotting Histogram of call times
timeHisto(mCallDF) + labs(title = "M Call Times")

#Analysis of M Call Usage
#Combine's day entries into 1 to find heavy usage dates
mCallUseDay <- ddply(mCallDF, .(Date, Day), summarise, Min.Total=abs(sum(Min)))

#Time Series Plot
timeSeriesPlotCall(mCallUseDay) + labs(title = "M Daily Call Totals for Call Usage")

#Boxplot by day
cBoxPlot(mCallUseDay) + labs(title = "M Daily Totals Sep by Day Boxplot (Y is log10 scaled)")
by(mCallUseDay$Min.Total, mCallUseDay$Day, summary)

#Analysis of M Data Usage that is not txt/pic msg for heavy use days
mDataUse <- mDataDF[mDataDF$Msg.KB!= 1,]
mDataUse$Msg.KB <- as.integer(as.character(mDataUse$Msg.KB))

#Combine's day entries into 1 to find heavy usage dates
mDataUseDay <- ddply(mDataUse, .(Date, Day), summarise, KB=abs(sum(Msg.KB)))

#Find the top 10 usage days
sort(mDataUseDay$KB, decreasing=TRUE)

#Time Series Plot
timeSeriesPlot(mDataUseDay) + labs(title = "M Daily Totals for Data Usage")

#Data Usage Statistic by day
dBoxPlot(mDataUseDay) + labs(title = "M Daily Data Totals Sep by Day Boxplot (Y is log10 scaled)")
by(mDataUseDay$KB, mDataUseDay$Day, summary)


##Analysis of Phone Two -------------------------------------------

dCallDF <- read.csv("./CodeSample/PhoneTwo/dCallData.csv", sep=";")
dDataDF <- read.csv("./CodeSample/PhoneTwo/dDataData.csv", sep=";")

#Reduce Data to Last 2 Months
dCallDF <- lastTwoMonths(dCallDF)
dDataDF <- lastTwoMonths(dDataDF)

#Arrange the days to M T W TH F SA SU
dCallDF <- arrangeWeek(dCallDF)
dDataDF <- arrangeWeek(dDataDF)

#Finding the most called number
sort(table(unlist(lapply(dCallDF$Number.Called, paste, collapse = " "))), decreasing=TRUE)

#Plotting Histogram of call times
timeHisto(dCallDF) + labs(title = "D Call Times")

#Analysis of D Call Usage
#Combine's day entries into 1 to find heavy usage dates
dCallUseDay <- ddply(dCallDF, .(Date, Day), summarise, Min.Total=abs(sum(Min)))

#Time Series Plot
timeSeriesPlotCall(dCallUseDay) + labs(title = "D Daily Totals for Call Usage")

#Boxplot by day
cBoxPlot(dCallUseDay) + labs(title = "D Daily Totals Sep by Day Boxplot (Y is log10 scaled)")
by(dCallUseDay$Min.Total, dCallUseDay$Day, summary)

#Analysis of D Data Usage that is not txt/pic msg for heavy use days
dDataUse <- dDataDF[dDataDF$Msg.KB!= 1,]
dDataUse$Msg.KB <- as.integer(as.character(dDataUse$Msg.KB))

#Combine's day entries into 1 to find heavy usage dates
dDataUseDay <- ddply(dDataUse, .(Date, Day), summarise, KB=abs(sum(Msg.KB)))

#Find the top 10 usage days
sort(dDataUseDay$KB, decreasing=TRUE)

#Time Series Plot
timeSeriesPlot(dDataUseDay) + labs(title = "D Daily Totals for Data Usage")

#Data Usage Statistic by day
dBoxPlot(dDataUseDay) + labs(title = "D Daily Data Totals Sep by Day Boxplot (Y is log10 scaled)")
by(dDataUseDay$KB, dDataUseDay$Day, summary)