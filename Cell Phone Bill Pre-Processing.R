library(plyr)
library(ggplot2)
# 
# Cell Phone Bill Script Pre-processing
# This will seperate the ATT bill raw csv dump into each accounts


# Functions ---------------------------------------------------------------

##Extracts only data and owner phone number
csvReadIn <- function(csvFile){
  day <- "Monday|Tuesday|Wednesday|Thursday|Friday|Saturday|Sunday"
  rawInput <- readLines(csvFile)
  outPut <- grep("^.*(day).*$|Call Detail.*$|Data Detail.*$", rawInput, value=TRUE)
  return(outPut)
}

##Reads in all the files in raw form and makes 1 master list
readAllIn <- function(paths){
  dfList <- list()
  for (i in 1:length(paths)){
    assign(paste("df", i, sep=""), csvReadIn(paths[i]))
    dfList[i] <- paste("df", i, sep="")
  }
  
  largeList <- list()
  
  for(j in dfList){
    largeList <- append(largeList, unlist(get(j)))
  }
  
  largeList[grep('Total', largeList)] <- NULL
  largeList <- unlist(largeList)
  largeList <- read.csv(text = largeList, header = FALSE, stringsAsFactors = FALSE)
  largeList[12:13] <- list(NULL)
  
  return(largeList)
}

##Function Used in sepData
relabelCol <- function(type, DFInput){
  if(type=="c"){
    colnames(DFInput) <- c('Item','Day','Date','Time','Number Called','Call To','Min','Rate Code','Rate Pd','Feature','Airtime Charge')
  }
  else if(type=="d"){
    colnames(DFInput) <- c('Item','Day','Date','Time','To/From','Type','Msg.KB','Rate Code','Rate Pd','Feature','In/Out')
  }
  return(DFInput)
}

##Separates out the data by owner and writes the cleaned data into a csv file for quicker analysis later on
sepData <- function(rawMasterList){
  
  callIndex <- which(rawMasterList$V1 == "Call Detail")
  dataIndex <- which(rawMasterList$V1 == "Data Detail")
  
  ## Separating out Call Data
  mCDF <- data.frame()
  dCDF <- data.frame()
  
  for(i in 1:length(callIndex)){
    if(rawMasterList$V2[callIndex[i]] == "**PHONE NUMBER 1 REPLACE**"){
      mCDF <- rbind(mCDF, rawMasterList[((callIndex[i]+1):(dataIndex[i]-1)),])
    }
    else if(rawMasterList$V2[callIndex[i]] == "**PHONE NUMBER 2 REPLACE**"){
      dCDF <- rbind(dCDF, rawMasterList[((callIndex[i]+1):(dataIndex[i]-1)),])
    }
  }
  
  ## Separating out Data Data
  mDDF <- data.frame()
  dDDF <- data.frame()
  
  for(i in 1:length(dataIndex)){
    if(i != length(dataIndex)){
      if(rawMasterList$V2[dataIndex[i]] == "**PHONE NUMBER 1 REPLACE**"){
        mDDF <- rbind(mDDF, rawMasterList[((dataIndex[i]+1):(callIndex[i+1]-1)),])
      }
      else if(rawMasterList$V2[callIndex[i]] == "**PHONE NUMBER 2 REPLACE**"){
        dDDF <- rbind(dDDF, rawMasterList[((dataIndex[i]+1):(callIndex[i+1]-1)),])
      }
    }
    else{
      if(rawMasterList$V2[dataIndex[i]] == "**PHONE NUMBER 1 REPLACE**"){
        mDDF <- rbind(mDDF, rawMasterList[((dataIndex[i]+1):nrow(rawMasterList)),])
      }
      else if(rawMasterList$V2[dataIndex[i]] == "**PHONE NUMBER 2 REPLACE**"){
        dDDF <- rbind(dDDF, rawMasterList[((dataIndex[i]+1):nrow(rawMasterList)),])
      }
    }
  }
  
  
  
  mCDF <- relabelCol(type='c', mCDF)
  dCDF <- relabelCol(type='c', dCDF)
  
  mDDF <- relabelCol(type='d', mDDF)
  dDDF <- relabelCol(type='d', dDDF)
  
  mCDF <- subset(mCDF, select = -c(Item))
  dCDF <- subset(dCDF, select = -c(Item))
  
  mDDF <- subset(mDDF, select = -c(Item))
  dDDF <- subset(dDDF, select = -c(Item))
  
  mCDF$Date <- as.Date(mCDF$Date, "%m/%d/%Y")
  dCDF$Date <- as.Date(dCDF$Date, "%m/%d/%Y")
  
  mDDF$Date <- as.Date(mDDF$Date, "%m/%d/%Y")
  dDDF$Date <- as.Date(dDDF$Date, "%m/%d/%Y")
  
  mCDF$Time <- format(strptime(mCDF$Time, "%I:%M %p"), format="%H:%M:%S")
  dCDF$Time <- format(strptime(dCDF$Time, "%I:%M %p"), format="%H:%M:%S")
  
  mDDF$Time <- format(strptime(mDDF$Time, "%I:%M %p"), format="%H:%M:%S")
  dDDF$Time <- format(strptime(dDDF$Time, "%I:%M %p"), format="%H:%M:%S")
  
  mCDF <- mCDF[order(mCDF$Date),]
  dCDF <- dCDF[order(dCDF$Date),]
  
  mDDF <- mDDF[order(mDDF$Date),]
  dDDF <- dDDF[order(dDDF$Date),]
  
  mDDF$Msg.KB <- gsub('[a-zA-Z]', '', mDDF$Msg.KB)
  dDDF$Msg.KB <- gsub('[a-zA-Z]', '', dDDF$Msg.KB)
  
  mDDF$Msg.KB <- gsub(',', '', mDDF$Msg.KB)
  dDDF$Msg.KB <- gsub(',', '', dDDF$Msg.KB)
  
  write.table(mCDF, "./CodeSample/PhoneOne/mCallData.csv", sep=";", row.names=FALSE)
  write.table(mDDF, "./CodeSample/PhoneOne/mDataData.csv", sep=";", row.names=FALSE)
  
  write.table(dCDF, "./CodeSample/PhoneTwo/dCallData.csv", sep=";", row.names=FALSE)
  write.table(dDDF, "./CodeSample/PhoneTwo/dDataData.csv", sep=";", row.names=FALSE)
}

## Pre-Processing to make a master list and then separate them into --------
setwd('C:/Users/Frank/Desktop/Fooling Around With R/Cell Phone Analysis/Raw Data Dump')
paths <- dir( pattern = "\\.csv$")
names(paths) <- basename(paths)

## Create a master list
rawMasterList <- readAllIn(paths)

## Separating out the call data and owners and creating their own CSV file for quicker analysis later on
setwd('C:/Users/Frank/Desktop/Fooling Around With R/Cell Phone Analysis')
sepData(rawMasterList)
