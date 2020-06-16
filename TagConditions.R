#Set the directory

setwd('D:/OrrS4/Desktop/Miki/SQL')

# Load libraries 
library(toolsForAtlas)
library(RSQLite)
library(RMySQL)
library(ggplot2)
library(RSQLite)
library(leaflet)
library(sp)
library(rgdal)
library(lubridate)
library(dplyr)
library(RSQLite)
library(shiny)
library(dismo)
library(readxl)
library(tictoc)
source("func\\visualMaps.R")    # my functions for maps
source("func\\Movement_Metrics_Functions.R")   # my enhancements for ADP
source("func/SplitNights.R")# helper functions for time-segmentation and data filtering

# Functions to interact with databases
# --- Database Connection ---------------------------------------------------
dbc <- dbConnect(RMySQL::MySQL(),
                 user = 'roatlasharod',            # username 
                 password = 'roatlasHarodOrr5678#',# password
                 host = '132.66.79.21',            # host ip address
                 port=5900,                        # port Number
                 dbname='harod')                   # name of data base


# --- Examine the tables contained in the database -------------------------
dbListTables(dbc)           

# --- Examine the names of the columns in a table --------------------------
dbListFields(dbc, 'LOCALIZATIONS')

# --- Set start & end time and convert to ATLAS time -----------------------

Start_Time_Str ='2020-03-31 06:00:00' # define start time
Start_Time_Str_Temp <- as.character.Date(Start_Time_Str) 
ATLAS_Start_Time<-as.numeric(as.POSIXct(Start_Time_Str_Temp,
                                        "%Y-%m-%d %H:%M:%S", tz="UTC"))*1000
End_Time_Str ='2020-06-10 06:00:00' # define end time
End_Time_Str_Temp <- as.character.Date(End_Time_Str)
ATLAS_End_Time<-as.numeric(as.POSIXct(End_Time_Str_Temp,
                                      "%Y-%m-%d %H:%M:%S", tz="UTC"))*1000

# --- Set Tags ID-------------------------------------------------------------


#The list of tags with the beggining dates (from the captured data)
ListOfStart <- read.csv("TAG_dates.csv")
TagListing <- ListOfStart$TAG #Create a list with only the tags

# I will take all the data from this time period - this include tags i dont need and will remove from my list
query = paste('select TAG,TIME,X,Y,Z,VARX,VARY,COVXY from LOCALIZATIONS WHERE TIME >=',ATLAS_Start_Time,
              'AND TIME <=', ATLAS_End_Time)
All_Data = dbGetQuery(dbc,query)
All_Data$TAG <- substr(All_Data$TAG, 10, 13)


#Removing unwanted tags
TagListing # The tags I need
unique(All_Data2$TAG) # The tags I have
All_Data$TAG <- as.numeric(All_Data$TAG)
#Removing unwanted tags
All_Data2 <- All_Data[!(All_Data$TAG < 45),] 
All_Data2 <- All_Data2[!(All_Data2$TAG > 90 & All_Data2$TAG < 130 ),] 
All_Data2 <- All_Data2[!(All_Data2$TAG == 53),] 
All_Data2 <- All_Data2[!(All_Data2$TAG >= 136 & All_Data2$TAG <= 155 ),] 
All_Data2 <- All_Data2[!(All_Data2$TAG >= 198 & All_Data2$TAG <= 200 ),] 

All_Data2<-All_Data2[order(All_Data2$TAG,All_Data2$TIME),] #make sure data is sorted chronologically (per tag)


# create a dataframe with the starting time and date

head(All_Data2)
All_Data2<-addLocAttribute(All_Data2, locAttributs=c("distanceSpeed", "locQuality")) # function to add attributes for each pair of 
head(All_Data2)

# I will work with date and dateTime to know the last date and time the tags were active

taillist <- list() #empty list

# Make the locations as ITM
All_Data3<-convertSpatial.ITM2WGS84(All_Data2, xyColNames=c("X","Y"))
All_Data3 <- as.data.frame(All_Data3)

head(All_Data3)
TagListing # the list of tags

#Make a loop that will give me a list of all the last line in each tag

for(i in 1:length(TagListing)) {
  d <- All_Data3[All_Data3$TAG==TagListing[i],]
  d2 <- tail(d, 1)
  #d2 <- d2[!(d2$date == "2020-06-10"),]
  d2 <- subset(d2, select=c("TAG", "dateTime","date", "LON", "LAT"))
  taillist[[i]] <- d2
  print(d2)
}

#Make the list as data frame
LastTags <- do.call(rbind.data.frame, taillist)
#Merge them together:
AllDates <- merge(ListOfStart, LastTags, by = 'TAG')

#Order the data so it show what I want
AllDates$end_hour <- substr(AllDates$dateTime, 12,19)
AllDates<-AllDates[,-which(colnames(AllDates) %in% "dateTime")]
AllDates$date <- as.character(AllDates$date)
AllDates$date <- na_if(AllDates$date, "2020-06-10")

write.csv(AllDates, file = paste('TagsCondition', 'csv', sep = '.'))

