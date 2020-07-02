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

End_Time_Str ='2020-07-01 06:00:00' # Need to change to corrent date
End_Time_Str_Temp <- as.character.Date(End_Time_Str)
ATLAS_End_Time<-as.numeric(as.POSIXct(End_Time_Str_Temp,
                                      "%Y-%m-%d %H:%M:%S", tz="UTC"))*1000 

# --- Set Tags ID-------------------------------------------------------------


#The list of tags with the beggining dates (from the captured data)
ListOfStart <- read.csv("TAG_dates.csv")

TagListing <- ListOfStart$TAG  #Create a list with only the tags
#Make the tag list as full tag name:

for (i in 1:length(TagListing)){
  d <- ListOfStart$TAG[i]
  if (d < 100){ 
    ListOfStart$FullTag[i] <- paste(9720060000, d, sep = "")
    ListOfStart$TAG2[i] <- paste(0, d, sep = "")
  } else { ListOfStart$FullTag[i] <- paste(972006000, d, sep = "")
  ListOfStart$TAG2[i] <- paste(d)}
}

ListOfStart$TAG <- ListOfStart$TAG2
FullTag <- ListOfStart$FullTag  #Create a list with only the tags
AllTags <- list() #make an empty list

for (i in 1:length(FullTag)) {
  query = paste('select TAG,TIME,X,Y,Z,VARX,VARY,COVXY from LOCALIZATIONS WHERE TAG=',FullTag[i],
                'AND TIME >=', ATLAS_Start_Time, 'AND TIME <=', ATLAS_End_Time)
  All_Data <- dbGetQuery(dbc,query)
  AllTags[[i]] <- All_Data
}

Allthetags <- do.call(rbind.data.frame, AllTags)
unique(Allthetags$TAG)
Allthetags$TAG <- substr(Allthetags$TAG, 10, 13)

Allthetags<-Allthetags[order(Allthetags$TAG,Allthetags$TIME),] #make sure data is sorted chronologically (per tag)


# create a dataframe with the starting time and date

head(Allthetags)
Allthetags<-addLocAttribute(Allthetags, locAttributs=c("locQuality")) # function to add attributes for time
head(Allthetags)

# I will work with date and dateTime to know the last date and time the tags were active

taillist <- list() #empty list

# Make the locations as ITM
Allthetags2<-convertSpatial.ITM2WGS84(Allthetags, xyColNames=c("X","Y"))
Allthetags2 <- as.data.frame(Allthetags2)

head(Allthetags2)

Lisoftags <- unique(Allthetags2$TAG)  #Create a list with only the tags

#Make a loop that will give me a list of all the last line in each tag

for(i in 1:length(Lisoftags)) {
  d <- Allthetags2[Allthetags2$TAG==Lisoftags[i],]
  d2 <- tail(d, 1)
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
names(AllDates)[names(AllDates) == "date"] <- "End_date"
AllDates$End_date <- as.character(AllDates$End_date)
AllDates <- subset(AllDates, select=-c(TAG2,FullTag))

#Now i want to know how much time the battery worked
AllDates$date_capture <- as.Date(factor(AllDates$date_capture), format = "%d/%m/%Y") #Change to date format
AllDates$End_date <- as.Date(AllDates$End_date) #Change to date format

str(AllDates)
AllDates$date_diff<-as.numeric(AllDates$End_date - AllDates$date_capture) #Make a new column as the differences in days
# start and end - need to make it as.numeric becouse the values it give me otherwise are characters

#Looking at the data
hist(AllDates$date_diff)
plot(density(AllDates$date_diff, na = T))

#working\not working tags

AllDates$DeadOrAlive<- NA

AllDates$DeadOrAlive[AllDates$date_diff >= 0] <- "Dead"
AllDates$DeadOrAlive[AllDates$End_date == "2020-07-01"] <- "Work" # Need to change according to the last wanted date

#Without Ofri
Workornot <- AllDates[with(AllDates, !((TAG >= 131 & TAG <= 135))), ] #Without Ofri
Workornot <- Workornot[1:(nrow(Workornot)-1),]
counts <- table(Workornot$DeadOrAlive)


ggplot(data=Workornot, aes(x = reorder(TAG, -date_diff), y=date_diff, fill = DeadOrAlive )) +
  geom_bar(stat="identity")+
  geom_text(aes(label=date_diff), vjust=-0.3, size=3.5)+
  theme_minimal() + ggtitle("Life span of all battery") +
  xlab("Tags") + ylab("Life span (Days)") +ylim(0, 100) + labs(fill = "Tag Condition")

table(Workornot$DeadOrAlive)

#See only dead tags:

AllDates$End_date <- na_if(AllDates$End_date, "2020-07-01") # Need to change to corrent date

#make graph with only working tags:

barplotfortags <- AllDates[complete.cases(AllDates$End_date), ]
barplotfortags2 <- barplotfortags[with(barplotfortags, !((TAG >= 131 & TAG <= 135))), ] #Without Ofri

# Graph to see the life span of dead tags
ggplot(data=barplotfortags2, aes(x = reorder(TAG, -date_diff), y=date_diff )) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=date_diff), vjust=-0.3, size=3.5)+
  theme_minimal() + ggtitle("Life span of dead battery") +
  xlab("Tags") + ylab("Life span (Days)")

#Write as CSV:
write.csv(AllDates, file = paste('TagsCondition', 'csv', sep = '.'))

