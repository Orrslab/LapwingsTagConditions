#Set the directory
rm(list=ls())
cat("\014")
###Lines that need to change to the current the date: 53,173,174,176,177, 229,230.
setwd('D:/OrrS4/Desktop/Miki/SQL')
# Load libraries 
library(reshape2)
library(ggpubr)# package needed to the ATLAS package (for plotting)
library(htmltools) # to add "pop-ups" to leaflet maps
library(dbscan) # clustering algorithm
library(toolsForAtlas)
library(RMySQL)
library(ggplot2)
library(leaflet)
library(sp)
library(rgdal)
library(lubridate)
library(dplyr)
library(plyr)
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
dbListFields(dbc, 'DETECTIONS')
dbListFields(dbc, 'LOCALIZATIONS')

# --- Set start & end time and convert to ATLAS time -----------------------

Start_Time_Str ='2020-03-31 06:00:00' # define start time
Start_Time_Str_Temp <- as.character.Date(Start_Time_Str) 
ATLAS_Start_Time<-as.numeric(as.POSIXct(Start_Time_Str_Temp,
                                        "%Y-%m-%d %H:%M:%S", tz="UTC"))*1000

End_Time_Str ='2020-07-28 06:00:00' # Need to change to corrent date
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
AllTagsDet <- list() #make an empty list for detections

for (i in 1:length(FullTag)) {
  query = paste('select TAG,TIME from DETECTIONS WHERE TAG=',FullTag[i],
                'AND TIME >=', ATLAS_Start_Time, 'AND TIME <=', ATLAS_End_Time)
  All_Data <- dbGetQuery(dbc,query)
  AllTagsDet[[i]] <- All_Data
}

AllTagsLoc <- list() #make an empty list for localizations

for (i in 1:length(FullTag)) {
  query = paste('select TAG,TIME,X,Y,Z,VARX,VARY,COVXY from LOCALIZATIONS WHERE TAG=',FullTag[i],
                'AND TIME >=', ATLAS_Start_Time, 'AND TIME <=', ATLAS_End_Time)
  All_Data <- dbGetQuery(dbc,query)
  AllTagsLoc[[i]] <- All_Data
}

 AllthetagsDet <- do.call(rbind.data.frame, AllTagsDet)
 AllthetagsLoc <- do.call(rbind.data.frame, AllTagsLoc)

 rm(AllTagsDet,AllTagsLoc,dbc)
 
head(AllthetagsLoc)

names(AllthetagsDet)[names(AllthetagsDet) == "TIME"] <- "Detection_Time"
names(AllthetagsLoc)[names(AllthetagsLoc) == "TIME"] <- "Location_Time"

# Make the locations as ITM
AllthetagsLoc <-convertSpatial.ITM2WGS84(AllthetagsLoc, xyColNames=c("X","Y"))
AllthetagsLoc <- as.data.frame(AllthetagsLoc)

AllthetagsDet$DateDetection<-as.POSIXct((AllthetagsDet$Detection_Time)/1000, tz="UTC", origin="1970-01-01")
AllthetagsLoc$DateLocation<-as.POSIXct((AllthetagsLoc$Location_Time)/1000, tz="UTC", origin="1970-01-01")

AllthetagsDet$TAG <- substr(AllthetagsDet$TAG, 10, 13)
AllthetagsLoc$TAG <- substr(AllthetagsLoc$TAG, 10, 13)

AllthetagsDet<-AllthetagsDet[order(AllthetagsDet$TAG,AllthetagsDet$Detection_Time),] #make sure data is sorted chronologically (per tag)
AllthetagsLoc<-AllthetagsLoc[order(AllthetagsLoc$TAG,AllthetagsLoc$Location_Time),] #make sure data is sorted chronologically (per tag)

taillistDet <- list() #empty list
taillistLoc <- list() #empty list

Lisoftags <- unique(AllthetagsDet$TAG)  #Create a list with only the tags

#For detection
for(i in 1:length(Lisoftags)) {
  d <- AllthetagsDet[AllthetagsDet$TAG==Lisoftags[i],]
  d2 <- tail(d, 1)
  d2 <- subset(d2, select=c("TAG", "DateDetection"))
  taillistDet[[i]] <- d2
  print(d2)
}
#For locations
for(i in 1:length(Lisoftags)) {
  d <- AllthetagsLoc[AllthetagsLoc$TAG==Lisoftags[i],]
  d2 <- tail(d, 1)
  d2 <- subset(d2,  select=c("TAG", "DateLocation", "LON", "LAT"))
  taillistLoc[[i]] <- d2
  print(d2)
}

Lastdet <- do.call(rbind.data.frame, taillistDet)
Lastloc <- do.call(rbind.data.frame, taillistLoc)

AllLastDetLoc <- merge(Lastdet, Lastloc, by = 'TAG')
AllLastDetLoc <- merge(AllLastDetLoc, ListOfStart, by = 'TAG')

AllLastDetLoc<-AllLastDetLoc[order(AllLastDetLoc$TAG),] #make sure data is sorted chronologically (per tag)
AllLastDetLoc$fulllast <- AllLastDetLoc$DateTime
AllLastDetLoc$fullstart <- paste(AllLastDetLoc$date_capture, AllLastDetLoc$start_hour, sep = " ")
str(AllLastDetLoc)
AllLastDetLoc$fullstart <- as.POSIXct(AllLastDetLoc$fullstart, format="%d/%m/%Y %H:%M:%S", tz="UTC")

head(AllLastDetLoc)
#Start_to_det
AllLastDetLoc$Start_To_Det <- as.numeric(difftime(AllLastDetLoc$DateDetection, AllLastDetLoc$fullstart, units = "days"))
AllLastDetLoc$Start_To_Det <- round(AllLastDetLoc$Start_To_Det, digits = 0)
#Start_to_loc
AllLastDetLoc$Start_To_Loc <- as.numeric(difftime(AllLastDetLoc$DateLocation, AllLastDetLoc$fullstart, units = "days"))
AllLastDetLoc$Start_To_Loc <- round(AllLastDetLoc$Start_To_Loc, digits = 0)
#Loc_to_det
AllLastDetLoc$Loc_To_det <- as.numeric(difftime(AllLastDetLoc$DateDetection, AllLastDetLoc$DateLocation, units = "days"))
AllLastDetLoc$Loc_To_det <- round(AllLastDetLoc$Loc_To_det, digits = 0)



#Order the data so it show what I want
AllLastDetLoc$End_Det <- substr(AllLastDetLoc$DateDetection, 0,10)
AllLastDetLoc$End_Loc <- substr(AllLastDetLoc$DateLocation, 0,10)

AllLastDetLoc$ProblemDetections<- NA
AllLastDetLoc$ProblemDetections[AllLastDetLoc$End_Det == "2020-07-28"] <- "Work" # Need to change according to the last wanted date
AllLastDetLoc$ProblemDetections[AllLastDetLoc$End_Loc == "2020-07-28"] <- "Work" # Need to change according to the last wanted date

AllLastDetLoc$ProblemDetections[AllLastDetLoc$End_Det == "2020-07-28" & AllLastDetLoc$End_Loc != "2020-07-28"] <- "No Location" # Need to change to corrent date
AllLastDetLoc$ProblemDetections[AllLastDetLoc$End_Det != "2020-07-28" & AllLastDetLoc$End_Loc != "2020-07-28"] <- "Dont work" # Need to change to corrent date

#Without Ofri
Workornot <- AllLastDetLoc[with(AllLastDetLoc, !((TAG >= 131 & TAG <= 135))), ] #Without Ofri
counts <- table(Workornot$ProblemDetections)

###########
ggplot(Workornot,
       aes(y = TAG)) +
  geom_point(aes(x = fullstart), 
            color = "Black",
            alpha = 1,
            size = 2) +
  labs(x = "Date", 
       y = "Tags",
       title = "Tags life spane") +
  theme_minimal() + 
  geom_point(aes( x = DateLocation)) +
  geom_segment(aes(x = fullstart,
                   y = TAG,
                   xend = DateDetection,
                   yend = TAG),
                 color = "Red",
               size = 1) +
  geom_segment(aes(x = fullstart,
                   y = TAG,
                   xend = DateLocation,
                   yend = TAG),
               color = "Black",
               size = 1) +
  theme(legend.position = "bottom") + 
  geom_point(aes(x = DateLocation, y = TAG), 
                                               color = "Red",
                                               alpha = 1,
                                               size = 1) + 
  geom_point(aes(x = DateDetection), 
             color = "Red",
             alpha = 1,
             size = 1) + geom_text(aes( x = DateDetection, label=Start_To_Det), vjust=-0.2, size=2.8)

head(Lastdet)
head(Lastloc)

AllLastDetLoc$End_Det <- na_if(AllLastDetLoc$End_Det, "2020-07-28") # Need to change to corrent date
AllLastDetLoc$End_Loc <- na_if(AllLastDetLoc$End_Loc, "2020-07-28") # Need to change to corrent date
AllLastDetLoc$Loc_To_det <- na_if(AllLastDetLoc$Loc_To_det, 0) # Need to change to corrent date

#Removing unwanted columns:

AllLastDetLoc <- subset(AllLastDetLoc, select = -c(FullTag, TAG2,date_capture,start_hour) )
names(AllLastDetLoc)[names(AllLastDetLoc) == "fullstart"] <- "Capture time"
names(AllLastDetLoc)[names(AllLastDetLoc) == "DateDetection"] <- "Last detection"
names(AllLastDetLoc)[names(AllLastDetLoc) == "DateLocation"] <- "Last Location"
names(AllLastDetLoc)[names(AllLastDetLoc) == "Start_To_Loc"] <- "Location days"
names(AllLastDetLoc)[names(AllLastDetLoc) == "Loc_To_det"] <- "Detection without location"
names(AllLastDetLoc)[names(AllLastDetLoc) == "Start_To_Det"] <- "Detection days"
names(AllLastDetLoc)[names(AllLastDetLoc) == "End_Det"] <- "Last detection date"
names(AllLastDetLoc)[names(AllLastDetLoc) == "End_Loc"] <- "Last location date"
names(AllLastDetLoc)[names(AllLastDetLoc) == "ProblemDetections"] <- "Problem Detections"

#Write as CSV:
write.csv(AllLastDetLoc, file = paste('TagsDecLoc', 'csv', sep = '.'))


Lastdet$Indent <- "Detection"
Lastdet$Time <- AllLastDetLoc$`Detection days`

Lastloc$Indent <- "Location"
Lastloc$Time <- AllLastDetLoc$`Location days`

names(Lastdet)[names(Lastdet) == "DateDetection"] <- "Last Time"
names(Lastloc)[names(Lastloc) == "DateLocation"] <- "Last Time"

barplots <- bind_rows(Lastloc, Lastdet)

#Order the data so it show what I want
barplots$`Last Time`<- substr(barplots$`Last Time`, 0,10)
barplots$ProblemDetections<- NA
barplots$ProblemDetections[barplots$`Last Time` == "2020-07-28"] <- "Work" # Need to change according to the last wanted date
barplots$ProblemDetections[barplots$`Last Time` != "2020-07-28"] <- "Dont Work" # Need to change according to the last wanted date

ggplot(barplots) + 
  geom_col(aes(x = reorder(TAG, -Time), y = Time, fill = ProblemDetections), size = 1) +
  geom_line(aes(x = reorder(TAG, -Time), y = Time, color=Indent), size = 1.5, group = 1)

ggplot(data=barplots, aes(x = reorder(TAG, -Time), y=Time , fill = ProblemDetections)) +
  geom_bar(stat="identity", position=position_dodge()) + scale_fill_brewer(palette="Blues")+
  geom_line(aes(x = reorder(TAG, -Time), y = Time, color=Indent), size = 1.5, group = 1) +
  geom_text(aes(label=Time), vjust=-0.3, size=3.5)+
  theme_minimal() + ggtitle("Life span of all battery") +
  xlab("Tags") + ylab("Life span (Days)") +ylim(0, 120) + labs(fill = "Tag Condition", color= " ")
