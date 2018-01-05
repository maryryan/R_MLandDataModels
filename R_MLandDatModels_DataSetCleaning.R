#########################
############################# R: MACHINE LEARNING AND DATA MODELS
############################# NY BEACHES DATA SET CREATION/COMBINATION
############################# MARY RYAN
############################# NICAR 2018
############################# MARCH 8-11
#########################

## GOAL: to see how increases in bacteria in water affects odds
## of NY beaches closing during June 1, 2016 - August 1, 2016

#### LOAD LIBRARIES ####
# install.packages('dplyr')
# install.packages('plyr')
# install.packages('lubridate')

library(dplyr)
library(plyr)
library(lubridate)

#### LOAD ORIGINAL FILES ####
## data originally obtained from EPA BEACON 2.0:
## https://watersgeo.epa.gov/beacon2/reports.html
wq <- read.csv('water_quality_report.csv', header=TRUE)
action <- read.csv('beach_actions_(advisories_and_closures)_-_detailed_report.csv',
                            header=TRUE)

#### WATER QUALITY DATASET ####
wq.trunc <- wq[,c("Beach.ID.Def..", "CountyName.Def..", 
                  "Start.Date.Def..", "CharacteristicName.Def..",
                  "Result.Value.Def..",
                 "Result.MeasureUnit")]

## making the observation date as common date format ##
wq.trunc$Start.Date.Def.. <- as.Date(wq.trunc$Start.Date.Def..,
                                     format='%m/%d/%y')
## restricting our dataset to June 1, 2016  - Aug 1, 2016 ##
wq.trunc <- wq.trunc[wq.trunc$Start.Date.Def.. %within% c(as.Date("2016-06-01", format="%Y-%m-%d") %--% as.Date("2016-08-01", format="%Y-%m-%d")), ]
colnames(wq.trunc)[1] <- "Beach.ID"

## make separate variables for "Enterococcus" & "Escherichia coli" measures ##
wq.trunc$ecoliValue <- ifelse(wq.trunc$CharacteristicName.Def..=="Escherichia coli",
                              wq.trunc$Result.Value.Def.., NA)
wq.trunc$ecoliUnit <- ifelse(wq.trunc$CharacteristicName.Def..=="Escherichia coli",
                             as.character(wq.trunc$Result.MeasureUnit), NA)
wq.trunc$EnterocValue <- ifelse(wq.trunc$CharacteristicName.Def..=="Enterococcus",
                           wq.trunc$Result.Value.Def.., NA)
wq.trunc$EnterocUnit <- ifelse(wq.trunc$CharacteristicName.Def..=="Enterococcus",
                                as.character(wq.trunc$Result.MeasureUnit), NA)

#### BEACH ACTION DATASET ####
action.trunc <- action[, c("County", "Beach.Id", "Beach.Status", "Action.Type",
                           "ActionStartDate","ActionEndDate",
                           "ActionDuration.Days", "Action.Reasons",
                           "Action.Possible.Source")]
## make start and end dates as common date format ##
action.trunc$ActionStartDate <- as.Date(action.trunc$ActionStartDate,
                                        format='%d-%b-%y')
action.trunc$ActionEndDate <- as.Date(action.trunc$ActionEndDate,
                                        format='%d-%b-%y')
## going to make events for every date in our range ##
action.list <- split(action.trunc, action.trunc$Beach.Id)
action.list.order <- lapply(action.list, function(df){
   df[order(df$ActionStartDate),]
})
## 62 days between Jun 1 - Aug 1
action.expanded.list <- lapply(action.list.order, function(df){
   Beach.Id <- rep(df[1,2], 62)
   County <- rep(df[1,1], 62)
   datesFull <- seq(as.Date("2016-06-01"), as.Date("2016-08-01"), by="days")
   actions.l <- list()
   reasons.l <- list()
   possibleSource.l <- list()
   
   for(i in 1:dim(df)[1]){
      actions.l[[i]] <- ifelse(datesFull %within% c(df[i,'ActionStartDate'] %--% df[i,'ActionEndDate']), 
                            df[i,"Action.Type"], NA)
      reasons.l[[i]] <- ifelse(datesFull %within% c(df[i,'ActionStartDate'] %--% df[i,'ActionEndDate']), 
                            df[i,"Action.Reasons"], NA)
      possibleSource.l[[i]] <- ifelse(datesFull %within% c(df[i,'ActionStartDate'] %--% df[i,'ActionEndDate']), 
                            df[i,"Action.Possible.Source"], NA)
   }
   actions <- do.call(cbind, actions.l)
   reasons <- do.call(cbind, reasons.l)
   possibleSource <- do.call(cbind, possibleSource.l)
   
   ## combine all interval date columns into one vector ##
   actions.v <- vector(,dim(actions)[1])
   reasons.v <- vector(,dim(reasons)[1])
   possibleSource.v <- vector(,dim(possibleSource)[1])
   for(i in 1:dim(actions)[2]){
      for(j in 1:dim(actions)[1]){
         actions.v[j] <- ifelse(is.na(actions[j,i])==FALSE,
                                actions[j,i], actions.v[j])
         reasons.v[j] <- ifelse(is.na(reasons[j,i])==FALSE,
                                reasons[j,i], reasons.v[j])
         possibleSource.v[j] <- ifelse(is.na(possibleSource[j,i])==FALSE,
                                possibleSource[j,i], possibleSource.v[j])
      }
   }
      
   set <- as.data.frame(cbind(Beach.Id, County, datesFull, actions.v,
                              reasons.v,possibleSource.v))
   set[,3] <- as.Date(set[,3], origin='1970-01-01')
   set[1:62,]
})
action.expanded <- ldply(action.expanded.list, data.frame)
action.expanded <- action.expanded[,-2]
colnames(action.expanded) <- c("Beach.ID", "County", "Date", "Action", "Reason", "PossibleSource")
counties <- cbind(1:12,levels(action.trunc$County))
colnames(counties) <- c("countyID", "County")
action.expanded.m <- merge(counties, action.expanded,
                           by.x="countyID", by.y="County", all=T)
action.expanded.m <- action.expanded.m[,-1]
action.expanded.m[,1:2] <- action.expanded.m[,c(2,1)]
colnames(action.expanded.m)[1:2] <- c("Beach.ID", "County")

#### ACTION LEVEL CODES ####
#1=closure
#2=contamination advisory
#3=rain advisory
## ACTION REASON LEVEL CODES ##
#1=Elev_bact
#2=model
#3=other
#4=rainfall
#5=sewage
## ACTION POSSIBLE SOURCE LEVEL CODES ##
#1=CSO
#2=Other
#3=Runoff
#4=Spetic
#5=SSO
#6=Storm
#7=Unknown
#8=wildlife

#### MERGE INTO ONE DATASET ####
## match on both beach ID and date ##
beaches <- merge(action.expanded.m, wq.trunc,
                 by.x=c('Beach.ID', 'Date', "County"),
                 by.y=c("Beach.ID", "Start.Date.Def..", "CountyName.Def.."),
                 all=TRUE)

colnames(beaches)[c(2,7:9)] <- c("CalendarDate", "MeasureType",
                                 "WQ.Value", "WQ.MeasureUnit")

## creating binary variables ##
beaches$closure <- ifelse(beaches$Action==1, 1, 0)
beaches$contamAd <- ifelse(beaches$Action==2, 1, 0)

beaches$Reason.Elev_bact <- ifelse(beaches$Reason==1, 1, 0)
beaches$Reason.Model <- ifelse(beaches$Reason==2, 1, 0)
   #action reason being 'model' means that a model predicted
   #it needed to beclosed
beaches$Reason.Other <- ifelse(beaches$Reason==3, 1, 0)
beaches$Reason.Rainfall <- ifelse(beaches$Reason==4, 1, 0)
beaches$Reason.Sewage <- ifelse(beaches$Reason==5, 1, 0)

beaches$Source.CSO <- ifelse(beaches$PossibleSource==1, 1, 0)
   #Combined sewer overflow
beaches$Source.Other <- ifelse(beaches$PossibleSource==2, 1, 0)
beaches$Source.Runoff <- ifelse(beaches$PossibleSource==3, 1, 0)
beaches$Source.Spetic <- ifelse(beaches$PossibleSource==4, 1, 0)
beaches$Source.SSO <- ifelse(beaches$PossibleSource==5, 1, 0)
   #Sanitary sewer overflow
beaches$Source.Storm <- ifelse(beaches$PossibleSource==6, 1, 0)
beaches$Source.Unknown <- ifelse(beaches$PossibleSource==7, 1, 0)
beaches$Source.Wildlife <- ifelse(beaches$PossibleSource==8, 1, 0)

## creating time variable as days since June 1 ##
beaches$daysSinceJune1 <- as.Date(beaches$CalendarDate, format='%Y-%m-%d') - as.Date('2016-06-01', format='%Y-%m-%d')

#### EXPORT FINAL DATASET ####
write.csv(beaches, 'beaches.csv')
