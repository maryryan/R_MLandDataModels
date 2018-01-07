######################
########################## R: MACHINE LEARNING AND DATA MODELS
########################## TEXAS MEDIAN HOUSING PRICE AND EDUCATIONAL ATTAINMENT
########################## DATASET CREATION/COMBINATION
########################## MARY RYAN
########################## NICAR 2018
########################## MARCH 8-11
######################

## GOAL: see how percent of college grads in a county
## affects median housing price

#### LOAD LIBRARIES ####
# install.packages('dplyr')
# install.packages('plyr')
# install.packages('stringr')

library(dplyr)
library(plyr)
library(stringr)

#### LOAD ORIGINAL FILES ####
## Housing price data originally obtained from American Community Survey
## Table S2506
house09 <- read.csv('TexMedHomeValue_09_S2506/ACS_09_5YR_S2506_with_ann.csv',
                    header=T)
house10 <- read.csv('TexMedHomeValue_10_S2506/ACS_10_5YR_S2506_with_ann.csv',
                    header=T)
house11 <- read.csv('TexMedHomeValue_11_S2506/ACS_11_5YR_S2506_with_ann.csv',
                    header=T)
house12 <- read.csv('TexMedHomeValue_12_S2506/ACS_12_5YR_S2506_with_ann.csv',
                    header=T)
house13 <- read.csv('TexMedHomeValue_13_S2506/ACS_13_5YR_S2506_with_ann.csv',
                    header=T)
house14 <- read.csv('TexMedHomeValue_14_S2506/ACS_14_5YR_S2506_with_ann.csv',
                    header=T)
house15 <- read.csv('TexMedHomeValue_15_S2506/ACS_15_5YR_S2506_with_ann.csv',
                    header=T)
house16 <- read.csv('TexMedHomeValue_16_S2506/ACS_16_5YR_S2506_with_ann.csv',
                    header=T)

## Educational attainment data originally obtained from American Community
## Survey
## Table S1501
edu09 <- read.csv('TexEduAttain_09_S1501/ACS_09_1YR_S1501_with_ann.csv',
                  header=T)
edu10 <- read.csv('TexEduAttain_10_S1501/ACS_10_1YR_S1501_with_ann.csv',
                  header=T)
edu11 <- read.csv('TexEduAttain_11_S1501/ACS_11_1YR_S1501_with_ann.csv',
                  header=T)
edu12 <- read.csv('TexEduAttain_12_S1501/ACS_12_1YR_S1501_with_ann.csv',
                  header=T)
edu13 <- read.csv('TexEduAttain_13_S1501/ACS_13_1YR_S1501_with_ann.csv',
                  header=T)
edu14 <- read.csv('TexEduAttain_14_S1501/ACS_14_1YR_S1501_with_ann.csv',
                  header=T)
edu15 <- read.csv('TexEduAttain_15_S1501/ACS_15_1YR_S1501_with_ann.csv',
                  header=T)
edu16 <- read.csv('TexEduAttain_16_S1501/ACS_16_1YR_S1501_with_ann.csv',
                  header=T)

#### HOUSING PRICE DATASET ####
## just want county code, county name, and median price (w/ mortgage) ##
house09.trunc <- house09[,c(2,3,20)]
house10.trunc <- house10[,c(2,3,20)]
house11.trunc <- house11[,c(2,3,20)]
house12.trunc <- house12[,c(2,3,20)]
house13.trunc <- house13[,c(2,3,20)]
house14.trunc <- house14[,c(2,3,20)]
house15.trunc <- house15[,c(2,3,20)]
house16.trunc <- house16[,c(2,3,20)]

## change column names to something interpretable ##
houseNames <- c("countyID", "countyName", "Median")
houseDataSets.list <- list(house09.trunc, house10.trunc, house11.trunc,
                           house12.trunc, house13.trunc, house14.trunc,
                           house15.trunc, house16.trunc)
houseDataSets.list <- lapply(houseDataSets.list, function(df){
   colnames(df) <- houseNames
   ## getting rid of the descriptive line ##
   df <- df[-1,]
   ## getting rid of the ', Texas' in county name ##
   df$countyName <- gsub(", Texas", "", df$countyName)
   df
})

## change list of dataframes to one dataframe ##
house <- ldply(houseDataSets.list, as.data.frame)
house$year <- c( rep(2009, (dim(house09.trunc)[1] - 1)),
                rep(2010, (dim(house10.trunc)[1] - 1)), 
                rep(2011, (dim(house11.trunc)[1] - 1)),
                rep(2012, (dim(house12.trunc)[1] - 1)), 
                rep(2013, (dim(house13.trunc)[1] - 1)),
                rep(2014, (dim(house14.trunc)[1] - 1)), 
                rep(2015, (dim(house15.trunc)[1] - 1)),
                rep(2016, (dim(house16.trunc)[1] - 1)) )
house$yrsSince2009 <- house$year - 2009

## rearrange columns ##
house <- house[,c(1,2,4,5,3)]

#### EDUCATIONAL ATTAINMENT DATASET ####
## just want county code, county name, pop total, % w/ at least bachelors ##
#4=pop 18-24
#28=pop 18-24 with at least bachelors
   #2009-2014 are in pcts
   #2015-16 are actual counts
#34=pop 25+
#70= pop 25+ with at least bacehlors
#2009-2014 are in pcts
#2015-16 are actual counts
edu09.trunc <- edu09[,c(2,3,4,28,34,70)]
edu09.trunc$BA18_24Count <- (as.numeric(as.character(edu09.trunc[,4]))/100)*as.numeric(as.character(edu09.trunc[,3]))
edu09.trunc$BA25plusCount <- (as.numeric(as.character(edu09.trunc[,6]))/100)*as.numeric(as.character(edu09.trunc[,5]))

edu10.trunc <- edu10[,c(2,3,4,28,34,70)]
edu10.trunc$BA18_24Count <- (as.numeric(as.character(edu10.trunc[,4]))/100)*as.numeric(as.character(edu10.trunc[,3]))
edu10.trunc$BA25plusCount <- (as.numeric(as.character(edu10.trunc[,6]))/100)*as.numeric(as.character(edu10.trunc[,5]))

edu11.trunc <- edu11[,c(2,3,4,28,34,70)]
edu11.trunc$BA18_24Count <- (as.numeric(as.character(edu11.trunc[,4]))/100)*as.numeric(as.character(edu11.trunc[,3]))
edu11.trunc$BA25plusCount <- (as.numeric(as.character(edu11.trunc[,6]))/100)*as.numeric(as.character(edu11.trunc[,5]))

edu12.trunc <- edu12[,c(2,3,4,28,34,70)]
edu12.trunc$BA18_24Count <- (as.numeric(as.character(edu12.trunc[,4]))/100)*as.numeric(as.character(edu12.trunc[,3]))
edu12.trunc$BA25plusCount <- (as.numeric(as.character(edu12.trunc[,6]))/100)*as.numeric(as.character(edu12.trunc[,5]))

edu13.trunc <- edu13[,c(2,3,4,28,34,70)]
edu13.trunc$BA18_24Count <- (as.numeric(as.character(edu13.trunc[,4]))/100)*as.numeric(as.character(edu13.trunc[,3]))
edu13.trunc$BA25plusCount <- (as.numeric(as.character(edu13.trunc[,6]))/100)*as.numeric(as.character(edu13.trunc[,5]))

edu14.trunc <- edu14[,c(2,3,4,28,34,70)]
edu14.trunc$BA18_24Count <- (as.numeric(as.character(edu14.trunc[,4]))/100)*as.numeric(as.character(edu14.trunc[,3]))
edu14.trunc$BA25plusCount <- (as.numeric(as.character(edu14.trunc[,6]))/100)*as.numeric(as.character(edu14.trunc[,5]))

edu15.trunc <- edu15[,c(2,3,4,52,64,136)]
#4=pop 18-24
#52= pop 18-24 w/ at least bacehlors
#64=pop 25+
#136=pop 25+ w/ at least bachelors
edu15.trunc$BA18_24Count <- as.numeric(as.character(edu15.trunc[,4]))
edu15.trunc$BA25plusCount <- as.numeric(as.character(edu15.trunc[,6]))

edu16.trunc <- edu16[,c(2,3,4,52,64,136)]
edu16.trunc$BA18_24Count <- as.numeric(as.character(edu16.trunc[,4]))
edu16.trunc$BA25plusCount <- as.numeric(as.character(edu16.trunc[,6]))

## change column names to something interpretable ##
eduNames <- c("countyID", "countyName", "pop18_24", "BA18_24",
              "pop25plus", "BA25plus")
eduDataSets.list <- list(edu09.trunc, edu10.trunc, edu11.trunc,
                           edu12.trunc, edu13.trunc, edu14.trunc,
                           edu15.trunc, edu16.trunc)
eduDataSets.list <- lapply(eduDataSets.list, function(df){
   colnames(df)[1:6] <- eduNames
   ## getting rid of the descriptive line ##
   df <- df[-1,]
   ## getting rid of the ', Texas' in county name ##
   df$countyName <- gsub(", Texas", "", df$countyName)
   ## make number columns numeric ##
   df[,3] <- as.numeric(as.character(df[,3]))
   df[,4] <- as.numeric(as.character(df[,4]))
   df[,5] <- as.numeric(as.character(df[,5]))
   df[,6] <- as.numeric(as.character(df[,6]))
   df
})

## change list of dataframes to one dataframe ##
edu <- ldply(eduDataSets.list, as.data.frame)
edu$year <- c( rep(2009, (dim(edu09.trunc)[1] - 1)),
                 rep(2010, (dim(edu10.trunc)[1] - 1)), 
                 rep(2011, (dim(edu11.trunc)[1] - 1)),
                 rep(2012, (dim(edu12.trunc)[1] - 1)), 
                 rep(2013, (dim(edu13.trunc)[1] - 1)),
                 rep(2014, (dim(edu14.trunc)[1] - 1)), 
                 rep(2015, (dim(edu15.trunc)[1] - 1)),
                 rep(2016, (dim(edu16.trunc)[1] - 1)) )
edu$yrsSince2009 <- edu$year - 2009

## make total population 18+ variable ##
edu$totPop18plus <- rowSums(edu[,c("pop18_24", "pop25plus")])
edu$BAtotPop18plus <- rowSums(edu[,c("BA18_24Count", "BA25plusCount")])
edu$BApctTotPop18plus <- (edu$BAtotPop18plus/edu$totPop18plus)*100

## rearrange the columns and get rid of original count columns ##
edu <- edu[,c(1,2,9,10,3,7,5,8,11,12,13)]

#### COMBINE THE DATASETS INTO MAIN TABLE ####
house.edu <- merge(house, edu,
                   by=c("countyID", "countyName","year", "yrsSince2009"))

#### WRITE INTO CSV ####
write.csv(house.edu, "house_edu.csv")
