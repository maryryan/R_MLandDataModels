#########################
############################# R: MACHINE LEARNING AND DATA MODELS
############################# TEXAS STAAR TEST DATA SET CREATION/COMBINATION
############################# MARY RYAN
############################# NICAR 2018
############################# MARCH 8-11
#########################

## GOAL: to see how increases in median district home price affects
## district composite percent passing ISAT test

#### LOAD LIBRARIES ####
# install.packages('readxl')
# install.packages('dplyr')
# install.packages('plyr')
# install.packages('lubridate')

library(readxl)
library(dplyr)
library(plyr)
library(lubridate)

#### LOAD ORIGINAL FILES ####
## data orginally obtained from Illinois Board of Education:
## https://www.isbe.net/pages/illinois-state-report-card-data.aspx
ISAT1314 <- as.data.frame(read_excel('ISAT_1314.xlsx'))
ISAT1314 <- ISAT1314[,c(1:5,23)]
colnames(ISAT1314) <- ISAT1314[6,1:6]
ISAT1314 <- ISAT1314[-c(1:6),]

ISAT1213 <- as.data.frame(read_excel('ISAT_1213.xlsx'))
ISAT1213 <- ISAT1213[,c(1:5,23)]
colnames(ISAT1213) <- ISAT1213[6,1:6]
ISAT1213 <- ISAT1213[-c(1:7),]

ISAT1112 <- as.data.frame(read_excel('ISAT_1112.xlsx'))
ISAT1112 <- ISAT1112[,c(1:5,23)]
colnames(ISAT1112) <- ISAT1112[6,1:6]
ISAT1112 <- ISAT1112[-c(1:7),]

ISAT1011 <- as.data.frame(read_excel('ISAT_1011.xlsx'))
ISAT1011 <- ISAT1011[,c(1:5,23)]
colnames(ISAT1011) <- ISAT1011[6,1:6]
ISAT1011 <- ISAT1011[-c(1:7),]

ISAT0910 <- as.data.frame(read_excel('ISAT_0910.xls'))
ISAT0910 <- ISAT0910[,c(1:5,23)]
colnames(ISAT0910) <- ISAT0910[6,1:6]
ISAT0910 <- ISAT0910[-c(1:7),]

ISAT0809 <- as.data.frame(read_excel('ISAT_0809.xls'))
ISAT0809 <- ISAT0809[,c(1:5,23)]
colnames(ISAT0809) <- ISAT0809[6,1:6]
ISAT0809 <- ISAT0809[-c(1:7),]

ISAT0708 <- as.data.frame(read_excel('ISAT_0708.xls'))
ISAT0708 <- ISAT0708[,c(1:5,20)]
colnames(ISAT0708) <- ISAT0708[6,1:6]
ISAT0708 <- ISAT0708[-c(1:7),]

## data originally obtained from American Community Survey
## Table S2506
house09 <- read.csv('illMedHomeValue_09_S2506/ACS_09_5YR_S2506_with_ann.csv', header=T)
house09.trunc <- house09[,c(2,3,20)]
house10 <- read.csv('illMedHomeValue_10_S2506/ACS_10_5YR_S2506_with_ann.csv', header=T)
house10.trunc <- house10[,c(2,3,20)]
house11 <- read.csv('illMedHomeValue_11_S2506/ACS_11_5YR_S2506_with_ann.csv', header=T)
house11.trunc <- house11[,c(2,3,20)]
house12 <- read.csv('illMedHomeValue_12_S2506/ACS_12_5YR_S2506_with_ann.csv', header=T)
house12.trunc <- house12[,c(2,3,20)]
house13 <- read.csv('illMedHomeValue_13_S2506/ACS_13_5YR_S2506_with_ann.csv', header=T)
house13.trunc <- house13[,c(2,3,20)]
house14 <- read.csv('illMedHomeValue_14_S2506/ACS_14_5YR_S2506_with_ann.csv', header=T)
house14.trunc <- house14[,c(2,3,20)]
house15 <- read.csv('illMedHomeValue_15_S2506/ACS_15_5YR_S2506_with_ann.csv', header=T)
house15.trunc <- house15[,c(2,3,20)]

#### ISAT SCORES DATASET ####
ISAT.list <- list(ISAT0708, ISAT0809, ISAT0910, ISAT1011,ISAT1112,
                    ISAT1213, ISAT1314)


ISAT.list <- lapply(ISAT.list, function(df){
      df$ISAT <- as.numeric(df$ISAT)
      return(df)
})

## subsetting data to overall district scores ##
# all the overall district observations have RCDTS numbers that end in 0
ISAT.list2 <- lapply(ISAT.list, function(df){
   df[which(substr( df[,1], 15, 15 ) == "0"),]
})

ISAT <- merge(ISAT.list2[[1]], ISAT.list2[[2]],
              by=c("RCDTS", "County", "Dist #",
                   "District Name/ School Name", "City"), all=T)
colnames(ISAT)[6:7] <- c("ISAT0708", "ISAT0809")
ISAT <- merge(ISAT, ISAT.list2[[3]],
              by=c("RCDTS", "County", "Dist #",
                   "District Name/ School Name", "City"), all=T)
colnames(ISAT)[8] <- "ISAT0910"
ISAT <- merge(ISAT, ISAT.list2[[4]],
              by=c("RCDTS", "County", "Dist #",
                   "District Name/ School Name", "City"), all=T)
colnames(ISAT)[9] <- "ISAT1011"
ISAT <- merge(ISAT, ISAT.list2[[5]],
              by=c("RCDTS", "County", "Dist #",
                   "District Name/ School Name", "City"), all=T)
colnames(ISAT)[10] <- "ISAT1112"
ISAT <- merge(ISAT, ISAT.list2[[6]],
              by=c("RCDTS", "County", "Dist #",
                   "District Name/ School Name", "City"), all=T)
colnames(ISAT)[11] <- "ISAT1213"
ISAT <- merge(ISAT, ISAT.list2[[7]],
              by=c("RCDTS", "County", "Dist #",
                   "District Name/ School Name", "City"), all=T)
colnames(ISAT)[12] <- "ISAT1314"




#### HOUSING PRICE DATASET ####
house <- merge(house09.trunc, house10.trunc, by="GEO.id2", all=TRUE)
house <- house[,-4]
colnames(house) <- c("GEO.id2", "District Name", "09Median", "10Median")
house <- merge(house, house11.trunc, by="GEO.id2", all=TRUE)
house <- house[,-5]
colnames(house) <- c("GEO.id2", "District Name", "09Median", "10Median",
                     "11Median")
house <- merge(house, house12.trunc, by="GEO.id2", all=TRUE)
house <- house[,-6]
colnames(house) <- c("GEO.id2", "District Name", "09Median", "10Median",
                     "11Median", "12Median")
house <- merge(house, house13.trunc, by="GEO.id2", all=TRUE)
house <- house[,-7]
colnames(house) <- c("GEO.id2", "District Name", "09Median", "10Median",
                     "11Median", "12Median", "13Median")
house <- merge(house, house14.trunc, by="GEO.id2", all=TRUE)
house <- house[,-8]
colnames(house) <- c("GEO.id2", "District Name", "09Median", "10Median",
                     "11Median", "12Median", "13Median", "14Median")
house <- merge(house, house15.trunc, by="GEO.id2", all=TRUE)
house <- house[,-9]
colnames(house) <- c("GEO.id2", "District Name", "09Median", "10Median",
                     "11Median", "12Median", "13Median", "14Median", "15Median")

## NEED TO MATCH HOUSING DATA AND SCORES DATA ON SOME DISTRICT CODE