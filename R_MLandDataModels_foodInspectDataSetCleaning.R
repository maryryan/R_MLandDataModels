######################
########################## R: MACHINE LEARNING AND DATA MODELS
########################## TEXAS MEDIAN HOUSING PRICE AND EDUCATIONAL ATTAINMENT
########################## DATASET CREATION/COMBINATION
########################## MARY RYAN
########################## NICAR 2018
########################## MARCH 8-11
######################

## GOAL: see how being high risk or a fast food chain affects likelihood of
## passing health inspection

#### LOAD LIBRARIES ####
# install.packages('dplyr')
# install.packages('data.table')

library(plyr)
library(data.table)

#### LOAD ORIGINAL FILES ####
## Chicago health inspection data originally obtained from City of Chicago
## Data Portal:
## https://data.cityofchicago.org/Health-Human-Services/Food-Inspections/4ijn-s7e5
   ## Other cities can be found through the US City Open Data Census:
   ## http://us-city.census.okfn.org/dataset/food-safety

food <- read.csv('Food_Inspections.csv', header=T)
## same entity has same License.. number

## make binary variables for inspection result ##
pass.cat <- c("Pass", "Pass w/ Conditions")
food$pass <- ifelse(food$Results %in% pass.cat, 1, 0)
food$fail <- ifelse(food$Results=="Fail", 1, 0)

## find the fastfood chains ##
bigChains <-c("MCDONALDS", "MCDONALD'S", "TACO BELL",
              "BURGER KING", "KFC", "KENTUCKY FRIED CHICKEN",
              "SUBWAY", "SONIC", "QUIZNOS", "CHICK-FIL-A",
              "CHICKFILA", "ARBYS", "ARBY'S", "SHAKE SHACK", "CARLS JR",
              "CARL'S JR", "CARLS JR.", "CARL'S JR.", "FIVE GUYS",
              "WENDYS", "WENDY'S")
chains <- sapply(bigChains, grepl, food$DBA.Name)
chains.id <- food[which(chains==T),"License.."]

food$chain <- ifelse(food$License.. %in% chains.id, 1, 0)

## make variable for high risk entities ##
food$Risk <- factor(food$Risk)
food$hiRisk <- ifelse(food$Risk == levels(food$Risk)[3], 1, 0)

## get rid of the violations col (too long for what we want) and rename ##
## also get rid of AKA name, Lat, Long, Location ##
colnames(food)[4] <- "License"
food.Inspect <- food[,-c(3,14:17)]

## reorder the columns ##
colOrder <- c("License", "DBA.Name", "Address", "City", "Zip", "Facility.Type",
              "Inspection.ID","Inspection.Date", "Inspection.Type", "Results",
              "pass", "fail", "chain", "hiRisk"
              )
food.Inspect <- food.Inspect[,colOrder]

## make inspection date variable a data class ##
food.Inspect$Inspection.Date <- as.Date(food.Inspect$Inspection.Date,
                                                  format='%m/%d/%Y')
food.Inspect$License <- as.character(food.Inspect$License)
## make sure the dates are ordered within each entity ##
food.Inspect.split <- lapply(split(food.Inspect, food.Inspect$License),
                           function(df){
                              df[order(df$Inspection.Date),]})
food.Inspect.dateOrdered <- do.call(rbind, food.Inspect.split)
   # this step takes a long time so go get some coffee for 20 minutes

## make a 'time since baseline' variable ##
food.Inspect.dateOrdered$timeSinceBaseline <- vector(length=dim(food.Inspect.dateOrdered)[1])

## subtract date of first visit from all other visits and make it in terms of months ##
#divide the days by 30.4375 because 365.25/12=30.4375
food.Inspect.dateOrdered$timeSinceBaseline <- unlist(lapply(split(food.Inspect.dateOrdered,
                                                                  food.Inspect.dateOrdered$License),
                                                          function(df){
                                                             (df$Inspection.Date-df$Inspection.Date[1])/30.4375
                                                          }))

## get rid of the License 0's ##
food.Inspect.dateOrdered <- food.Inspect.dateOrdered[which(food.Inspect.dateOrdered$License != 0),]

## write the new csv ##
write.csv(food.Inspect.dateOrdered, "food.InspectClean.csv")
