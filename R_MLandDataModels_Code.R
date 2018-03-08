######################
############################# R: MACHINE LEARNING AND DATA MODELS
############################# MARY RYAN
############################# NICAR 2018
############################# MARCH 8-11
######################

#### LINK TO THE GITHUB REPO FOR THIS CLASS ####
## If you want to access what we're doing today on your own machine anytime
## you want, all code and data exist in a repo on my Github.
## Additional data, R code, and notes live there as well:
## https://github.com/maryryan/R_MLandDataModels.git

#### LOAD LIBRARIES ####
#install.packages('splines')
#install.packages('gee')
#install.packages('survival')
#install.packages('nlme')
#install.packages('lme4')

library(splines)
library(gee)
library(survival)
library(nlme)
#library(lme4)

#### FUNCTIONS WE NEED ####
glmCI.long <- function (model, transform = TRUE, robust = FALSE) 
{
   link <- model$family$link
   coef <- summary(model)$coef[, 1]
   se <- ifelse1(robust, summary(model)$coef[,4], summary(model)$coef[, 
                                                                        2])
   zvalue <- coef/se
   pvalue <- 2 * (1 - pnorm(abs(zvalue)))
   if (transform & is.element(link, c("logit", "log"))) {
      ci95.lo <- exp(coef - qnorm(0.975) * se)
      ci95.hi <- exp(coef + qnorm(0.975) * se)
      est <- exp(coef)
   }
   else {
      ci95.lo <- coef - qnorm(0.975) * se
      ci95.hi <- coef + qnorm(0.975) * se
      est <- coef
   }
   rslt <- round(cbind(est, ci95.lo, ci95.hi, zvalue, pvalue), 
                 4)
   colnames(rslt) <- ifelse1(robust, c("Est", "robust ci95.lo", 
                                       "robust ci95.hi", "robust z value", "robust Pr(>|z|)"), 
                             c("Est", "ci95.lo", "ci95.hi", "z value", "Pr(>|z|)"))
   colnames(rslt)[1] <- ifelse(transform & is.element(link, 
                                                      c("logit", "log")), "exp( Est )", "Est")
   rslt
}

#### LOAD CLEAN DATA ####
house.edu <- read.csv('house_edu.csv', header=T)
house.edu <- house.edu[,-1]

food <- read.csv('food.InspectClean.csv', header=T)
food <- food[,-1]

## links to original data and code for how it was cleaned can be found in
## my github repo

####
######## CONTINUOUS DATA (HOUSE.EDU) ########
####

#### PLOT ALL DATA TOGETHER AND FIND TREND ####
par(mfrow=c(1,1))
plot(house.edu$yrsSince2009, house.edu$Median,
     main="Overall Trend",
     xlab="Years Since 2009",
     ylab="Median Home Value")
abline(lm(house.edu$Median ~ house.edu$yrsSince2009))

#### SPAGHETTI PLOT ####
par(mfrow=c(1,1))
plot( house.edu$yrsSince2009, house.edu$Median, pch=".",
      xlab="Years Since 2009",
      ylab="Median Home Value")
uid <- unique( house.edu$countyID )

for( j in seq( length(uid) ) ){
   lines(house.edu$yrsSince2009[ house.edu$countyID==uid[j] ],
         house.edu$Median[ house.edu$countyID==uid[j] ])
}

#### INDIVIDUAL LINEAR REGRESSIONS ####
house.edu.grouped <- groupedData( Median ~ yrsSince2009 | countyID,
                                  data=house.edu )

indiv.house.edu.lm <- lmList( Median ~ yrsSince2009 | countyID,
                        data=house.edu.grouped )

## creating forest plots of the intercepts & slopes of all the regressions ##
len <- length(indiv.house.edu.lm)
forest.matrix <- matrix(NA, nrow = len, ncol = 7)
forest.matrix[,1] <- names(indiv.house.edu.lm)
forest.matrix[,2] <- intervals(indiv.house.edu.lm)[1:len]
forest.matrix[,3] <- intervals(indiv.house.edu.lm)[(len+1):(2*len)]
forest.matrix[,4] <- intervals(indiv.house.edu.lm)[(2*len + 1):(3*len)]
forest.matrix[,5] <- intervals(indiv.house.edu.lm)[(3*len + 1):(4*len)]
forest.matrix[,6] <- intervals(indiv.house.edu.lm)[(4*len + 1):(5*len)]
forest.matrix[,7] <- intervals(indiv.house.edu.lm)[(5*len + 1):(6*len)]

par(mfrow = c(1,2))
plot( forest.matrix[1,2:4], rep(1, 3), type = "l", ylim = c(1, len),
      xlim=c(70000,210000),xlab = "Intercept", ylab= "Order of Intercept")
for( j in 1:length(unique(forest.matrix[,1]))){
   lines(forest.matrix[j,2:4], rep(j, 3))
   
}

plot( forest.matrix[1,5:7], rep(1, 3), type = "l", ylim = c(1, len),
      xlim=c(-200,12000), xlab = "Slope", ylab= "Order of Intercept")
for( j in 1:length(unique(forest.matrix[,1]))){
   lines(forest.matrix[j,5:7], rep(j, 3))
   
}
mtext("Random Intercepts & Slopes of Texas Median Home Values", outer=TRUE,
      line=-2)

#### RANDOM INTERCEPTS ####

#### RANDOM SLOPES ####


#### GENERALIZED ESTIMATING EQUATIONS (GEES) ####
## Independence structure ##
## What you would get if you did a regular logit/GLM regression
house.edu.gee.ind <- gee(Median ~ BApctTotPop18plus + yrsSince2009,
                         id=countyID,
                         data=house.edu,
                         corstr="independence")
summary( house.edu.gee.ind )

## Exchangeable structure ##
house.edu.gee.exch <- gee(Median ~ BApctTotPop18plus + yrsSince2009,
                          id=countyID,
                          data=house.edu,
                          corstr="exchangeable")
summary( house.edu.gee.exch )

## AR-1 structure ##
house.edu.gee.AR1 <- gee(Median ~ BApctTotPop18plus + yrsSince2009,
                         id=countyID,
                         data=house.edu,
                         corstr="AR-M",
                         Mv=1)
summary( house.edu.gee.AR1 )
## INTERPRETATIONS ##
## yrsSince2009: expected slope of median home value among counties with
   # no college grads
## BApctTotPop18plus: expected difference in mean median home value in
   # 2009 comparing sub-groups of counties with 1 percentage point
   # difference in college grads
      # i.e., counties with no college grads compared with counites where
      # 1% of the 18+ population have at least a bachelors
## compare these results with what you would've gotten with a classic lm

#### ROBUST STANDARD ERROR ####
## You may notice in the output from the summary of the GEE that in addition to
## the "Estimate" column there are 4 other columns:
   ## Naive S.E.
   ## Naive Z
   ## Robust S.E.
   # Robust Z
## "Naive S.E." indicates the standard error for that coefficient if we assumed
## constant variance. Similarly, "Naive Z" is the Z statistic you would get if
## you used that standard error.
## However, we have reason to believe that different counties will have
## different variances. The Robust standard error (denoted "Robust S.E.") is
## is post-model fix-em-up for this problem. There's a lot of statistical
## theory that goes into this, but basically if the variances really don't
## differ by county you'll get something pretty close to the regular
## standard error, and if they do differ then this will help fix the
## variance. And, as you can imagine, the "Robust Z" is the Z statistic you
## would get if you used the Robust standard error.
## BIG OLE ASTERISKS HERE THOUGH. The Robust standard error DOES NOT DO WELL
## (i.e., it's unpredictable) when you have fewer than 50 independent clusters.
## If you have fewer than 50 clusters, it's safer to go with the regular
## standard error because at least we know *why* it's wrong.

#### INTERACTIONS WITH TIME ####
house.edu.gee.interact <- gee(Median ~ BApctTotPop18plus*yrsSince2009,
                     id=countyID,
                     data=house.edu,
                     corstr="AR-M",
                     Mv=1)
summary( house.edu.gee.interact )
## INTERPRETATIONS ##
## yrsSince2009 and BApctTotPop18plus have the same interpretaitosn as before.
## The interaction term: expected difference in slope of median home value 
   # among sub-groups of counties with 1 percentage point difference in
   # college grads.
      # i.e., does how median home values change over time depend on the
      # proportion of the 18+ population that have at least a bachelors?


####

####
######## BINARY DATA (HEALTH INSPECTIONS) ########
####
#### PLOT ALL DATA TOGETHER AND FIND TREND ####
par(mfrow=c(1,1))
plot(food$timeSinceBaseline, food$fail,
     main="Overall Trend",
     xlab="Months Since First Inspection",
     ylab="Failing Inspection")
abline(lm(food$fail ~ food$timeSinceBaseline))
## not super informative, so let's look at some box plots over time

## make a "time category" variable by splitting up time into 6-mo chunks ##
## The chunks are subjective -- I made a decision based on
## summary(food$timeSinceBaseline) -- Q3 was 46 mo so figured anything above
## that could be one category
food$timeCat <- rep(0, length(food$License))
for(i in seq(length(food$License))){
   if(food[i, "timeSinceBaseline"] == 0){
      food[i, "timeCat"] <- 1
   } else if(food[i, "timeSinceBaseline"] <= 6){
      food[i, "timeCat"] <- 2
   } else if(food[i, "timeSinceBaseline"] <=12){
      food[i, "timeCat"] <- 3
   } else if(food[i, "timeSinceBaseline"] <=18){
      food[i, "timeCat"] <- 4
   } else if(food[i, "timeSinceBaseline"] <=24){
      food[i, "timeCat"] <- 5
   } else if(food[i, "timeSinceBaseline"] <=30){
      food[i, "timeCat"] <- 6
   } else if(food[i, "timeSinceBaseline"] <=36){
      food[i, "timeCat"] <- 7
   } else if(food[i, "timeSinceBaseline"] <=42){
      food[i, "timeCat"] <- 8
   } else if(food[i, "timeSinceBaseline"] <= 48){
      food[i, "timeCat"] <- 9
   } else {
      food[i, "timeCat"] <- 10
   }
}

boxplot(food$fail ~ food$timeCat)

#### GENERALIZED ESTIMATING EQUATIONS (GEES) ####
## Independence structure ##
## What you would get if you did a regular logit/GLM regression
food.gee.ind <- gee(fail ~ chain + timeSinceBaseline,
                    id=License,
                    data=food,
                    family=binomial(link='logit'), # this is different from the continuous set!
                    corstr="independence")
summary( food.gee.ind )

## to truly interpret, though, you need to exponentiate ##
glmCI.long(food.gee.ind, robust=TRUE)

## Exchangeable structure ##
food.gee.exch <- gee(fail ~ chain + timeSinceBaseline,
                     id=License,
                     data=food,
                     family=binomial(link='logit'), 
                     corstr="exchangeable")

glmCI.long(food.gee.exch, robust=T)

## AR-1 structure ##
food.gee.AR1 <- gee(fail ~ chain + timeSinceBaseline,
                    id=License,
                    data=food,
                    family=binomial(link='logit'), 
                    corstr="AR-M",
                    Mv=1)

glmCI(food.gee.AR1, robust=TRUE)

## INTERPRETATIONS ##
## chain: the increased/decreased percentage likelihood of failure of a chain
## restaurant compared to another entity at the same timepoint
   ## if the exp( Est ) > 1, then the chain is [exp( Est ) - 1]*100% more
   ## likely to fail
   ## if exp( Est ) < 1, then the chain is [1 - exp( Est )]*100% less
   ## likely to fail

## Q: can we trust the robust inference here? ##
#### INTERACTIONS WITH TIME ####
food.gee.interact <- gee(fail ~ chain*timeSinceBaseline,
                         id=License,
                         data=food,
                         family=binomial(link='logit'),
                         corstr="exchangeable")

glmCI(food.gee.interact, robust=TRUE)
