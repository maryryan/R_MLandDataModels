######################
############################# R: MACHINE LEARNING AND DATA MODELS
############################# MARY RYAN
############################# NICAR 2018
############################# MARCH 8-11
######################

#### LOAD LIBRARIES ####
#install.packages('splines')
#install.packages('gee')
#install.packages('survival')
#install.packages('nlme')

library(splines)
library(gee)
library(survival)
library(nlme)

#### LOAD DATA ####
beaches <- read.csv('beaches.csv', header=T)
beaches <- beaches[,-1]
house.edu <- read.csv('house_edu.csv', header=T)
house.edu <- house.edu[,-1]

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


# #### EMPIRICAL CORRELATION MATRIX ####
# fit.house <- lm( Median ~ yrsSince2009, data=house.edu )
# resids.house <- house.edu$Median - fitted( fit.house )
# nobs.house <- length( house.edu$Median )
# ncounties <- length( table( house.edu$countyID ) )
# rmat.house <- matrix( NA, ncounties, 8 )
# ycat.house <- 0:7
# nj.house <- unlist( lapply( split( house.edu$countyID, house.edu$countyID ),
#                             length ) )
# 
# for( j in seq( dim(rmat.house)[2] ) ){
#    
#    legal <- ( house.edu$yrsSince2009 >= ycat.house[j]-0.5 )&( house.edu$yrsSince2009 < ycat.house[j]+0.5 )
#    jtime <- house.edu$yrsSince2009 + 0.01*rnorm(nobs.house)
#    t0 <- unlist( lapply( split(abs(jtime - ycat.house[j]), house.edu$countyID), min ) )
#    tj <- rep( t0, nj.house )
#    keep <- ( abs( jtime - ycat.house[j] )==tj )&( legal )
#    yj <- rep(NA, nobs.house)
#    yj[keep] <- resids.house[keep]
#    yj <- unlist( lapply( split(yj, house.edu$countyID), mymin ) )
#    rmat.house[,j] <- yj
#    
# }
# 
# dimnames( rmat.house ) <- list( NULL, paste("Years Since 2009", 0:7) )
# 
# cmat.house <- matrix(0, 8, 8)
# nmat.house <- matrix(0, 8, 8)
# 
# for( j in seq( dim(cmat.house)[1] ) ){
#    for( k in j:dim(cmat.house)[1] ){
#       
#       njk <- sum( !is.na( rmat.house[,j]*rmat.house[,k] ) )
#       sjk <- sum( rmat.house[,j]*rmat.house[,k], na.rm=T )/njk
#       cmat.house[j,k] <- sjk
#       nmat.house[j,k] <- njk
#       
#    }
# }
# 
# vvec.house <- diag( cmat.house )
# cormat.house <- cmat.house/( outer( sqrt(vvec.house), sqrt(vvec.house) ) )
# 
# round(cormat.house, 3)
# nmat.house

#### GENERALIZED ESTIMATING EQUATIONS (GEES) ####
house.edu.gee <- gee(Median ~ BApctTotPop18plus + yrsSince2009,
                     id=countyID,
                     data=house.edu,
                     corstr="AR-M",
                     Mv=1)
summary( house.edu.gee )
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
## You may notice in the output from the summary of the GEE that in additon to
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
######## BINARY DATA (BEACHES) ########
####
## split into fresh and saltwater beaches ##
beaches.fresh <- beaches[which(beaches$MeasureType=="Escherichia coli"),]
beaches.fresh <- beaches.fresh[,c(-7, -8, -9, -12, -13)]
beaches.salt <- beaches[which(beaches$MeasureType=="Enterococcus"),]
beaches.salt <- beaches.salt[,c(-7, -8, -9, -10, -11)]

## remove missing values ##
beaches.fresh.narm <- na.omit(beaches.fresh)
beaches.salt.narm <- na.omit(beaches.salt)
#beaches.narm <- na.omit(beaches)
   
#### LOGISTIC REGRESSION REVIEW ####
## Logitistic regression is a way to turn a regression with a binary (0 or 1)
## response to one that has a continuous range, using the logit function.
## The logit function essentially takes the probability of an event happening,
## divides it by the probability of the event not happening, and taking the
## log of the quantity.
## This means that the way we interpret the coefficients (betas) of the model
## is different than we would in a regular linear regression model.
## With logisitc regression, we look at e^(b), instead of just b, because
## we are undoing the log of the logit function.
## If e^(b) = 1, this means that the an increase in variable for that
## coefficient does not increase the odds that the event will happen.
## If e^(b) > 1 (say, 1.2), this means that a 1 unit increase in the variable
## for that coefficient are 20% more likely to have the event than those with
## 1 unit less of the vairable.
## If e^(b) < 1 (say, 0.8), this means that a 1 unit increase in the variable
## for that coefficient are 20% less likely to have the event than those with
## 1 unit less of the vairable.

#### INDIVIDUAL LINEAR REGRESSION MODELS ####
## grouping the data by beach ID ##
beaches.salt.grouped <- groupedData(closure ~ daysSinceJune1 | Beach.ID,
                                  data=beaches.salt.narm)
beaches.fresh.grouped <- groupedData(closure ~ daysSinceJune1 | Beach.ID,
                                    data=beaches.fresh.narm)
## making an individual logistic regression for each beach ##
indiv.salt.lm <- lmList(closure ~ daysSinceJune1 | Beach.ID,
                   data=beaches.salt.grouped)
indiv.fresh.lm <- lmList(closure ~ daysSinceJune1 | Beach.ID,
                        data=beaches.fresh.grouped)

## creating forest plots of the intercepts & slopes of all the regressions ##
len <- length(indiv.fresh.lm)
forest.matrix <- matrix(NA, nrow = len, ncol = 7)
forest.matrix[,1] <- names(indiv.fresh.lm)
forest.matrix[,2] <- intervals(indiv.fresh.lm)[1:len]
forest.matrix[,3] <- intervals(indiv.fresh.lm)[(len+1):(2*len)]
forest.matrix[,4] <- intervals(indiv.fresh.lm)[(2*len + 1):(3*len)]
forest.matrix[,5] <- intervals(indiv.fresh.lm)[(3*len + 1):(4*len)]
forest.matrix[,6] <- intervals(indiv.fresh.lm)[(4*len + 1):(5*len)]
forest.matrix[,7] <- intervals(indiv.fresh.lm)[(5*len + 1):(6*len)]

par(mfrow = c(1,2))
plot( forest.matrix[1,2:4], rep(1, 3), type = "l", ylim = c(1, len),
      xlim=c(-4, 4),xlab = "Intercept", ylab= "Order of Intercept")
for( j in 1:length(unique(forest.matrix[,1]))){
   lines(forest.matrix[j,2:4], rep(j, 3))
   
}

plot( forest.matrix[1,5:7], rep(1, 3), type = "l", ylim = c(1, len),
      xlim=c(-0.5, 0.5), xlab = "Slope", ylab= "Order of Intercept")
for( j in 1:length(unique(forest.matrix[,1]))){
   lines(forest.matrix[j,5:7], rep(j, 3))
   
}
mtext("Random Intercepts & Slopes of NY Freshwater Beaches", outer=TRUE,
      line=-2)

#### RANDOM INTERCEPTS ####

#### RANDOM SLOPES ####


#### GENERALIZED ESTIMATING EQUATIONS (GEES) ####
beaches.salt.gee <- gee(closure ~ EnterocValue + daysSinceJune1,
                   id=Beach.ID,
                   data=beaches.salt.narm,
                   family=binomial(link='logit'),
                   corstr="exchangeable")
beaches.fresh.gee <- gee(closure ~ ecoliValue + daysSinceJune1,
                        id=Beach.ID,
                        data=beaches.fresh.narm,
                        family=binomial(link='logit'),
                        corstr="exchangeable")

#### ROBUST VARIANCE FIX-UP FOR INFERENCE ####