######################
############################# R: MACHINE LEARNING AND DATA MODELS
############################# MARY RYAN
############################# NICAR 2018
############################# MARCH 8-11
######################

#### LOAD LIBRARIES ####
#install.packages('gee')
#install.packages('survival')
#install.packages('nlme')
library(gee)
library(survival)
library(nlme)

#### LOAD DATA ####
beaches <- read.csv('beaches.csv', header=T)
beaches <- beaches[,-1]
staar <- read.csv('staar.csv', header=T)

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

#### VARIOGRAMS ####

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