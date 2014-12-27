# Nick Giffen - 24 Dec 2014
# Week 17 NFL Analysis for DraftKings

# Set working directory and import datafiles
setwd("~/DFS/DFS-NFL")
MASTER <- read.csv("MASTER_DFS.csv")
WK17 <- read.csv("Week17.csv")

# Install and load required packages for decision trees, random forests, and plotting
#install.packages('rattle')
#install.packages('rpart.plot')
#install.packages('RColorBrewer')
#install.packages('randomForest')
#install.packages('party')

library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(randomForest)
library(party)

# Set parameters for # of trees in random forests
HIPar <- 1000
DVOAPar <- 2000
DKPtsPar <- 500
fitPar <- 2000

# Populate Missing Harris Index (HI) data with Random Forest Prediction
HIFit <- cforest(HI ~ Opp + Spread + Total + Pos, data=MASTER[!is.na(MASTER$HI),],
                 controls=cforest_unbiased(ntree=HIPar, mtry=2))
MASTER$HI[is.na(MASTER$HI)] <- predict(HIFit, MASTER[is.na(MASTER$HI),], OOB=TRUE)

# Populate Missing Harris Index Last 5 week (HI5) data with Random Forest Prediction
HI5Fit <- cforest(HI5 ~ Opp + Spread + Total + Pos + HI, data=MASTER[!is.na(MASTER$HI5),],
                  controls=cforest_unbiased(ntree=HIPar, mtry=2))
MASTER$HI5[is.na(MASTER$HI5)] <- predict(HI5Fit, MASTER[is.na(MASTER$HI5),], OOB=TRUE)

# Populate Missing DVOA (PosDVOA) data with Random Forest Prediction
DVOAFit <- cforest(PosDVOA ~ Opp + Spread + Total + Pos + HI + HI5, 
                   data=MASTER[!is.na(MASTER$PosDVOA),],
                   controls=cforest_unbiased(ntree=DVOAPar, mtry=2))
MASTER$PosDVOA[is.na(MASTER$PosDVOA)] <- predict(DVOAFit, MASTER[is.na(MASTER$PosDVOA),], OOB=TRUE)

# Box-Cox transformation to make DKPts data near normal
MASTER$DKPts <- ((MASTER$DKPts ^ 0.2) - 1) / 0.0456898623408216

# NA values in DKPts filled by Random Forest
DKPtsFit <- cforest(DKPts ~ Yds + TD + Int + Yds.1 + TD.1 + Rec + Yds.2 + TD.2 + Fmb + PaBonus + 
                      RuBonus + ReBonus + Day, 
                    data=MASTER[!is.na(MASTER$DKPts),],
                    controls=cforest_unbiased(ntree=DKPtsPar, mtry=3))
MASTER$DKPts[is.na(MASTER$DKPts)] <- predict(DKPtsFit, MASTER[is.na(MASTER$DKPts),], OOB=TRUE)

# Decision Tree to Predict DraftKings Points (removed FPPG as test)
fit <- cforest(DKPts ~ Pos + Last.1 + HI + HI5 + Last.2 + Last.3 + Last.4 + Last.5 + Last.6 + 
                 Spread + Total + PosDVOA + Opp, data=MASTER, 
               controls = cforest_unbiased(ntree=fitPar, mtry=4))

# Prediciton for current week
Prediction <- predict(fit, WK17, OOB=TRUE)
Prediction <- ((Prediction * 0.0456898623408216) + 1) ^ 5
submit <- data.frame(Player = WK17$Player, PredDKPts = Prediction)
write.csv(submit, file = "Updated-Pred-17.csv", row.names = FALSE)

# Box-Cox Transformed prediction on MASTER data file up to week prior to current
Prediction2 <- predict(fit, MASTER, OOB=TRUE)
submit <- data.frame(Player = MASTER$Player, PredDKPts = Prediction2)
write.csv(submit, file = "Updated-Pred-MASTER.csv", row.names = FALSE)