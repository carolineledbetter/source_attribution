###########################################
# Project: P1330White
# Author: David Weitzenkamp & Caroline Ledbetter
# Date: 07/30/2018
# #########################################

load(file = 'DataProcessed/DataClean.RData')

library(CIDAtools)
library(caret)
library(adabag)
library(rpart)
library(C50)
library(earth)
library(mda)
library(plyr)
library(doParallel)
cl <- makeCluster(detectCores() -1)
registerDoParallel(cl)


Analysis$Geography <- factor('Missing', levels = c("MultiState", 
                                              "MultiCounty",
                                              'SingleCounty', 
                                              'Missing'))
Analysis$Geography[Analysis$MultiCountyExposure] <- 'MultiCounty'
Analysis$Geography[Analysis$MultiStateExposure] <- 'MultiState'
Analysis$Geography[!Analysis$MultiStateExposure & 
                     !Analysis$MultiCountyExposure] <- 'SingleCounty'

table(Analysis$MultiCountyExposure, 
      Analysis$MultiStateExposure, 
      Analysis$Geography, 
      useNA = 'ifany')
Analysis <- subset(Analysis, select = -c(CDCID, OutbreakLength, 
                                         HospPercent, DeathsNum, DeathsInfo, 
                                         HospitalNum, HospitalInfo, SalmSTEC, 
                                         DeathsPct, MultiCountyExposure, 
                                         MultiStateExposure))
Analysis$Category <- factor(Analysis$Category, levels = c("Eggs", 
                                                          "Meat", 
                                                          "Poultry", 
                                                          "Produce", 
                                                          "AnimalContact"), 
                            labels = c("Eggs", 
                                       "MeatPoultry", 
                                       "MeatPoultry", 
                                       "Produce", 
                                       "AnimalContact"))
vars <- seq_along(Analysis)
first <- which(names(Analysis) == 'Category')
vars <- setdiff(vars, first)
Analysis <- Analysis[, c(first, vars)]
rm(vars, first)

nearZeroVar(Analysis)
names(Analysis)[nearZeroVar(Analysis)]


set.seed(107)
inTrain <- createDataPartition(y = Analysis$Category, 
                               p = .75, 
                               list = F)
training <- Analysis[inTrain, ]
testing <- Analysis[-inTrain, ]


trainX <- subset(training, select = -Category)
trainY <- training[, 'Category', drop = T]

seeds <- vector(mode = "list", length = nrow(training) + 1)
seeds <- lapply(seeds, function(x) 1:20)
cctrl1 <- trainControl(method = "repeatedcv", number = 10, 
                       repeats = 3, 
                       returnResamp = "all",
                       classProbs = TRUE, 
                       summaryFunction = multiClassSummary, 
                       seeds = seeds, 
                       search = 'random', 
                       allowParallel = T)
models <- list()
set.seed(849)
# models$adaboostm1 <- train(trainX, trainY, 
#                            method = "AdaBoost.M1", 
#                            trControl = cctrl1, 
#                            preProc = c("center", "scale"))

grid <- expand.grid(mfinal = (1:3)*3, maxdepth = c(1, 3))
set.seed(849)
models$AdaBag <- train(trainX, trainY, 
                       method = 'AdaBag', 
                       trControl = cctrl1, 
                       tuneGrid = grid,  
                       preProc = c('center', 'scale'))

set.seed(849)
models$CART <- train(trainX, trainY, 
                     method = 'rpart1SE', 
                     trControl = cctrl1, 
                     preProc = c("center", "scale"))

set.seed(849)
models$baggedFDA <- train(trainX, trainY, 
                          method = "bagFDA", 
                          trControl = cctrl1,
                          preProc = c("center", "scale", 'knnImpute'), 
                          tuneGrid = data.frame(.degree = 1,
                                                .nprune = 2:4), 
                          B = 10)
beepr::beep('ready')
set.seed(849)
models$FDA <- train(trainX, trainY, 
                    method = "fda", 
                    trControl = cctrl1,
                    preProc = c("center", "scale", 'knnImpute'), 
                    tuneLength = 4)

beepr::beep('ready')

Pred_Probs <- lapply(models, predict, newdata = testing[, -1],  
                     type = 'prob')

ordtest <- order(testing$Category)
orderedtest <- testing[ordtest, ]
Pred_Probs <- lapply(Pred_Probs, function(l) {
  l <- l[ordtest, ]
  l$Actual <- as.numeric(orderedtest$Category)
  l$ID <- 1:nrow(l)
  return(l)})

y_axis <- data.frame(Ticks = which(!duplicated(orderedtest$Category)),
                     Labels = levels(orderedtest$Category))
y_axis$LabelPos <- diff(c(y_axis$Ticks, nrow(testing)))/2 + y_axis$Ticks
MeltedPredictions <- lapply(Pred_Probs, reshape2::melt, 
                            id.vars = c('ID', 'Actual'),  
                            value.name = 'PredProb', 
                            variable.name = 'Category')
heat_map <- function(l){
  ggplot(l, aes(x = Category, y = ID, fill = PredProb)) +
    geom_tile() + theme_classic() + 
    scale_y_continuous(breaks = y_axis$LabelPos, 
                       labels = y_axis$Labels, 
                       minor_breaks = y_axis$Ticks, 
                       expand = c(0,0)) + 
    scale_fill_gradient2(low = 'red', high = "#0000cc", mid = '#D9E8F7', 
                         midpoint = 1/9) + 
    theme(panel.grid.minor.y = element_line(colour = 'black', size = 0.5), 
          panel.ontop = TRUE, 
          panel.background = element_rect(fill = NA), 
          axis.text.y = element_text(hjust = 0.5)) + 
    labs(x = 'Predicted Probability', y = 'Actual Source')
}
lapply(MeltedPredictions, heat_map)


Pred_Probs <- lapply(Pred_Probs, function(l) {
  l$obs <- factor(l$Actual,
                     labels = levels(orderedtest$Category))
  return(l)})

AdaBag <- predict(models[[1]], testing[-1], type = 'prob')
AdaBag$pred <- predict(models[[1]], testing[-1])
AdaBag$obs <- testing[, 1]

rpart <- predict(models[[2]], testing[-1], type = 'prob')
rpart$pred <- predict(models[[2]], testing[-1])
rpart$obs <- testing[, 1]

FDA <- predict(models[[3]], testing[-1], type = 'prob')
FDA$pred <- predict(models[[3]], testing[-1])
FDA$obs <- testing[, 1]

multiClassSummary(AdaBag, lev = levels(AdaBag$obs))
multiClassSummary(rpart, lev = levels(rpart$obs))
multiClassSummary(FDA, lev = levels(FDA$obs))
mnLogLoss(FDA, lev = levels(FDA$obs))
stopCluster(cl)

Pred_accuracy <- lapply(Pred_Probs, function(l){
  l[, 1:nlevels(l$obs)] <- lapply(l[, 1:nlevels(l$obs)], 
                                  cut, breaks = c(0, .25, .50, .75, 1), 
                                  include.lowest = T)
  l <- droplevels(l)
  return(l)
  })
lapply(Pred_accuracy, function(l){
  p <- list()
  for(i in 1:nlevels(l$obs)){
    p[[i]] <- 
    prop.table(table(l[, i], l$Actual == i), 1)[, 2]
  }; remove(i)
  names(p) <- levels(l$obs)
  return(p)
})
