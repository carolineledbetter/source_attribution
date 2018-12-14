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
library(plyr)
library(RWeka)
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
                                                          "AnimalContact", 
                                                          'Other'), 
                            labels = c("Eggs", 
                                       "MeatPoultry", 
                                       "MeatPoultry", 
                                       "Produce", 
                                       "AnimalContact", 
                                       'Other'))
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
models$kknn <- train(trainX, trainY, 
                    method = "kknn", 
                    trControl = cctrl1,
                    tuneLength = 4, 
                          preProc = c("center", "scale", 'knnImpute'))
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
heat_map <- function(l, name){
  ggplot(l, aes(x = Category, y = ID, fill = PredProb)) +
    geom_tile() + theme_classic() + 
    scale_y_continuous(breaks = y_axis$LabelPos, 
                       labels = y_axis$Labels, 
                       minor_breaks = y_axis$Ticks, 
                       expand = c(0,0)) + 
    scale_fill_gradient2(low = 'red', high = "#0000cc", mid = '#D9E8F7', 
                         midpoint = 1/9) + 
    theme(panel.grid.minor.y = element_line(colour = 'red', size = 1), 
          panel.ontop = TRUE, 
          panel.background = element_rect(fill = NA), 
          axis.text.y = element_text(hjust = 0.5)) + 
    labs(x = 'Predicted Probability', y = 'Actual Source', 
         title = paste('Model = ', name))
}
mapply(heat_map, MeltedPredictions, names(MeltedPredictions), SIMPLIFY = F)



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

kknn <- predict(models[[3]], testing[-1], type = 'prob')
kknn$pred <- predict(models[[3]], testing[-1])
kknn$obs <- testing[, 1]

FDA <- predict(models[[4]], testing[-1], type = 'prob')
FDA$pred <- predict(models[[4]], testing[-1])
FDA$obs <- testing[, 1]

multiClassSummary(AdaBag, lev = levels(AdaBag$obs))
multiClassSummary(rpart, lev = levels(rpart$obs))
multiClassSummary(kknn, lev = levels(kknn$obs))
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


################################################################################
### Remove Others

training <- subset(training, Category != 'Other')
training <- droplevels(training)
testing <- subset(testing, Category != 'Other')
testing <- droplevels(testing)
trainX <- subset(training, select = -Category)
trainY <- training[, 'Category', drop = T]
models2 <- list()
cl <- makeCluster(detectCores() -1)
registerDoParallel(cl)
set.seed(849)
# models2$adaboostm1 <- train(trainX, trainY, 
#                            method = "AdaBoost.M1", 
#                            trControl = cctrl1, 
#                            preProc = c("center", "scale"))

grid <- expand.grid(mfinal = (1:3)*3, maxdepth = c(1, 3))
set.seed(849)
models2$AdaBag <- train(trainX, trainY, 
                       method = 'AdaBag', 
                       trControl = cctrl1, 
                       tuneGrid = grid,  
                       preProc = c('center', 'scale'))

set.seed(849)
models2$CART <- train(trainX, trainY, 
                     method = 'rpart1SE', 
                     trControl = cctrl1, 
                     preProc = c("center", "scale"))

set.seed(849)
models2$kknn <- train(trainX, trainY, 
                     method = "kknn", 
                     trControl = cctrl1,
                     tuneLength = 4, 
                     preProc = c("center", "scale", 'knnImpute'))
beepr::beep('ready')
set.seed(849)
models2$FDA <- train(trainX, trainY, 
                    method = "fda", 
                    trControl = cctrl1,
                    preProc = c("center", "scale", 'knnImpute'), 
                    tuneLength = 4)

beepr::beep('ready')
set.seed(849)
models2$NonInf <- train(trainX, trainY, 
                      method = "null", 
                      trControl = cctrl1,
                      preProc = c("center", "scale", 'knnImpute'), 
                      tuneLength = 4)

set.seed(849)
models2$wsrf <- train(trainX, trainY, 
                      method = "wsrf", 
                      trControl = cctrl1,
                      preProc = c("center", "scale", 'knnImpute'), 
                      tuneLength = 4)

set.seed(849)
models2$NaiveBayes <- train(trainX, trainY, 
                            method = "naive_bayes", 
                            trControl = cctrl1,
                            preProc = c("center", "scale", 'knnImpute'), 
                            tuneLength = 4)
cctrl1 <- trainControl(method = "repeatedcv", number = 10, 
                       repeats = 3, 
                       returnResamp = "all",
                       classProbs = TRUE, 
                       summaryFunction = multiClassSummary, 
                       seeds = seeds, 
                       search = 'random', 
                       allowParallel = F)
set.seed(849)
models2$PART <- train(trainX, trainY, 
                      method = "PART", 
                      trControl = cctrl1,
                      preProc = c("center", "scale", 'knnImpute'), 
                      tuneLength = 4)
beepr::beep()

Pred_Probs2 <- lapply(models2, predict, newdata = testing[, -1],  
                     type = 'prob')

ordtest <- order(testing$Category)
orderedtest <- testing[ordtest, ]
Pred_Probs2 <- lapply(Pred_Probs2, function(l) {
  l <- l[ordtest, ]
  l$Actual <- as.numeric(orderedtest$Category)
  l$ID <- 1:nrow(l)
  return(l)})

y_axis <- data.frame(Ticks = which(!duplicated(orderedtest$Category)),
                     Labels = levels(orderedtest$Category))
y_axis$LabelPos <- diff(c(y_axis$Ticks, nrow(testing)))/2 + y_axis$Ticks
MeltedPredictions2 <- lapply(Pred_Probs2, reshape2::melt, 
                            id.vars = c('ID', 'Actual'),  
                            value.name = 'PredProb', 
                            variable.name = 'Category')

mapply(heat_map, MeltedPredictions2, names(MeltedPredictions2), SIMPLIFY = F)


Pred_Probs2 <- lapply(Pred_Probs2, function(l) {
  l$obs <- factor(l$Actual,
                  labels = levels(orderedtest$Category))
  return(l)})

# AdaBag2 <- predict(models2[[1]], testing[-1], type = 'prob')
# AdaBag2$pred <- predict(models2[[1]], testing[-1])
# AdaBag2$obs <- testing[, 1]
# 
# rpart2 <- predict(models2[[2]], testing[-1], type = 'prob')
# rpart2$pred <- predict(models2[[2]], testing[-1])
# rpart2$obs <- testing[, 1]
# 
# kknn2 <- predict(models2[[3]], testing[-1], type = 'prob')
# kknn2$pred <- predict(models2[[3]], testing[-1])
# kknn2$obs <- testing[, 1]
# 
# FDA2 <- predict(models2[[4]], testing[-1], type = 'prob')
# FDA2$pred <- predict(models2[[4]], testing[-1])
# FDA2$obs <- testing[, 1]
# 
# multiClassSummary(AdaBag2, lev = levels(AdaBag2$obs))
# multiClassSummary(rpart2, lev = levels(rpart2$obs))
# multiClassSummary(kknn2, lev = levels(kknn2$obs))
# multiClassSummary(FDA2, lev = levels(FDA2$obs))

stopCluster(cl)

# Pred_accuracy2 <- lapply(Pred_Probs2, function(l){
#   l[, 1:nlevels(l$obs)] <- lapply(l[, 1:nlevels(l$obs)], 
#                                   cut, breaks = c(0, .25, .50, .75, 1), 
#                                   include.lowest = T)
#   l <- droplevels(l)
#   return(l)
# })
# lapply(Pred_accuracy2, function(l){
#   p <- list()
#   stop()
#   for(i in 1:nlevels(l$obs)){
#     p[[i]] <- 
#       prop.table(table(l[, i], l$Actual == i), 1)[, 2]
#   }; remove(i)
#   names(p) <- levels(l$obs)
#   return(p)
# })


testing <- Analysis[-inTrain, ]
Pred_Probs3 <- lapply(models2, predict, newdata = testing[, -1],  
                      type = 'prob')

ordtest <- order(testing$Category)
orderedtest <- testing[ordtest, ]
Pred_Probs3 <- lapply(Pred_Probs3, function(l) {
  l <- l[ordtest, ]
  l$Actual <- as.numeric(orderedtest$Category)
  l$ID <- 1:nrow(l)
  return(l)})

y_axis <- data.frame(Ticks = which(!duplicated(orderedtest$Category)),
                     Labels = levels(orderedtest$Category))
y_axis$LabelPos <- diff(c(y_axis$Ticks, nrow(testing)))/2 + y_axis$Ticks
MeltedPredictions3 <- lapply(Pred_Probs3, reshape2::melt, 
                             id.vars = c('ID', 'Actual'),  
                             value.name = 'PredProb', 
                             variable.name = 'Category')

mapply(heat_map, MeltedPredictions3, names(MeltedPredictions3), SIMPLIFY = F)


Pred_Probs3 <- lapply(Pred_Probs3, function(l) {
  l$obs <- factor(l$Actual,
                  labels = levels(orderedtest$Category))
  return(l)})

# AdaBag3 <- predict(models2[[1]], testing[-1], type = 'prob')
# AdaBag3$pred <- predict(models2[[1]], testing[-1])
# AdaBag3$obs <- testing[, 1]
# 
# rpart3 <- predict(models2[[2]], testing[-1], type = 'prob')
# rpart3$pred <- predict(models2[[2]], testing[-1])
# rpart3$obs <- testing[, 1]
# 
# kknn3 <- predict(models2[[3]], testing[-1], type = 'prob')
# kknn3$pred <- predict(models2[[3]], testing[-1])
# kknn3$obs <- testing[, 1]
# 
# FDA3 <- predict(models2[[4]], testing[-1], type = 'prob')
# FDA3$pred <- predict(models2[[4]], testing[-1])
# FDA3$obs <- testing[, 1]

# multiClassSummary(AdaBag3, lev = levels(AdaBag3$obs))
# multiClassSummary(rpart3, lev = levels(rpart3$obs))
# multiClassSummary(kknn3, lev = levels(kknn3$obs))
# multiClassSummary(FDA3, lev = levels(FDA3$obs))

Pred_accuracy3 <- lapply(Pred_Probs3, function(l){
  l[, 1:4] <- lapply(l[, 1:4], 
                                  cut, breaks = c(0, .2, .4, .6, .8, 1), 
                                  include.lowest = T)
  return(l)
})
(lapply(Pred_accuracy3, function(l){
  p <- list()
  for(i in 1:(nlevels(l$obs)-1)){
    p[[i]] <- 
      prop.table(table(l[, i], l$Actual == i), 1)[, 2]
  }; remove(i)
  names(p) <- levels(l$obs)[-5]
  return(p)
}) -> ForGraph)
ForTable <- ForGraph$kknn
ForGraph <- mapply(function(l, name){
  l <- as.data.frame(l)
  l$Model <- name
  l$Bins <- levels(Pred_accuracy3[[1]]$Eggs)
  return(l)
}, ForGraph, names(ForGraph), SIMPLIFY = F)
ForGraph <- do.call(rbind, ForGraph)
ForGraph <- reshape2::melt(ForGraph, value, id.vars = c("Model", "Bins"), 
                           measure.vars = c("Eggs", "MeatPoultry", "Produce", 
                                            "AnimalContact"), 
                           value.name = 'PercentCorrect', 
                           variable.name = 'Outbreak Source')
ForGraph$Bins <- factor(ForGraph$Bins, 
                        levels = c("[0,0.2]", 
                                   "(0.2,0.4]",
                                   "(0.4,0.6]", 
                                   "(0.6,0.8]", 
                                   "(0.8,1]"), 
                        labels = seq(0.1, 0.9, 0.2))
ForGraph$Bins <- as.numeric(as.character(ForGraph$Bins))

ForGraph$Model <- factor(ForGraph$Model, 
                         levels = c('NonInf', 'AdaBag', 'CART', 'kknn', 'FDA', 
                                    'wsrf', 'NaiveBayes', 'PART'), 
                         labels = c('Non Informative Model', 
                                    'Bagged Adaptive Boosting Classification Trees', 
                                    'Classification and Regression Trees (CART)', 
                                    'Weighted k-Nearest Neighbors', 
                                    'Flexible Discriminant Analysis', 
                                    'Weighted Subspace Random Forest', 
                                    'Naive Bayes', 
                                    'Rule-Based Classifier'
                                    ))

ggplot(ForGraph, aes(x = Bins, y = PercentCorrect, group = `Outbreak Source`, 
                     colour = `Outbreak Source`)) + 
  geom_point() + geom_line() + facet_wrap(~ Model) + theme_classic() + 
  scale_x_continuous(breaks = seq(0.1, 0.9, 0.2)) + 
  scale_y_continuous(breaks = seq(0.1, 0.9, 0.2)) +
  labs(x = 'Bin Midpoint', 
       y = 'Observed Event Proportion', 
       title = 'Calibration Plots For All Models', 
       colour = 'Outbreak Source') + 
  geom_abline(slope = 1)  + 
  theme(legend.position = c(0.82, 0.13))
ggsave('Reports/Figures/CalibrationPlots.png', dpi = 300, width = 10, 
       height = 8, units = 'in')


BrierScore2 <- lapply(MeltedPredictions2, function(l){
  N <- nrow(l)
  R <- nlevels(l$Category)
  l$Category <- as.numeric(l$Category)
  l$y <- l$Actual == l$Category
  l$inner <- (l$y - l$PredProb)^2
  BS <- sum(l$inner)/N
})
BrierScore <- lapply(MeltedPredictions3, function(l){
  N <- nrow(l)
  R <- nlevels(l$Category)
  l$Category <- as.numeric(l$Category)
  l$y <- l$Actual == l$Category
  l$inner <- (l$y - l$PredProb)^2
  BS <- sum(l$inner)/N
})

save.image('DataProcessed/AnalysisWorkspace.Rdata')
finalchoice <- models2$kknn
save(finalchoice, file = 'SourceAttribution/knnnmodelobj.rda')
input_skeleton <- trainX[0, ]
save(input_skeleton, file = 'SourceAttribution/FileSkeleton.rda')

library(tidyverse)
ForGraph %>% filter(Model == 'Weighted k-Nearest Neighbors') %>% 
  spread(`Outbreak Source`, PercentCorrect) %>% select(-Model) %>%
  rename(Midpoint = Bins) -> CalibrationTable
CalibrationTable[, 2:5] <- round(CalibrationTable[, 2:5] * 100, 0)

Pred_accuracy3$kknn %>% 
  gather(key = 'PredSource', value = 'Prediction', 1:4) %>% 
  mutate(Correct = obs == PredSource) -> PredTbl
prop.table(table(PredTbl$Prediction, PredTbl$Correct), 1)
Correct <- prop.table(table(PredTbl$Prediction, PredTbl$Correct), 1)[c(5, 1:4), 
                                                                     2]
Pred_accuracy3$kknn %>% filter(MeatPoultry == "(0.8,1]") -> highMP
prop.table(table(highMP$obs))
Pred_accuracy3$kknn %>% filter(obs == 'Other')
save(BrierScore, testing, training, ForTable, Correct, 
     file = 'DataProcessed/Results.RData')
