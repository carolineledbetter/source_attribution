###########################################
# Project: P1330White
# Author: David Weitzenkamp & Caroline Ledbetter
# Date: 08/15/2018
# #########################################

library(CIDAtools)
load(file = 'DataProcessed/DataClean.RData')
table1 <- Table1(Analysis, -c(CDCID, Category, Category2), 
                 Category2, sigfig = 2)
table(Analysis$DeathsNum, useNA = 'ifany')
summary(Analysis$TotalCases)
library(doBy)
summaryBy(TotalCases ~ Category2, Analysis, FUN = list(mean, quantile), 
          probs = c(0.25, 0.5, 0.75))
table(Analysis$TotalCases[Analysis$Category2 == 'NutsSeeds'])
table(Analysis$TotalCases, Analysis$Category2)
Analysis$Size <- cut(Analysis$TotalCases, breaks = c(0, 10, 50, 100, 
                                                     200, 500, 1000, 
                                                     2000, Inf))
table(Analysis$Size, Analysis$Category2)
