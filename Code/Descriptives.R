###########################################
# Project: P1330White
# Author: David Weitzenkamp & Caroline Ledbetter
# Date: 08/15/2018
# #########################################

library(CIDAtools)
load(file = 'DataProcessed/DataClean.RData')
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
Analysis$Category <- factor(Analysis$Category, 
                            levels = c("Eggs", 
                                       "Meat", 
                                       "Poultry", 
                                       "Produce", 
                                       "AnimalContact", 
                                       'Other'), 
                            labels = c("Eggs", 
                                       "Meat-Poultry", 
                                       "Meat-Poultry", 
                                       "Produce", 
                                       "Animal Contact", 
                                       'Other'))
Analysis$Season <- factor(Analysis$Season, 
                          levels = c('Winter', 
                                     'Spring', 
                                     'Summer', 
                                     'Fall'), 
                          labels = c('Winter (Jan-Mar)', 
                                     'Spring (Apr-Jun)', 
                                     'Summer (Jul-Sep)', 
                                     'Fall (Oct-Dec)'))
Analysis$Agent <- factor(Analysis$Agent, 
                         levels = c("STEC", 
                                    "Braenderup", 
                                    "Enteritidis", 
                                    "Heidelberg", 
                                    "I 4,[5],12:i:-", 
                                    "Javiana", 
                                    "Montevideo", 
                                    "Newport", 
                                    "Paratyphi B", 
                                    "Saintpaul", 
                                    "Typhimurium", 
                                    "NonSpecific Sero group", 
                                    "Primary Animal Sero group", 
                                    "Primary Plant Sero group", 
                                    "Rare", 
                                    "Salm unk sero", 
                                    "Multiple Serotypes"), 
                         labels = c("STEC", 
                                    "Braenderup", 
                                    "Enteritidis", 
                                    "Heidelberg", 
                                    "I 4,[5],12:i:-", 
                                    "Javiana", 
                                    "Montevideo", 
                                    "Newport", 
                                    "Paratyphi B", 
                                    "Saintpaul", 
                                    "Typhimurium", 
                                    "Nonspecific Salmonella Serotype", 
                                    "Primary Animal Salmonella Serotype", 
                                    "Primary Plant Salmonella Serotype", 
                                    "Rare Salmonella Serotype", 
                                    "Unknown Salmonella Serotype", 
                                    "Multiple Salmonella Serotypes"))
Analysis$Geography <- factor(Analysis$Geography, 
                             levels = c("MultiState", 
                                        "MultiCounty",
                                        'SingleCounty', 
                                        'Missing'), 
                             labels = c("Multi-State", 
                                        "Multi-County",
                                        'Single County', 
                                        'Missing'))
Analysis$HospPercent2 <- Analysis$HospPercent2*100
tbl1 <- Table1(Analysis, rowvars = c(TotalCases, Season, Agent, PercentMale, 
                             PercentFemale, PercentSexUnknown, 
                             PercentAgeUnder1, PercentAge1to4, 
                             PercentAge5to9, PercentAge10to19, 
                             PercentAge20to49, PercentAge50to74, 
                             PercentAge75plus, PercentAgeUnknown, 
                             HospPercent2, Geography), 
               Category, 
               rowvar_names =  c("Total Cases", "Season", "Agent", 
                                 "% Male", 
                                 "% Female", "% Sex Unknown", 
                                 "% Age Under1", "% Age 1 to 4", 
                                 "% Age 5 to 9", "% Age 10 to 19", 
                                 "% Age 20 to 49", "% Age 50 to 74", 
                                 "% Age 75 and over", "% Age Unknown", 
                                 "% Hospitalized", 
                                 "Geography"), sigfig = 2)
save(tbl1, file = 'DataProcessed/Table1.R')
