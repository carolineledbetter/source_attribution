###########################################
# Project: P1330White
# Author: Caroline Ledbetter
# Date: 06/07/2018
# #########################################
library(ggplot2)
library(CIDAtools)

load('DataRaw/WHIT_20180502_NoWater.RData')
summary(NORSMain)
NORSMain$OutbreakLength <- difftime(NORSMain$LastExposure, 
                                    NORSMain$InitialExposure, 
                                    units = 'days')
sum(is.na(NORSMain$OutbreakLength))


Ecoli <- aggregate(GenusName ~ CDCID, data = GenEtiology, 
                  function(x) any(x == 'Escherichia'))
Salmonella <- aggregate(GenusName ~ CDCID, data = GenEtiology, 
                        function(x) any(x == 'Salmonella'))
NORSMain$STEC <- Ecoli$GenusName
NORSMain$Salmonella <- Salmonella$GenusName
NORSMain$SalmSTEC <- NA
NORSMain$SalmSTEC[NORSMain$STEC] <- 'STEC'
NORSMain$SalmSTEC[NORSMain$Salmonella] <- 'Salmonella'
table(NORSMain$SalmSTEC)
# tbl <- Table1(c(vars, 'Season'), 'SalmSTEC', NORSMain, incl_missing = T, incl_pvalues = F)
# save(tbl, file = 'DataProcessed/tabl1.rda')

NORSMain$Init2 <- as.POSIXlt.POSIXct(NORSMain$InitialExposure)
NORSMain$Season <- quarters.POSIXt(NORSMain$Init2) 
NORSMain$Season <- factor(NORSMain$Season, 
                          levels = c('Q1', 'Q2', 'Q3', 'Q4'), 
                          exclude = 'QNA', 
                          labels = c('Winter', 'Spring', 'Summer', 'Fall'))
table(NORSMain$Season)

summary(`__IFSACCommodityData`)
table(`__IFSACCommodityData`$IFSAC_MMWR)
attach(`__IFSACCommodityData`)
varIFSAC <- grep('IFSAC', ls('__IFSACCommodityData'), value = T)
detach(`__IFSACCommodityData`)

lapply(`__IFSACCommodityData`[, varIFSAC], table)


IFSAC <- `__IFSACCommodityData`
IFSAC[, 8:14] <- lapply(IFSAC[, 8:14], factor)
IFSAC <- droplevels(IFSAC)
Analysis <- IFSAC[IFSAC$IFSACLevel1 %in% c('Aquatic Animals', 'Land Animals', 'Plant'), ]
table(IFSAC$IFSACLevel1, useNA = 'ifany')
head(IFSAC[is.na(IFSAC$IFSACLevel1), ], 20)
tail(IFSAC[is.na(IFSAC$IFSACLevel1), ], 20)
Analysis <- Analysis[!Analysis$IFSACLevel2 %in% c('Game', 'Oils-sugars', 'Other Aquatic Animals'), ]
Analysis$Category <- factor(NA, 
                            levels = c('Fish', 'Shell Fish', 'Eggs', 
                                       'Fluid milk', 
                                       'Solid/semi-solid dairy products', 
                                       'Meat', 'Poultry', 'Seeded Vegetables', 
                                       'Vegetable Row Crops', 'Fruits', 
                                       'Grains-beans', 'Nuts-seeds', 
                                       'Animal Contact'))
Analysis$Category[!Analysis$IFSACLevel2 %in% 
                    c('Dairy', 'Produce', 'Meat-Poultry') ] <- 
  Analysis$IFSACLevel2[!Analysis$IFSACLevel2 %in% 
                         c('Dairy', 'Produce', 'Meat-Poultry') ]
lapply(Analysis[Analysis$IFSACLevel2 == 'Eggs', 8:14], table)
Analysis[Analysis$IFSACLevel2 == 'Eggs' & !is.na(Analysis$IFSACLevel2), ]
table(Analysis$IFSACLevel2)
table(Analysis$IFSACLevel4)
table(Analysis$IFSACLevel4[Analysis$IFSACLevel2 == 'Produce'])
table(Analysis$IFSACLevel3[Analysis$IFSACLevel2 == 'Produce'])
table(Analysis$IFSACLevel4[Analysis$IFSACLevel3 == 'Vegetables'])
table(Analysis$IFSACLevel5[Analysis$IFSACLevel3 == 'Vegetables'])
table(Analysis$IFSACLevel5[Analysis$IFSACLevel3 == 'Fruits'])
table(Analysis$IFSACLevel4[Analysis$IFSACLevel3 == 'Fruits'])

Analysis$Category[Analysis$IFSACLevel2 %in% 
                    c('Dairy', 'Produce', 'Meat-Poultry') ] <- 
  Analysis$IFSACLevel3[Analysis$IFSACLevel2 %in% 
                      c('Dairy', 'Produce', 'Meat-Poultry') ]
Analysis$Category[Analysis$IFSACLevel3 %in% 'Vegetables'] <- 
  Analysis$IFSACLevel5[Analysis$IFSACLevel3 %in% 'Vegetables']
length(unique(Analysis$CDCID))
table(FB_FoodMain$FoodVehicleUndetermined)

Analysis <- subset(Analysis, !is.na(Category))
table(Analysis$Category, useNA = 'ifany')
SelectNORS <- subset(NORSMain, CDCID %in% Analysis$CDCID | 
                       PrimaryMode == 'Animal Contact')
Analysis <- merge(Analysis, SelectNORS, by = 'CDCID', all = T)
Analysis$Category[Analysis$PrimaryMode == 'Animal Contact'] <- "Animal Contact"
Analysis$OutbreakLength <- as.numeric(Analysis$OutbreakLength)

Logical <- grep('^Multi.+Exposure|^Multi.+Residence', names(Analysis))
names(Analysis)[Logical]
Analysis[, Logical] <- lapply(Analysis[, Logical], as.logical)
Analysis$HospPercent <- Analysis$HospitalNum/Analysis$HospitalInfo
Analysis$HospPercent2 <- Analysis$HospitalNum/Analysis$EstimatedPrimary


Analysis$PercentSexUnknown[is.na(Analysis$PercentSexUnknown)] <- 100 - 
  apply(subset(Analysis, is.na(PercentSexUnknown), select = c(PercentFemale, 
                                                              PercentMale)), 
        1, sum)
Analysis$Agent <- Analysis$SalmSTEC
SalmSero <- subset(GenEtiology, GenusName == 'Salmonella', 
                   select = c(CDCID, SerotypeName))
SalmSero <- SalmSero[!duplicated(SalmSero), ]
sum(duplicated(SalmSero$CDCID))
SalmSeroOne <- SalmSero[!duplicated(SalmSero$CDCID), ]
multiType <- SalmSero$CDCID[duplicated(SalmSero$CDCID)]
SalmSeroOne$SerotypeName[SalmSeroOne$CDCID %in% multiType] <- 'Multiple Serotypes'

vars <- grep('Percent', names(Analysis))
vars <- c(vars, which(names(Analysis) %in% c("MultiCountyExposure", 
                                             'MultiStateExposure', 'PrimaryMode',
                                             'TotalCases', 
                                             'OutbreakLength', 
                                             'Season', 'SalmSTEC', 'Agent')))
names(Analysis)[vars]


Analysis <- merge(Analysis, SalmSeroOne, by = 'CDCID', all.x = T)
Analysis$Agent[Analysis$SalmSTEC == 'Salmonella'] <- 
  Analysis$SerotypeName[Analysis$SalmSTEC == 'Salmonella']
table(Analysis$Agent)

vars <- grep('Percent', names(Analysis))
vars <- c(vars, which(names(Analysis) %in% c("MultiCountyExposure", 
                                             'MultiStateExposure', 
                                             'TotalCases', 
                                             'OutbreakLength', 
                                             'Season', 'SalmSTEC', 'Agent')))
names(Analysis)[vars]
Analysis <- subset(Analysis, !is.na(Category))
Analysis <- droplevels(Analysis)
Missing <- which(is.na(Analysis$Agent))
Agents <- table(Analysis$Agent)
Agents <- dimnames(Agents[Agents > 20])[[1]]
Blue <- c('Derby', 'Dublin', 'Give', 'Goldcoast', 'Group B', 'Group D1', 
          'Hadar', 'Infantis', 'Mbandaka', 'Montevideo', 'Oranienburg', 
          'Reading', 'Stanley', 'Typhimurium var Cope', 'Uganda')
Greeen <- c('Javiana', 'Poona', 'Senftenberg')
Red <- c('Agona', 'Anatum','Berta', 'Braenderup', 'Muenchen', 'Saintpaul', 
         'Thompson')
Analysis$Agent[Analysis$Agent %in% Blue] <- 'Blue'
Analysis$Agent[Analysis$Agent %in% Green] <- 'Green'
Analysis$Agent[Analysis$Agent %in% Red] <- 'Red'
Analysis$Agent <- factor(Analysis$Agent, levels = c(Agents, 'Paratyphi B', 
                                                    'Blue', 'Red', 'Green', 
                                                    'Rare'))
Analysis$Agent[is.na(Analysis$Agent)] <- 'Rare'
Analysis$Agent[Missing] <- NA


tbl <- Table1(Analysis, vars, Category, incl_missing = T, incl_pvalues = F, 
              rowvar_names = c('Percent Male', 'Percent Female', 
                               'Percent Unknown Sex', 'Perenct Under 1', 
                               'Percent 1 to 4', 'Percent 5 to 9', 
                               'Percent 10 to 19', 'Percent 20 to 49', 
                               'Percent 50 to 74', 'Percent 75 and over', 
                               'Percent Unknown Age', 'Percent Hospitalized', 
                               'Percent Hospitalized (T)', 
                               'Multi State', 'Multi County', 'Total Cases', 
                               'Outbreak Length(Days)', 'Salmonella or STEC',  
                               'Season', 'Salm Serotype or STEC'), sigfig = 2)
save(tbl, file = 'DataProcessed/tabl1.rda')
Blue <- c('Derby', 'Dublin', 'Give', 'Goldcoast', 'Group B', 'Group D1', 
          'Hadar', 'Infantis', 'Mbandaka', 'Montevideo', 'Oranienburg', 
          'Reading', 'Stanley', 'Typhimurium var Cope', 'Uganda')
Greeen <- c('Javiana', 'Poona', 'Senftenberg')
Red <- c('Agona', 'Anatum','Berta', 'Braenderup', 'Muenchen', 'Saintpaul', 
         'Thompson')