###########################################
# Project: P1330White
# Author: Caroline Ledbetter
# Date: 07/30/2018
# #########################################
library(CIDAtools)
library(mice)
library(VIM)
library(ggplot2)

load('DataRaw/WHIT_20180502_NoWater.RData')

################################################################################
# Get length of outbreak
NORSMain$OutbreakLength <- difftime(NORSMain$LastExposure, 
                                    NORSMain$InitialExposure, 
                                    units = 'days')
sum(is.na(NORSMain$OutbreakLength))


################################################################################
# Get outbreak agent (Salmonella or STEC)
Ecoli <- aggregate(GenusName ~ CDCID, data = GenEtiology, 
                   function(x) any(x == 'Escherichia'))
Salmonella <- aggregate(GenusName ~ CDCID, data = GenEtiology, 
                        function(x) any(x == 'Salmonella'))
NORSMain$STEC <- Ecoli$GenusName
NORSMain$Salmonella <- Salmonella$GenusName
NORSMain$SalmSTEC <- NA
NORSMain$SalmSTEC[NORSMain$STEC] <- 'STEC'
NORSMain$SalmSTEC[NORSMain$Salmonella] <- 'Salmonella'
table(NORSMain$SalmSTEC, useNA = 'ifany')

################################################################################
# Get Outbreak Season
NORSMain$Season <- quarters(NORSMain$DateFirstIll) 
NORSMain$Season <- factor(NORSMain$Season, 
                          levels = c('Q1', 'Q2', 'Q3', 'Q4'), 
                          exclude = 'QNA', 
                          labels = c('Winter', 'Spring', 'Summer', 'Fall'))

table(NORSMain$Season, useNA = 'ifany')
################################################################################
# Get food source
IFSAC <- `__IFSACCommodityData`
IFSAC[, 8:14] <- lapply(IFSAC[, 8:14], factor)
IFSAC <- droplevels(IFSAC)

# Only include outbreaks with an identifiable single source
Analysis <- 
  IFSAC[IFSAC$IFSACLevel1 %in% c('Land Animals', 'Plant', 'Other', 
                                 'Aquatic Animals'), ]
n_unident_multi <- nrowP(IFSAC[!IFSAC$IFSACLevel1 %in% 
                                c('Land Animals', 'Plant', 'Other', 
                                  'Aquatic Animals'), ])
(IFSAC_LVL1 <- table(IFSAC$IFSACLevel1))
(IFSAC_LVL2 <- table(IFSAC$IFSACLevel2))
Analysis <- droplevels(Analysis)
table(Analysis$IFSACLevel2)

Analysis$Category <- factor(NA, 
                            levels = c('Eggs', 
                                       'Meat', 'Poultry', 'Produce',
                                       'AnimalContact', 'Other'))
Analysis$Category[!Analysis$IFSACLevel2 %in% 
                    c('Meat-Poultry') ] <- 
  Analysis$IFSACLevel2[!Analysis$IFSACLevel2 %in% 
                         c('Meat-Poultry') ]

Analysis$Category[Analysis$IFSACLevel2 %in% 
                    c('Meat-Poultry') ] <- 
  Analysis$IFSACLevel3[Analysis$IFSACLevel2 %in% 
                         c('Meat-Poultry') ]


Analysis$Category[is.na(Analysis$Category)] <- 'Other'
table(Analysis$Category, useNA = 'ifany')
table(Analysis$IFSACLevel1, Analysis$Category, useNA = 'ifany')
table(Analysis$IFSACLevel2, Analysis$Category, useNA = 'ifany')
table(Analysis$IFSACLevel3, Analysis$Category, useNA = 'ifany')

################################################################################
# Include only outbreaks with identified food source or animal contact

SelectNORS <- subset(NORSMain, CDCID %in% Analysis$CDCID | 
                       PrimaryMode == 'Animal Contact')
Analysis <- merge(Analysis, SelectNORS, by = 'CDCID', all = T)
Analysis$Category[Analysis$PrimaryMode == 'Animal Contact'] <- "AnimalContact"
Analysis$OutbreakLength <- as.numeric(Analysis$OutbreakLength)

Logical <- grep('^Multi.+Exposure|^Multi.+Residence', names(Analysis))
names(Analysis)[Logical]
Analysis[, Logical] <- lapply(Analysis[, Logical], as.logical)
Analysis$HospPercent <- Analysis$HospitalNum/Analysis$HospitalInfo
Analysis$HospPercent2 <- Analysis$HospitalNum/Analysis$EstimatedPrimary
Analysis$DeathsPct <- Analysis$DeathsNum/Analysis$EstimatedPrimary

Analysis$PercentMale[is.na(Analysis$PercentMale)] <- 0
Analysis$PercentFemale[is.na(Analysis$PercentFemale)] <- 0
Analysis$PercentSexUnknown[is.na(Analysis$PercentSexUnknown)] <- 100 - 
  apply(subset(Analysis, is.na(PercentSexUnknown), select = c(PercentFemale, 
                                                              PercentMale)), 
        1, sum)


################################################################################
# Serotype for Salmonella outbreaks
Analysis$Agent <- Analysis$SalmSTEC
SalmSero <- subset(GenEtiology, GenusName == 'Salmonella', 
                   select = c(CDCID, SerotypeName))
SalmSero <- SalmSero[!duplicated(SalmSero), ]
sum(duplicated(SalmSero$CDCID))
# one line per outbreak
SalmSeroOne <- SalmSero[!duplicated(SalmSero$CDCID), ]
# set outbreaks with more than one serotype to multiple serotypes
multiType <- SalmSero$CDCID[duplicated(SalmSero$CDCID)]
SalmSeroOne$SerotypeName[SalmSeroOne$CDCID %in% multiType] <- 
  'Multiple Serotypes'

Analysis <- merge(Analysis, SalmSeroOne, by = 'CDCID', all.x = T)
Analysis$Agent[Analysis$SalmSTEC == 'Salmonella'] <- 
  Analysis$SerotypeName[Analysis$SalmSTEC == 'Salmonella']
table(Analysis$Agent, useNA = 'ifany')
Analysis$Agent[Analysis$Agent == 'unknown'] <- 'Salm unk sero'
table(Analysis$Agent, useNA = 'ifany')
table(Analysis$Agent, Analysis$SalmSTEC, useNA = 'ifany')
Analysis$Agent[is.na(Analysis$Agent) & Analysis$Salmonella] <- 'Salm unk sero'
table(Analysis$Agent, Analysis$SalmSTEC, useNA = 'ifany')

Analysis <- droplevels(Analysis)
Agents <- table(Analysis$Agent)
(Agents <- dimnames(Agents[Agents > 20])[[1]])
NonSpecific <- c('Agona', 'Anatum','Berta', 'Mbandaka', 'Muenchen', 
                 'Stanley', 'Thompson')
PrimaryAnimal <- c('Derby', 'Group B', 'Hadar', 'Infantis', 'Johannesburg', 
                   'Oranienburg', 'Reading', 'Sandiego', 
                   'Typhimurium var Cope', 'Uganda')
PrimaryPlant <- c('Cubana', 'Poona', 'Senftenberg', 'Virchow')
# RareAgents <- subset(Analysis, Agent %in% Agents & (IFSACLevel1 %in% 
#                        c('Land Animals', 'Plant', 'Aquatic Animals') | 
#                        Category == "AnimalContact"), 
#                      select = c(IFSACLevel1, Agent))
# RareAgents$IFSACLevel1 <- factor(RareAgents$IFSACLevel1, 
#                                  levels = c('Plant', 'Aquatic Animals', 
#                                             'Land Animals', 'AnimalContact'))
# RareAgents$IFSACLevel1[is.na(RareAgents$IFSACLevel1)] <- 'AnimalContact'
# RareAgents <- RareAgents[order(RareAgents$IFSACLevel1), ]
# ggplot(RareAgents, aes(x = Agent, y = IFSACLevel1)) + geom_count() + 
#   theme_classic()

Analysis$Agent[Analysis$Agent %in% NonSpecific] <- 'NonSpecific Sero group'
Analysis$Agent[Analysis$Agent %in% PrimaryPlant] <- 'Primary Plant Sero group'
Analysis$Agent[Analysis$Agent %in% PrimaryAnimal] <- 'Primary Animal Sero group'
Analysis$Agent <- factor(Analysis$Agent, levels = c(Agents, 'Paratyphi B', 
                                                    'NonSpecific Sero group', 
                                                    'Primary Plant Sero group', 
                                                    'Primary Animal Sero group', 
                                                    'Rare'))
Analysis$Agent[is.na(Analysis$Agent)] <- 'Rare'
table(Analysis$Agent, useNA = 'ifany')

vars <- grep('Percent|Deaths|Hospital', names(Analysis))
vars <- c(which(names(Analysis) %in% c("CDCID", 'Agent', 
                                       "Category", 
                                       "MultiCountyExposure", 
                                       'MultiStateExposure', 
                                       'TotalCases', 
                                       'OutbreakLength', 
                                       'Season', 'SalmSTEC')), 
          vars)
names(Analysis)[vars]
Analysis <- subset(Analysis, select = vars)

################################################################################
# set Age variables that are missing to zero if other age variables
# add up to 99 or greater

# get names of age variables
AgeVars <- grep('Age[0-9]|U', names(Analysis))
# get sums of age variables
Age <- apply(subset(Analysis, select = AgeVars), 1, sum, na.rm = T)
# are any missing
AgeMis <- apply(subset(Analysis, select = AgeVars), 1, 
                function(x) any(is.na(x)))

# set to 0 those that are missing, where the sum is greater than 99
Analysis[, AgeVars][is.na(Analysis[, AgeVars]) & Age >=99 & AgeMis] <- 0
Age <- apply(subset(Analysis, select = AgeVars), 1, sum, na.rm = T)

# check
AgeMis <- apply(subset(Analysis, select = AgeVars), 1, 
                function(x) any(is.na(x)))
Age[Age >= 99 & AgeMis]
# checks
AgeVars <- grep('Age[0-9]|Under1', names(Analysis))
Analysis[, AgeVars][is.na(Analysis[, AgeVars])] <- 0
Analysis$PercentAgeUnknown[is.na(Analysis$PercentAgeUnknown)] <- 
  100 - apply(subset(Analysis, is.na(PercentAgeUnknown), 
                     select = AgeVars), 1, sum)

# aggr_plot <- aggr(Analysis, col=c('navyblue','red'), numbers=TRUE, 
#                   sortVars=TRUE, labels=names(Analysis), cex.axis=.7, 
#                   gap=3, ylab=c("Histogram of missing data","Pattern"))
marginplot(Analysis[, c("Category", "PercentAgeUnknown")])
table(Analysis$Category, is.na(Analysis$PercentAgeUnknown))
prop.table(table(Analysis$Category, is.na(Analysis$PercentAgeUnknown)), 1)

table(Analysis$Season, useNA = 'ifany')

save(Analysis, file = 'DataProcessed/DataClean.RData')

