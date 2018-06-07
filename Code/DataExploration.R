###########################################
# Project: P1330White
# Author: Caroline Ledbetter
# Date: 06/07/2018
# #########################################
library(ggplot2)

load('DataRaw/NORS1998_2014.RData')
summary(NORSMain)
NORSMain$OutbreakLength <- difftime(NORSMain$LastExposure, 
                                    NORSMain$InitialExposure, 
                                    units = 'days')
sum(is.na(NORSMain$OutbreakLength))
ggplot(data = NORSMain) + 
  geom_bar(aes(x = OutbreakLength), col = 'blue', fill = 'blue') + 
  coord_cartesian(xlim = c(0, 100)) + 
  theme_classic()
nrow(NORSMain[!is.na(NORSMain$OutbreakLength), 
         c("InitialExposure", "LastExposure")]) 
table(NORSMain$SalmSTEC)
table(NORSMain$MultiGenus)
table(NORSMain$DeathsNum)

attach(NORSMain)
vars <- grep('Percent', ls(NORSMain), value = T)
detach(NORSMain)
vars <- c(vars, "MultiCountyExposure", 'MultiStateExposure', 'PrimaryMode', 
          'TracebackConducted', 'TotalCases', 'OutbreakLength')
charvars <- sapply(NORSMain[, vars], is.character)
charvars <- vars[charvars]
NORSMain[, charvars] <- lapply(NORSMain[, charvars, drop = F], factor)
source(file = '~/Repositories/P1180Hoppe/Code/Table1.R')

head(NORSMain)
library(psych)
describe(NORSMain)
quantile(NORSMain$TotalCases)
table(NORSMain$RecordStatus)
table(table(GenEtiology$CDCID))
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
tbl <- Table1(c(vars, 'Season'), 'SalmSTEC', NORSMain, incl_missing = T, incl_pvalues = F)
save(tbl, file = 'DataProcessed/tabl1.rda')

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
