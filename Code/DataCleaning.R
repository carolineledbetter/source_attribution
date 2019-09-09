### Project Info #######################################
# Project: P1330White
# Author: Caroline Ledbetter
# Date: 07/30/2018
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


library(tidyverse)
library(lubridate)

load('DataRaw/WHIT_20180502_NoWater.RData')

NORSMain %>% as_tibble() %>%  
  mutate(
    outbreak_length = (LastExposure - InitialExposure)/ddays(), 
    season = quarters(DateFirstIll), 
    season = factor(season, 
                    levels = c('Q1', 'Q2', 'Q3', 'Q4'), 
                    exclude = 'QNA', 
                    labels = c('Winter', 'Spring', 'Summer', 'Fall')), 
    month = month(DateFirstIll), 
    geography = case_when(
      MultiStateExposure  == 1 ~ 'multi_state', 
      MultiCountyExposure == 1 ~ 'multi_county', 
      MultiCountyExposure == 0 ~ 'single_county'
    ), 
    hosp_percent = HospitalNum/HospitalInfo, 
    death_percent = DeathsNum/DeathsInfo, 
    
  ) %>% 
  select(-matches('^Percent.+Unknown$')) %>% 
  rowwise() %>% 
  mutate(PercentSex = sum(PercentMale, PercentFemale, na.rm = T),
         PercentMale = PercentMale/PercentSex*100, 
         PercentFemale = PercentFemale/PercentSex*100, 
         PercentAge = sum(PercentAgeUnder1, 
                          PercentAge1to4, 
                          PercentAge5to9, 
                          PercentAge10to19,  
                          PercentAge20to49,  
                          PercentAge50to74,  
                          PercentAge75plus, na.rm = T)
  ) %>% 
  ungroup() %>% 
  mutate_at(vars(matches('Age[U1-9]+')), ~ ./PercentAge*100) %>% 
  mutate_at(vars(matches('^Percent(Fe)*[Mm]ale$')), 
            ~ if_else(PercentSex == 0, 
                      ., 
                      replace_na(., 0L))) %>% 
  mutate_at(vars(matches('^PercentAge[U1-9]+')), 
            ~ if_else(PercentAge == 0, 
                      ., 
                      replace_na(., 0L))) %>% 
  select(-PercentAge, -PercentSex) %>% 
  janitor::clean_names() -> NORSMain
  
    
# Outbreak agent (Salmonella or STEC) -------------------------------------
GenEtiology <- 
  GenEtiology %>% 
  mutate(SerotypeName = replace_na(SerotypeName, 'unknown'), 
         SerotypeName = case_when(
           SerotypeName != 'unknown' ~ SerotypeName, 
           str_detect(OtherCharacteristics, '4[,\\[ ]*5[,\\] ]*12[,\\ :]*') ~ 
             "I 4,[5],12:i:-", 
           str_detect(OtherCharacteristics, 'Oranienburg') ~ "Oranienburg", 
           str_detect(OtherCharacteristics, "	Javiana") ~ "	Javiana", 
           str_detect(OtherCharacteristics, "Bofflens") ~ "Bofflens", 
           str_detect(OtherCharacteristics, "Bovismorbificans") ~ 
             "Bovismorbificans", 
           str_detect(OtherCharacteristics, "Braenderup") ~ "Braenderup", 
           str_detect(OtherCharacteristics, "Mbandaka") ~ "Mbandaka", 
           str_detect(OtherCharacteristics, "Stanley") ~ "Stanley", 
           str_detect(OtherCharacteristics, "Uganda") ~ "Uganda", 
           str_detect(OtherCharacteristics, "Montevideo") ~ "Montevideo", 
           str_detect(OtherCharacteristics, "typhimurium") ~ "Typhimurium", 
           str_detect(OtherCharacteristics, 'Berta') ~ "Berta", 
           str_detect(OtherCharacteristics, 'BERTA') ~ 'Berta', 
           str_detect(OtherCharacteristics, "B\\b") ~ "Group B", 
           str_detect(OtherCharacteristics, "C1") ~ "Group C1", 
           str_detect(OtherCharacteristics, "C2") ~ "Group C2", 
           str_detect(OtherCharacteristics, "D1") ~ "Group D1", 
           str_detect(OtherCharacteristics, "E1") ~ "Group E1" 
         ) 
  ) 

GenEtiology %>% 
  filter(GenusName %in% c('Escherichia', 'Salmonella')) %>% 
  arrange(Confirmed, desc(NumberLabConfirmed)) %>% 
  mutate(genus = GenusName, 
         serotype = if_else(GenusName == 'Escherichia', 
                            'STEC',
                            SerotypeName)) %>% 
  select(cdcid = CDCID, genus, serotype) %>% 
  distinct(cdcid, .keep_all = TRUE) -> agent

agent %>% count(genus, serotype) %>% view

# Serotype for Salmonella outbreaks --------------------------------------------

# Food source ---------------------------------------------------------------
IFSAC <- `__IFSACCommodityData`


# Only include outbreaks with an identifiable single source
IFSAC %>% count(IFSACLevel1, IFSACLevel2, IFSACLevel3) %>% view

analysis <- 
  IFSAC %>% 
  filter(IFSACLevel1 %in% c("Aquatic Animals", 
                            "Land Animals", 
                            "Other", 
                            "Plant")
         ) %>% 
  mutate(food_source = case_when(
    IFSACLevel2 == "Dairy" ~ "Dairy", 
    IFSACLevel2 == "Eggs" ~ "Eggs", 
    IFSACLevel3 == "Meat" ~ "Meat", 
    IFSACLevel3 == "Poultry" ~ "Poultry", 
    IFSACLevel3 == "Fruits" ~ "Fruits", 
    IFSACLevel3 == "Vegetables" ~ "Vegetables", 
    IFSACLevel2 == "Meat-Poultry" ~ "Meat-Poultry Other", 
    IFSACLevel2 == "Produce" ~ "Produce Other", 
    TRUE ~ "Other"
  )) %>% 
    select(cdcid = CDCID, food_source)




# save -------------------------------------------------------------------------
analysis <- 
  NORSMain %>% select(cdcid, starts_with("percent"), 
                      total_cases, month, hosp_percent, geography, 
                      primary_mode) %>% 
  left_join(agent) %>% 
  right_join(x = analysis, y = .) %>% 
  mutate(attr_source = case_when(
    primary_mode == 'Animal Contact' ~ 'Animal Contact', 
    TRUE ~ food_source)) %>% 
  filter(!is.na(attr_source)) %>% 
  select(-food_source, -primary_mode) 

analysis %>% 
  select(serotype, attr_source) %>% 
  add_count(serotype) %>% 
  filter(n > 3  & n < 10) %>%
  mutate(attr_source = fct_collapse(attr_source, 
                                  Animal = 
                                    c("Animal Contact", 
                                      "Dairy", 
                                      "Eggs", 
                                      "Meat", 
                                      "Poultry", 
                                      "Meat-Poultry Other"), 
                                  Plant = 
                                    c("Fruits", 
                                      "Vegetables", 
                                      "Produce Other")
  )
  ) %>% 
  select(-n) -> uncommon_sero 
fit_serotype <- glm(attr_source ~ serotype, data = uncommon_sero, 
                    family = 'binomial')
uncommon_sero <- uncommon_sero %>% 
  mutate(pred_source = 
           predict(fit_serotype, 
                   newdata = uncommon_sero, 
                   type = 'response'), 
         serogroup = case_when(
           pred_source <= 1/3 ~ 'Group1', 
           pred_source <= 2/3 ~ 'Group2', 
           TRUE ~ 'Group3'
         )
  ) %>% select(serotype, serogroup) %>% 
  distinct()

analysis <- 
  analysis %>% 
  left_join(uncommon_sero) %>% 
  add_count(serotype) %>% 
  mutate(serotype = case_when(
    n <= 3 ~ 'rare', 
    !is.na(serogroup) ~ serogroup, 
    TRUE ~ serotype)) %>% 
  select(-serogroup, -n)

save(analysis, file = 'DataProcessed/cleaned_model.RData')

