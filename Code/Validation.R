### Project Info #######################################
# Project: P1330White
# Author: Caroline Ledbetter
# Date: 07/30/2018
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

library(CIDAtools)
library(tidyverse)
library(lubridate)

load('DataRaw/WHIT_20190325_NoWater.RData')
NORSMain %>% glimpse()


NORSMain <- 
  NORSMain %>% as_tibble() %>% 
  filter(year(DateFirstIll) == 2017) %>% 
  mutate(CDCID = as.character(CDCID), 
         PercentAge5to19 = sum_ignore_NA(PercentAge5to9, 
                                         PercentAge10to19), 
         PercentAge50plus = sum_ignore_NA(PercentAge50to74, 
                                          PercentAge75plus), 
         month = month(DateFirstIll), 
         year  = year(DateFirstIll), 
         geography = case_when(
           MultiStateExposure  == 1 ~ 'multi_state', 
           MultiCountyExposure == 1 ~ 'multi_county', 
           MultiCountyExposure == 0 ~ 'single_county'
         ), 
         hosp_percent = HospitalNum/HospitalInfo, 
         death_percent = DeathsNum/DeathsInfo, 
         PercentSex = sum_ignore_NA(PercentMale, PercentFemale),
         PercentMale = PercentMale/PercentSex*100, 
         PercentFemale = PercentFemale/PercentSex*100, 
         PercentAge = sum_ignore_NA(PercentAgeUnder1, 
                                    PercentAge1to4, 
                                    PercentAge5to19, 
                                    PercentAge20to49,  
                                    PercentAge50plus)) %>% 
  select(-PercentAge5to9, -PercentAge10to19, -PercentAge50to74, 
         -PercentAge75plus, -EforsId, 
         -matches('^Percent.+Unknown$')) %>% 
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
  janitor::clean_names()
  
rm(list = ls()[!ls() %in% c("NORSMain", 
                            "GenEtiology", 
                            "__IFSACCommodityData")])

GenEtiology <- GenEtiology %>% 
  mutate(CDCID = as.character(CDCID)) %>% 
  right_join(select(NORSMain, CDCID = cdcid))

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
  mutate(CDCID = as.character(CDCID), 
         genus = GenusName, 
         serotype = if_else(GenusName == 'Escherichia', 
                            'STEC',
                            SerotypeName)) %>% 
  select(cdcid = CDCID, genus, serotype) %>% 
  distinct(cdcid, .keep_all = TRUE) -> agent


# Serotype for Salmonella outbreaks --------------------------------------------

# Food source ---------------------------------------------------------------
IFSAC <- `__IFSACCommodityData` %>% 
  mutate(CDCID = as.character(CDCID)) %>% 
  right_join(select(NORSMain, CDCID = cdcid))


analysis <- 
  IFSAC %>% 
  filter(IFSACLevel1 %in% c("Aquatic Animals", 
                            "Land Animals", 
                            "Other", 
                            "Plant")
         ) %>% 
  mutate(CDCID = as.character(CDCID), 
         food_source = case_when(
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
                      primary_mode, year) %>% 
  left_join(agent) %>% 
  right_join(x = analysis, y = .) %>% 
  mutate(attr_source = case_when(
    primary_mode == 'Animal Contact' ~ 'Animal Contact', 
    TRUE ~ food_source)) %>% 
  filter(!is.na(attr_source)) %>% 
  select(-food_source, -primary_mode) 

load(file = "DataProcessed/SeroGroupings.rda")

analysis <- 
  analysis %>% 
  left_join(uncommon_sero) %>% 
  mutate(serotype = case_when(
    serotype %in% rare$serotype ~ 'rare', 
    !is.na(serogroup) ~ serogroup, 
    TRUE ~ serotype)) %>% 
  select(-serogroup)


validate <- analysis %>% 
  mutate(serotype = str_remove(serotype, ' var.+$'), 
         attr_source = str_replace(str_to_lower(attr_source), 
                                            pattern = ' ',
                                            replacement = '_')
         ) %>% 
  select(percent_female, 
         percent_age_under1, 
         percent_age1to4, 
         percent_age5to19, 
         percent_age20to49, 
         percent_age50plus, 
         month, 
         geography, 
         serotype, 
         attr_source) 

load(file = 'DataProcessed/cleaned_model.RData')

validate %>% filter(!serotype %in% analysis$serotype) %>% count(serotype)

validate <- validate %>% 
  mutate(serotype = if_else(serotype %in% analysis$serotype, 
                            serotype, 'rare')) 

save(validate, file = 'DataProcessed/validation_data.RData')

validate <- validate %>% 
  mutate_if(is.character, as.factor) %>% 
  drop_na


load(file = 'DataProcessed/model_objects.RData')

predict(models$ranger, validate, type = 'prob') %>% 
  bind_cols(select(validate, attr_source)) %>% 
  pivot_longer(-attr_source, 
               names_to = 'predicted_cat', 
               values_to = 'predicted_value') %>% 
  mutate(y = if_else(predicted_cat == attr_source, 1, 0), 
         f_ti_minus_o_ti_sq = (predicted_value - y)^2) %>% 
  summarise(brier_score = 1/n()*sum(f_ti_minus_o_ti_sq))

predict(models$ranger, validate, type = 'prob') %>% 
  bind_cols(select(validate, attr_source)) %>% 
  pivot_longer(-attr_source, 
               names_to = 'predicted_cat', 
               values_to = 'predicted_value') %>% 
  mutate(y = if_else(predicted_cat == attr_source, 1, 0), 
       bin_midpoint = cut(predicted_value, 
                          breaks = seq(0, 1, 0.2), 
                          include.lowest = T, 
                          labels = seq(0.1, 0.9, 0.2)), 
       bin_midpoint = as.numeric(as.character(bin_midpoint))) %>% 
  group_by(predicted_cat, bin_midpoint) %>% 
  count(y) %>% 
  mutate(pct = n/sum(n)) %>% 
  ungroup() %>% 
  mutate(predicted_cat = str_to_title(str_replace(predicted_cat, 
                                                  pattern = '_',
                                                  replacement = ' '))
  ) %>% 
  filter(y == 1) %>% 
  ggplot(aes(x = bin_midpoint, 
             y = pct, 
             colour = predicted_cat)) +
  geom_line(aes(group = predicted_cat)) +
  geom_point(aes(size = n)) + 
  scale_x_continuous(breaks = seq(0.1, 0.9, 0.2)) + 
  scale_y_continuous(breaks = seq(0.1, 0.9, 0.2)) +
  labs(x = 'Predicted Bin Midpoint', 
       y = 'Observed Event Proportion', 
       title = 'Calibration Plots For All Models', 
       colour = 'Outbreak Source') + 
  geom_abline(slope = 1) + 
  theme_classic()
ggsave('Reports/Figures/validation_calibration_plot.png')

highest_two <- function(...){
  row <- list(...)[[1]]
  values <- unlist(row)
  ordered <- names(sort(values, decreasing = TRUE))
  return(paste(ordered[1:2], collapse = '.'))
}

predict(models$ranger, validate, type = 'prob') %>% 
  bind_cols(select(validate, attr_source)) %>%
  mutate(id = row_number()) %>% 
  group_by(id, attr_source) %>% 
  nest() %>% 
  transmute(two_highest = map(data, highest_two)) %>% 
  unnest(two_highest) %>% 
  separate(two_highest, into = c('highest', 'second_highest'), sep = '\\.') %>% 
  mutate(accurate = attr_source %in% c(highest, second_highest)) %>% 
  group_by(attr_source) %>% 
  count(accurate) %>% 
  mutate(pct = n/sum(n)) %>% 
  filter(accurate) %>% 
  ggplot(aes(y = pct, str_to_title(str_replace(attr_source, '_', ' ')))) + 
  geom_col(aes(fill = str_to_title(str_replace(attr_source, '_', ' ')))) + 
  guides(fill = 'none') + 
  labs(x = 'Actual Attributed Source', 
       y = 'Percent of Outbreaks Where Actual Source was in Top Two') + 
  theme_classic()
ggsave('Reports/Figures/predicted_top_two.png')


  
