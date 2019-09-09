### Project Info ##########################
# Project: P1330White
# Author: Caroline Ledbetter
# Date: 08/22/2019
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

load(file = 'DataProcessed/cleaned_model.RData')

library(tidyverse)
library(tidymodels)
library(kknn)

analysis <- analysis %>% 
  as_tibble() %>% 
  mutate(
    attr_source_coll = fct_collapse(attr_source, 
                                    `Meat-Poultry` = c('Meat', 
                                                       'Poultry', 
                                                       'Meat-Poultry Other'), 
                                    `Produce` = c('Fruits', 
                                                  'Vegetables', 
                                                  'Produce Other')
    ), 
    attr_source_coll = as.character(attr_source_coll))
  
set.seed(1450)
analysis_split <- analysis %>% 
  select(-cdcid, -percent_male, -genus) %>%  
  filter(!attr_source == 'Other') %>% 
  initial_split(strata = attr_source)
analysis_split

analysis_split %>% glimpse()

recipe_1st_step <- 
  training(analysis_split) %>% 
  recipe(attr_source ~ .) %>% 
  update_role(attr_source_coll, 
           new_role = 'outcome') %>% 
  step_center(all_numeric()) %>% 
  step_scale(all_numeric()) %>% 
  step_knnimpute(all_predictors())

downsample <- recipe_1st_step %>% 
  remove_role(attr_source_coll, old_role = 'outcome') %>% 
  step_filter(!attr_source %in% c('Meat-Poultry Other', 'Produce Other')) %>% 
  step_mutate(attr_souce = fct_drop(attr_souce)) %>% 
  step_downsample(attr_source, ratio = 3) 

collapse <- recipe_1st_step %>% 
  remove_role(attr_source, old_role = 'outcome') %>% 
  step_filter(attr_source_coll != 'Dairy') 

all_recipe <- downsample %>% prep()
coll_recipe <- collapse %>% prep()

juice(all_recipe) %>% glimpse()
juice(coll_recipe) %>% glimpse()

kknn_model <- nearest_neighbor() %>% 
  set_mode('classification') %>% 
  set_engine('kknn') %>% 
  fit(attr_source ~ ., juice(recipe_anal))

kknn_model



  
generate_result <- function(pred_model){
  pred_model %>% 
    predict(anal_testing, type = 'prob') %>% 
    bind_cols(anal_testing)
}



kknn_model %>% generate_result() %>% 
  mutate_at(vars(starts_with('.pred_')), cut, breaks = seq(0, 1, 0.2), 
            include.lowest = T, labels = seq(0.1, 0.9, 0.2)) %>% 
  pivot_longer(starts_with('.pred'), names_to = "pred_source") %>% 
  mutate(pred_source = str_remove(pred_source, '\\.pred_')) %>% 
  group_by(pred_source, value) %>% 
  count(correct = pred_source == attr_source) %>% 
  mutate(pct = n/sum(n)) %>% 
  filter(correct) %>% 
  ggplot(aes(x = as.numeric(as.character(value)), 
             y = pct, colour = pred_source)) +
  geom_line(aes(group = pred_source)) +
  scale_x_continuous(breaks = seq(0.1, 0.9, 0.2)) + 
  scale_y_continuous(breaks = seq(0.1, 0.9, 0.2)) +
  labs(x = 'Bin Midpoint', 
       y = 'Observed Event Proportion', 
       title = 'Calibration Plots For All Models', 
       colour = 'Outbreak Source') + 
  geom_abline(slope = 1) + 
  theme_classic() + 
  theme(legend.position = c(0.12, 0.8)) 

