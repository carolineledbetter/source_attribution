### Project Info ##########################
# Project: P1330White
# Author: Caroline Ledbetter
# Date: 08/22/2019
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

load(file = 'DataProcessed/cleaned_model.RData')

library(tidyverse)
library(tidymodels)
library(kknn)
library(earth)

run_predictions <- function(data){
  set.seed(1450)
  analysis_split <- data %>% 
    select(-cdcid, -percent_male, -genus) %>%  
    initial_split(strata = attr_source)
  analysis_split
  
  analysis_split %>% glimpse()
  
  suppressWarnings({
    recipe <- 
      training(analysis_split) %>% 
      recipe(attr_source ~ .) %>% 
      step_center(all_numeric()) %>% 
      step_scale(all_numeric()) %>% 
      step_string2factor(all_nominal()) %>% 
      step_knnimpute(all_predictors()) %>% 
      step_downsample(attr_source, ratio = 2.5) %>%
      prep()
    
    juice(recipe) %>% glimpse()
    
    anal_testing <- recipe %>% 
      bake(testing(analysis_split))
    
    models <- list()
    
    models$null <- 
      null_model() %>% 
      set_mode('classification') %>% 
      set_engine('parsnip') %>% 
      fit(attr_source ~ ., juice(recipe))
    
    models$kknn <- 
      nearest_neighbor() %>% 
      set_mode('classification') %>% 
      set_engine('kknn') %>% 
      fit(attr_source ~ ., juice(recipe))
    
    # models$mars <- mars() %>% 
    #   set_mode('classification') %>% 
    #   set_engine('earth') %>% 
    #   fit(attr_source ~ ., juice(recipe))
    
    models$c50 <- 
      boost_tree() %>% 
      set_mode('classification') %>% 
      set_engine('C5.0') %>% 
      fit(attr_source ~ ., juice(recipe))
    
    models$xgboost <- 
      boost_tree() %>% 
      set_mode('classification') %>% 
      set_engine('xgboost') %>% 
      fit(attr_source ~ ., juice(recipe))
    
    mars <- earth(attr_source ~ ., juice(recipe))
    
  })
  
  
  mars_results <- 
    predict(mars, anal_testing, type = 'response') %>%
    as_tibble() %>% 
    rename(`Animal Contact` = AnimalContact) %>% 
    rename_all(~str_c('.pred_', .)) %>% 
    mutate(model = 'mars') %>% 
    bind_cols(anal_testing)
  
  generate_result <- function(pred_model){
    predict(pred_model, 
            anal_testing, 
            type = 'prob') %>% 
      bind_cols(anal_testing)
  }
  
  generate_result_table <- function(result) {
    result %>% 
      pivot_longer(starts_with('.pred_'), 
                   names_to = 'predicted_cat', 
                   values_to = 'predicted_value') %>% 
      mutate(predicted_cat = str_remove(predicted_cat, '\\.pred_'), 
             y = if_else(predicted_cat == attr_source, 1, 0)) 
  }
  
  generate_brier_score <- function(result){
    generate_result_table(result) %>% 
      mutate(f_ti_minus_o_ti_sq = (predicted_value - y)^2) %>% 
      group_by(model) %>% 
      summarise(brier_score = 1/n()*sum(f_ti_minus_o_ti_sq))
  }
  
  map_dfr(models, generate_result, .id = 'model') %>% 
    bind_rows(mars_results) %>% 
    generate_result_table() %>% 
    mutate(bin_midpoint = cut(predicted_value, 
                              breaks = seq(0, 1, 0.2), 
                              include.lowest = T, 
                              labels = seq(0.1, 0.9, 0.2)), 
           bin_midpoint = as.numeric(as.character(bin_midpoint))) %>% 
    group_by(model, predicted_cat, bin_midpoint) %>% 
    count(y) %>% 
    mutate(pct = n/sum(n)) %>% 
    filter(y == 1 & model != 'null') %>% 
    ggplot(aes(x = bin_midpoint, 
               y = pct, 
               colour = predicted_cat)) +
    geom_line(aes(group = predicted_cat)) +
    geom_point(aes(size = n)) + 
    scale_x_continuous(breaks = seq(0.1, 0.9, 0.2)) + 
    scale_y_continuous(breaks = seq(0.1, 0.9, 0.2)) +
    labs(x = 'Bin Midpoint', 
         y = 'Observed Event Proportion', 
         title = 'Calibration Plots For All Models', 
         colour = 'Outbreak Source') + 
    geom_abline(slope = 1) + 
    theme_classic() + 
    facet_wrap(vars(model)) -> plots
  
  map_dfr(models, generate_result, 
          .id = 'model') %>% 
    bind_rows(mars_results) %>% 
    generate_brier_score() -> brier_scores
  
  map_dfr(models, generate_result, 
          .id = 'model') %>% 
    bind_rows(mars_results) %>% 
    generate_result_table() -> predictions
  
  return(list(plots = plots, brier_scores = brier_scores, 
              predictions = predictions))
  
}

analysis %>% 
  select(-year) %>% 
  filter(!str_detect(attr_source, 'Other')) %>% 
  run_predictions() -> no_other
no_other$plots

analysis %>% 
  select(-year) %>% 
  mutate(   
    attr_source = fct_collapse(attr_source, 
                               `Meat-Poultry` = c('Meat', 
                                                  'Poultry', 
                                                  'Meat-Poultry Other'), 
                               `Produce` = c('Fruits', 
                                             'Vegetables', 
                                             'Produce Other')
                               )
  ) %>% 
  filter(!attr_source %in% c('Dairy', 'Other')) %>%
    droplevels() %>% 
  run_predictions() -> collapsed

collapsed$plots


save(collapsed, no_other, file = 'DataProcessed/Results.RData')

