### Project Info ##########################
# Project: P1330White
# Author: Caroline Ledbetter
# Date: 03/26/2020
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

load("~/Repositories/P1330White/DataProcessed/cleaned_model.RData")
load("~/Repositories/P1330White/DataProcessed/model_objects.RData")
load("~/Repositories/P1330White/DataProcessed/recipe.rda")
library(recipes)
library(rsample)
library(tidyverse)
library(lime)
library(parsnip)


# model_set ----
model_set <- analysis %>% 
  select(percent_female, 
         percent_age_under1, 
         percent_age1to4, 
         percent_age5to19, 
         percent_age20to49, 
         percent_age50plus, 
         month, 
         geography, 
         serogroup, 
         attr_source) %>% 
  filter(!str_detect(attr_source, 'Other')) %>% 
  mutate(attr_source = str_replace(str_to_lower(attr_source), 
                                   pattern = ' ',
                                   replacement = '_'))

# training split
set.seed(1450)
analysis_split <- model_set %>% 
  initial_split(strata = attr_source)
analysis_split

analysis_split %>% glimpse()

# impute missing data
recipe <- 
  training(analysis_split) %>% 
  recipe(attr_source ~ .) %>% 
  step_string2factor(all_nominal()) %>% 
  step_knnimpute(all_predictors()) %>% 
  prep()

anal_training <- juice(recipe) 
anal_training %>% glimpse()

anal_testing <- recipe %>% 
  bake(testing(analysis_split))

# Create an explainer object
explainer <- lime(select(anal_training, -attr_source), models$ranger, quantile_bins = FALSE)
save(explainer, file = 'SourceAttribution/lime_explainer.rda')
set.seed(1036)
which_outbreak <- sample(1:nrow(anal_testing), 1)
explanation <- explain(anal_testing[which_outbreak ,-10], explainer, n_labels = 7, n_features = 9)
explanation %>% 
  mutate(feature_desc = str_to_title(str_replace_all(feature_desc, "_", " ")), 
         feature_desc = str_replace_all(feature_desc, 
                                        c(`Serogroup = Stec` = 'STEC', 
                                          Under1 = "Under 1",
                                          Age1to4 = "Age 1 - 4", 
                                          Age5to19 = "Age 5 - 19", 
                                          Age20to49 = "Age 20 - 49", 
                                          Age50plus = "Age 50 and older", 
                                          `Month <= 3.75` = 'Jan - Mar', 
                                          `3.75 < month <= 6.50` = 'Apr - Jun', 
                                          `6.50 < month <= 9.25` = 'Jul - Sep', 
                                          `9.25 < month` = 'Oct - Dec'))
  
) %>% 
plot_features()

anal_testing[which_outbreak,]
