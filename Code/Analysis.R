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
library(ranger)
library(xgboost)
library(C50)
library(caret)

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
         serotype, 
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

# modelling ----    
models <- list()
blueprint <- as.formula(attr_source ~ .)
cv_train <- trainControl(method = 'cv', number = 5, 
                         classProbs = TRUE, 
                         seeds = lapply(1:nrow(anal_training), 
                                        function(x) 1:20))

# null model ----
models$null <- 
  null_model() %>% 
  set_mode('classification') %>% 
  set_engine('parsnip') %>% 
  fit(attr_source ~ ., juice(recipe))

# mars model ----
# create tuning grid
hyper_grid <- expand.grid(
  degree = 1:3, 
  nprune = seq(2, 100, length.out = 10) %>% 
    floor()
)

set.seed(1119)
cv_mars <- train(
  blueprint, 
  data = anal_training, 
  method = "earth",
  trControl = cv_train,
  tuneGrid = hyper_grid
)

cv_mars$bestTune
ggplot(cv_mars)

# narrow search
hyper_grid <- expand.grid(
  degree = 1:3, 
  nprune = seq(18, 27, length.out = 10) %>% 
    floor()
)

set.seed(1119)
cv_mars <- train(
  blueprint, 
  data = anal_training, 
  method = "earth",
  trControl = cv_train,
  tuneGrid = hyper_grid
)

cv_mars$bestTune
ggplot(cv_mars)

# select best model
mars <- earth(blueprint, 
                     data = anal_training, 
                     degree = 2, 
                     nprune = 18)

# weighted k nearest neighboors ----
hyper_grid <- expand.grid(
  k = seq(1, nrow(anal_training)/3, length.out = 20) %>% 
    floor
)

# Fit knn model and perform grid search
set.seed(1119)
knn_grid <- train(
  blueprint, 
  data = anal_training, 
  method = "knn", 
  trControl = cv_train, 
  tuneGrid = hyper_grid
)

ggplot(knn_grid)
knn_grid$bestTune

hyper_grid <- expand.grid(
  k = seq(30, 100, length.out = 20) %>% 
    floor
)

set.seed(1119)
knn_grid <- train(
  blueprint, 
  data = anal_training, 
  method = "knn", 
  trControl = cv_train, 
  tuneGrid = hyper_grid
)

ggplot(knn_grid)
knn_grid$bestTune

models$kknn <-
  nearest_neighbor(neighbors = 44) %>%
  set_mode('classification') %>%
  set_engine('kknn') %>%
  fit(attr_source ~ ., juice(recipe))

# C5.0 ----
set.seed(1119)
models$c50 <- train(
  blueprint, 
  data = anal_training, 
  method = "C5.0", 
  trControl = cv_train, 
  control = C50::C5.0Control(seed = 1)
)

ggplot(models$c50)
models$c50$bestTune

# xgboost ----
set.seed(1119)
models$xgboost <- 
  boost_tree() %>% 
  set_mode('classification') %>% 
  set_engine('xgboost') %>% 
  fit(attr_source ~ ., juice(recipe))

# ranger ----
set.seed(1119)
models$ranger <- train(
  blueprint, 
  data = anal_training, 
  method = "ranger", 
  trControl = cv_train, 
  num.trees = 20,
  seed = 345, 
  num.threads = 1,
  importance = "permutation"
)

ggplot(models$ranger)
models$ranger$bestTune


# Results ----
mars_results <-
  predict(mars, select(anal_testing, -attr_source), 
        type = 'response') %>% 
  as_tibble() %>% 
  mutate(model = 'mars') %>%
  bind_cols(select(anal_testing, attr_source))



generate_result <- function(pred_model){
  predict(pred_model, 
          select(anal_testing, -attr_source), 
          type = 'prob') %>% 
    rename_at(vars(starts_with('.pred')), str_remove, '.pred_') %>% 
    bind_cols(select(anal_testing, attr_source))
}

results <- map_dfr(models, generate_result, .id = 'model') %>% 
  bind_rows(mars_results)

generate_result_table <- function(result) {
  result %>% 
    pivot_longer(starts_with('.pred_'), 
                 names_to = 'predicted_cat', 
                 values_to = 'predicted_value') %>% 
    mutate(predicted_cat = str_remove(predicted_cat, '\\.pred_'), 
           y = if_else(predicted_cat == attr_source, 1, 0)) 
}

results_table <- results %>% 
  pivot_longer(-c(attr_source, model), 
               names_to = 'predicted_cat', 
               values_to = 'predicted_value') %>% 
  mutate(y = if_else(predicted_cat == attr_source, 1, 0)) 

results_table %>% 
  mutate(bin_midpoint = cut(predicted_value, 
                            breaks = seq(0, 1, 0.2), 
                            include.lowest = T, 
                            labels = seq(0.1, 0.9, 0.2)), 
         bin_midpoint = as.numeric(as.character(bin_midpoint))) %>% 
  group_by(model, predicted_cat, bin_midpoint) %>% 
  count(y) %>% 
  mutate(pct = n/sum(n)) %>% 
  ungroup() %>% 
  mutate(model = factor(model, 
                         levels = c('null', 'mars', 'kknn', 
                                    'c50', "xgboost", 'ranger')), 
         predicted_cat = str_to_title(str_replace(predicted_cat, 
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
  labs(x = 'Bin Midpoint', 
       y = 'Observed Event Proportion', 
       title = 'Calibration Plots For All Models', 
       colour = 'Outbreak Source') + 
  geom_abline(slope = 1) + 
  theme_classic() + 
  facet_wrap(vars(model))

brier_scores <- results_table %>% 
  mutate(f_ti_minus_o_ti_sq = (predicted_value - y)^2) %>% 
  group_by(model) %>% 
  summarise(brier_score = 1/n()*sum(f_ti_minus_o_ti_sq))

models$mars <- mars
final_model <- models$ranger
skeleton <- anal_training[0, ]
save(models, file = 'DataProcessed/model_objects.RData')
save(final_model, file = 'SourceAttribution/ranger_model_obj.rda')
save(skeleton, file = 'SourceAttribution/file_skeleton.rda')






