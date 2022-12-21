# Loading require packages and data ====

library(tidyverse)
library(here)
library(tidymodels)
library(themis)
library(stacks)
library(doParallel)

tidymodels_prefer()
theme_set(theme_bw())
theme_update(
  text = element_text(size = 14)
)

# hea <- read_csv(here("Data", "HEA melted.csv")) |> 
#   drop_na(Phase) |> 
#     mutate(
#     Phase = str_to_upper(Phase) |> str_remove_all('\\s'),
#     fase = case_when(
#       str_detect(Phase, 'BCC') & str_detect(Phase, 'FCC') & str_detect(Phase, 'IM') ~ "FCC+BCC+IM",
#       str_detect(Phase, 'BCC') & str_detect(Phase, 'FCC') ~ 'FCC+BCC',
#       str_detect(Phase, 'BCC') & str_detect(Phase, 'IM') ~ 'BCC+IM',
#       str_detect(Phase, 'FCC') & str_detect(Phase, 'IM') ~ 'FCC+IM',
#       str_detect(Phase, 'BCC') ~ 'BCC',
#       str_detect(Phase, 'FCC') ~ 'FCC',
#       TRUE ~ Phase
#     )) |> 
#   filter(!fase %in% c('SS', 'SS+IM', 'HCP')) |> 
#   select(-Phase) |> 
#   rename(Phase = fase)
# 
# write_csv(x = hea, file = here('Data', 'HEA cleaned.csv'))

hea <- read_csv(here('Data', 'HEA cleaned.csv'))

# Data partition ====

set.seed(123)
splits <- initial_split(
  hea |> mutate(Phase = factor(Phase)), 
  strata = 'Phase',
  prop = 7/10
)

train_phases <- training(splits)
test_phases <- testing(splits)
metric <- metric_set(accuracy, roc_auc, precision)

set.seed(234)
cv_strategy <- vfold_cv(
  train_phases, 
  v = 10, 
  strata = 'Phase'
)

# EDA ====

# There are 2 outliers in the `Elect.Diff` variable; let's explore them

# ggplot(data = train_phases,
#        mapping = aes(x = Phase, y = Elect.Diff, fill = Phase)) +
#   geom_boxplot(show.legend = FALSE)
# 
# train_phases |> 
#   arrange(desc(Elect.Diff))

# Usually, high `Elect.Diff` means negative `dHmix`, but this is not the case
# Both outliers correspond to the BCC phase, the most populated one
# How does the boxplot look without the outliers?

# train_phases |> 
#   arrange(desc(Elect.Diff)) |> 
#   slice(3:nrow(cur_data())) |> 
#   mutate(label = 'Without outliers') |> 
#   bind_rows(train_phases |> mutate(label = 'Original data')) |> 
#   ggplot(mapping = aes(x = Phase, y = Elect.Diff, fill = Phase)) +
#   geom_boxplot(show.legend = FALSE) + 
#   facet_wrap(~label, ncol = 2, scales = 'free_y')

# It's clear that outliers will have a significant effect on distance based models
# such as KNN and regularized regressions
# For this reason, we exclude the 2 outliers

train_phases_clean <- train_phases |> 
  arrange(desc(Elect.Diff)) |> 
  slice(3:nrow(cur_data()))

# Preprocessing ====

base_recipe <- recipe(Phase ~ Elect.Diff + VEC + dHmix, data = train_phases_clean)

normalize_recipe <- base_recipe |> 
  step_zv(all_numeric_predictors()) |> 
  step_normalize(all_numeric_predictors()) 

range_recipe <- base_recipe |> 
  step_zv(all_numeric_predictors()) |> 
  step_range(all_numeric_predictors(), min = 0, max = 1)

# Model specifications ====

knn_spec <- nearest_neighbor(
  neighbors = tune(), 
  dist_power = tune(),
  weight_func = tune()
) |> 
  set_mode('classification') |> 
  set_engine('kknn')

multinomial_spec <- multinom_reg(
  penalty = tune(), 
  mixture = tune()
) |> 
  set_mode('classification') |> 
  set_engine('glmnet')

mlp_spec <- mlp(
  hidden_units = tune(),
  dropout = 0.2,
  epochs = tune(),
  learn_rate = tune(),
  activation = 'relu'
) |> 
  set_mode('classification') |> 
  set_engine('keras')

rf_spec <- rand_forest(
  trees = 2000,
  mtry = tune(),
  min_n = tune()
) |> 
  set_mode('classification') |> 
  set_engine('ranger')

xgb_spec <- boost_tree(
  trees = tune(),
  mtry = tune(),
  min_n = tune(), 
  learn_rate = tune()
) |> 
  set_mode('classification') |> 
  set_engine('xgboost')

# Hyperparameter grids ====

set.seed(456)

knn_grid <- crossing(
  neighbors = seq(3, 30, by = 3),
  dist_power = c(1L, 2L),
  weight_func = c("triangular", "cos", "inv", "gaussian", "optimal")
)

multinomial_grid <- grid_latin_hypercube(
  penalty(range = c(-5, 0)),
  mixture(),
  size = 50
)

mlp_grid <- grid_latin_hypercube(
  hidden_units(),
  epochs(range = c(10L, 100L)),
  learn_rate(range = c(-5, 0)),
  size = 50
)

rf_grid <- crossing(
  mtry = 1:3,
  min_n = seq(5, 50, by = 5)
)

xgb_grid <- grid_latin_hypercube(
  trees(range = c(10L, 1000L)),
  mtry(range = c(1L, 3L)),
  min_n(range = c(1L, 50L)),
  learn_rate(range(-5, 0)),
  size = 50
)
# Workflows ====

knn_wf <- workflow(normalize_recipe, knn_spec)
multinomial_wf <- workflow(normalize_recipe, multinomial_spec)
mlp_wf <- workflow(range_recipe, mlp_spec)
rf_wf <- workflow(base_recipe, rf_spec)
xgb_wf <- workflow(base_recipe, xgb_spec)

# Tuning ====

# Null model metrics:
# accuracy = 0.252
# roc AUC Hand-Till = 0.5
# sensitivity = 0.125
# specificity = 0.875

all_cores <- parallel::detectCores(logical = FALSE)
cl <- makePSOCKcluster(all_cores)
registerDoParallel(cl)

knn_results <- tune_grid(
  knn_wf,
  grid = knn_grid,
  resamples = cv_strategy,
  metrics = metric,
  control = control_stack_grid()
)

multinomial_results <- tune_grid(
  multinomial_wf,
  grid = multinomial_grid,
  resamples = cv_strategy,
  metrics = metric,
  control = control_stack_grid()
)

# mlp_results <- tune_grid(
#   mlp_wf,
#   grid = mlp_grid,
#   resamples = cv_strategy,
#   metrics = metric,
#   control = control_stack_grid()
# )

rf_results <- tune_grid(
  rf_wf, 
  grid = rf_grid,
  resamples = cv_strategy,
  metrics = metric,
  control = control_stack_grid()
)

xgb_results <- tune_grid(
  xgb_wf,
  grid = xgb_grid,
  resamples = cv_strategy,
  metrics = metric,
  control = control_stack_grid()
)

saveRDS(knn_results, file = here('Model results', 'knn_results.rds'))
saveRDS(multinomial_results, file = here('Model results', 'multinomial_results.rds'))
# saveRDS(mlp_results, file = here('Model results', 'mlp_results.rds'))
saveRDS(rf_results, file = here('Model results', 'rf_results.rds'))
saveRDS(xgb_results, file = here('Model results', 'xgb_results.rds'))

stopImplicitCluster()