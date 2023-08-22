library(tidyverse)
library(here)
library(tidymodels)
library(doParallel)
tidymodels_prefer()

hea <- read_csv(here('Data', 'HEA cleaned.csv'))

# Data partition ====

set.seed(123)
splits <- 
  initial_split(
    hea |> 
      mutate(
        Phase = factor(Phase),
        gamma_factor = ifelse(Phase %in% c('BCC', 'FCC', 'FCC+BCC'), 1.174, 1.1751)
      ), 
    strata = 'Phase',
    prop = 7/10
  )

train_phases <- training(splits)
test_phases <- testing(splits)
metric <- metric_set(accuracy, precision, recall, roc_auc, f_meas)

train_phases_clean <- 
  train_phases |> 
  arrange(desc(Elect.Diff)) |> 
  slice_tail(n = -2)

set.seed(234)
cv_strategy <- 
  vfold_cv(
    train_phases_clean, 
    v = 10, 
    strata = 'Phase'
  )

# Modeling ----------------------------------------------------------------

rf_spec <- 
  rand_forest(
    trees = 2000,
    mtry = tune(),
    min_n = tune()
  ) |> 
  set_mode('classification') |> 
  set_engine('ranger')

knn_spec <- 
  nearest_neighbor(
    neighbors = tune(), 
    dist_power = tune(),
    weight_func = tune()
  ) |> 
  set_mode('classification') |> 
  set_engine('kknn')

multinomial_spec <- 
  multinom_reg(
    penalty = tune(), 
    mixture = tune()
  ) |> 
  set_mode('classification') |> 
  set_engine('glmnet')

xgb_spec <- 
  boost_tree(
    trees = tune(),
    mtry = tune(),
    min_n = tune(), 
    learn_rate = tune()
  ) |> 
  set_mode('classification') |> 
  set_engine('xgboost')

# Tuning grids 
rf_grid <- 
  crossing(
    mtry = 2:4,
    min_n = seq(2, 20, by = 2)
  )

knn_grid <- 
  crossing(
    neighbors = seq(3, 30, by = 3),
    dist_power = c(1L, 2L),
    weight_func = c("triangular", "cos", "inv", "gaussian", "optimal")
  )

set.seed(465)
multinomial_grid <- 
  grid_latin_hypercube(
    penalty(range = c(-5, 0)),
    mixture(),
    size = 50
  )

xgb_grid <- 
  grid_latin_hypercube(
    trees(range = c(10L, 1000L)),
    mtry(range = c(1L, 4L)),
    min_n(range = c(2L, 20L)),
    learn_rate(range(-5, 0)),
    size = 50
  )

# Recipes & workflows
preprocessing <- 
  recipe(Phase ~ Elect.Diff + VEC + dHmix + gamma_factor, data = train_phases_clean) |> 
  step_mutate(gamma_indicator = gamma_factor < 1.175) |> 
  step_rm(gamma_factor)

normalize_preprocessing <- 
  preprocessing |>
  step_zv(all_numeric_predictors()) |>
  step_normalize(all_numeric_predictors())

numeric_preprocessing <- 
  normalize_preprocessing |> 
  step_mutate(gamma_indicator = as.numeric(gamma_indicator))

xgb_preprocessing <- 
  preprocessing |> 
  step_mutate(gamma_indicator = as.numeric(gamma_indicator))

rf_wf <- workflow(preprocessing, rf_spec)
knn_wf <- workflow(normalize_preprocessing, knn_spec)
multinomial_wf <- workflow(numeric_preprocessing, multinomial_spec)
xgb_wf <- workflow(xgb_preprocessing, xgb_spec)

# Tuning ------------------------------------------------------------------


all_cores <- parallel::detectCores(logical = FALSE)
cl <- makePSOCKcluster(all_cores)
registerDoParallel(cl)


rf_results <- 
  tune_grid(
    rf_wf, 
    grid = rf_grid,
    resamples = cv_strategy,
    metrics = metric,
    control = control_grid(save_pred = TRUE)
  )

knn_results <- 
  tune_grid(
    knn_wf,
    grid = knn_grid,
    resamples = cv_strategy,
    metrics = metric,
    control = control_grid(save_pred = TRUE)
  )

multinomial_results <- 
  tune_grid(
    multinomial_wf,
    grid = multinomial_grid,
    resamples = cv_strategy,
    metrics = metric,
    control = control_grid(save_pred = TRUE)
  )

xgb_results <- 
  tune_grid(
    xgb_wf,
    grid = xgb_grid,
    resamples = cv_strategy,
    metrics = metric,
    control = control_grid(save_pred = TRUE)
  )


# Explore results
rf_results |> 
  autoplot()

knn_results |> 
  show_best(metric = 'accuracy')

# Build overall metrics dataset
overall_results <- 
  bind_rows(
    get_metrics(rf_results, 'Random Forest'),
    get_metrics(multinomial_results, 'Multinomial Regression'),
    get_metrics(knn_results, 'K-Nearest Neighbors'),
    get_metrics(xgb_results, 'XGBoost')
  )

best_rf <- 
  rf_results |> 
  select_by_one_std_err(metric = 'accuracy', -min_n)

finalized_wf <- finalize_workflow(rf_wf, best_rf)
last_rf <- last_fit(finalized_wf, split = splits, metrics = metric)

last_rf |> collect_metrics() 
# A tibble: 2 × 4
# .metric   .estimator .estimate .config             
# accuracy  multiclass     0.808 Preprocessor1_Model1
# precision macro          0.742 Preprocessor1_Model1
# recall    macro          0.727 Preprocessor1_Model1
# f_meas    macro          0.730 Preprocessor1_Model1
# roc_auc   hand_till      0.972 Preprocessor1_Model1

final_rf <- extract_workflow(last_rf)

saveRDS(rf_results, file = here('Model results', 'v2', 'rf_results_v2.rds'))
saveRDS(final_rf, file = here('Model results', 'v2', 'rf_fit_v2.rds'))
saveRDS(knn_results, file = here('Model results', 'v2', 'knn_results_v2.rds'))
saveRDS(multinomial_results, file = here('Model results', 'v2', 'multinom_results_v2.rds'))
saveRDS(xgb_results, file = here('Model results', 'v2', 'xgb_results_v2.rds'))

write_csv(overall_results, file = here('Model results', 'v2', 'overall_results_v2.csv'))

