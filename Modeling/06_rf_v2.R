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

rf_grid <- 
  crossing(
    mtry = 2:4,
    min_n = seq(2, 20, by = 2)
  )

preprocessing <- 
  recipe(Phase ~ Elect.Diff + VEC + dHmix + gamma_factor, data = train_phases_clean) |> 
  step_mutate(gamma_indicator = gamma_factor < 1.175) |> 
  step_rm(gamma_factor)

rf_wf <- workflow(preprocessing, rf_spec)


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


rf_results |> 
  autoplot()

best_rf <- 
  rf_results |> 
  select_by_one_std_err(metric = 'accuracy', -min_n)

finalized_wf <- finalize_workflow(rf_wf, best_rf)
last_rf <- last_fit(finalized_wf, split = splits)

last_rf |> collect_metrics() 
# A tibble: 2 × 4
# .metric  .estimator .estimate .config             
# <chr>    <chr>          <dbl> <chr>               
#   1 accuracy multiclass     0.808 Preprocessor1_Model1
#   2 roc_auc  hand_till      0.971 Preprocessor1_Model1

final_rf <- extract_workflow(last_rf)

saveRDS(rf_results, file = here('Model results', 'RF v2', 'rf_results_v2.rds'))
saveRDS(final_rf, file = here('Model results', 'RF v2', 'rf_fit_v2.rds'))















