library(tidyverse)
library(here)
library(tidymodels)
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
hea |> skimr::skim_without_charts()

# ---------------------------------------------------------------------------
# tidymodels
# ---------------------------------------------------------------------------

set.seed(123)
splits <- initial_split(hea |> mutate(Phase = factor(Phase)), strata = 'Phase')
train_phases <- training(splits)
test_phases <- testing(splits)

set.seed(234)
cv_strategy <- vfold_cv(train_phases, 10, strata = 'Phase')

# ---------------------------------------------------------------------------
# Preprocessing
# ---------------------------------------------------------------------------

simple_recipe <- recipe(Phase ~ Elect.Diff + VEC + dHmix, data = train_phases) |> 
  step_log(Elect.Diff, offset = 0.1) |>
  step_normalize(all_predictors())

# ---------------------------------------------------------------------------
# Model specifications
# ---------------------------------------------------------------------------

knn_spec <- nearest_neighbor(
  neighbors = tune(), 
  dist_power = tune(),
  weight_func = tune()
) |> 
  set_mode('classification') |> 
  set_engine('kknn')

# ---------------------------------------------------------------------------
# Hyperparameter grids
# ---------------------------------------------------------------------------

knn_grid <- crossing(
  neighbors = seq(3, 30, by = 3),
  dist_power = c(1L, 2L),
  weight_func = weight_func()$values
)
# ---------------------------------------------------------------------------
# Workflows and metrics
# ---------------------------------------------------------------------------

knn_wf <- workflow(simple_recipe, knn_spec)
classification_kpi <- metric_set(accuracy, recall, precision, f_meas)

# ---------------------------------------------------------------------------
# Tuning
# ---------------------------------------------------------------------------

library(doParallel)

# Null model metrics:
# accuracy = 0.252
# roc AUC Hand-Till = 0.5
# sensitivity = 0.125
# specificity = 0.875

all_cores <- parallel::detectCores(logical = FALSE)
cl <- makePSOCKcluster(all_cores)
registerDoParallel(cl)

knn_results <- 
  tune_grid(
    knn_wf,
    grid = knn_grid,
    resamples = cv_strategy,
    metrics = classification_kpi,
    control = control_grid(save_pred = TRUE)
  )

stopImplicitCluster()

# show_notes(.Last.tune.result):
# en 5/10 folds no hubo predicciones para la clase `FCC+BCC+IM`
# por ello, precision es 0/0. El precision macro-averaged ignora estos casos

# ---------------------------------------------------------------------------
# Model assessment
# ---------------------------------------------------------------------------

autoplot(knn_results)

knn_best_config <- 
  knn_results |>
  select_best(metric = 'f_meas')

best_config_preds <- 
  knn_results |> 
  collect_predictions() |>
  filter(.config == knn_best_config$.config) |> 
  select(.row, .pred_class, Phase) |> 
  mutate(correct = Phase == .pred_class) |> 
  left_join(
    train_phases |> 
      mutate(.row = row_number()) |> 
      select(-Phase),
    by = '.row'
  ) 


best_config_preds |> 
  ggplot(aes(dHmix, Elect.Diff)) +
  stat_summary_hex(aes(z = as.integer(correct)), 
                   fun = 'mean') +
  scale_y_log10() +
  scale_fill_gradient(low = 'gray20', high = 'cyan3')

best_config_preds |> 
  ggplot(aes(dHmix, VEC)) +
  stat_summary_hex(aes(z = as.integer(correct)), 
                   fun = 'mean') +
  scale_y_log10() +
  scale_fill_gradient(low = 'gray20', high = 'cyan3')

best_config_preds |> 
  ggplot(aes(Elect.Diff, VEC)) +
  stat_summary_hex(aes(z = as.integer(correct)), 
                   fun = 'mean') +
  scale_x_log10() +
  scale_y_log10() +
  scale_fill_gradient(low = 'gray20', high = 'cyan3')

# ---------------------------------------------------------------------------
# Retrain models: KNN
# ---------------------------------------------------------------------------
library(themis)

upsampled_preprocessing <- 
  recipe(Phase ~ Elect.Diff + VEC + dHmix, data = train_phases) |> 
  step_log(Elect.Diff, offset = 0.1) |>
  step_normalize(all_predictors()) |> 
  step_upsample(Phase) 

knn_wf <- 
  workflow() |> 
  add_model(knn_spec) |> 
  add_recipe(upsampled_preprocessing)

# ---------------------------------------------------------------------------
# Parallel processing
# ---------------------------------------------------------------------------

registerDoParallel(cl)

knn_results <- 
  tune_grid(
    knn_wf,
    grid = knn_grid,
    resamples = cv_strategy,
    metrics = classification_kpi,
    control = control_grid(save_pred = TRUE)
  )

stopImplicitCluster()
autoplot(knn_results)
knn_results |> show_best('f_meas')












