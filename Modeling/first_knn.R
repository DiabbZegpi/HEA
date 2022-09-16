library(tidyverse)
library(here)

hea <- read_csv(here("Data", "HEA melted.csv"))

count_pct <- function(.data, variable, n_print = Inf) {
  .data |> 
    count({{ variable }}, sort = TRUE) |> 
    mutate(pct = n / sum(n),
           acum_pct = cumsum(pct)) |> 
    print(n = n_print)
}

clean_phases <- hea |> 
  drop_na(Phase) |> 
  mutate(
    Phase = str_to_upper(Phase) |> str_remove_all('\\s'),
    fase = case_when(
      str_detect(Phase, 'BCC') & str_detect(Phase, 'FCC') & str_detect(Phase, 'IM') ~ "FCC+BCC+IM",
      str_detect(Phase, 'BCC') & str_detect(Phase, 'FCC') ~ 'FCC+BCC',
      str_detect(Phase, 'BCC') & str_detect(Phase, 'IM') ~ 'BCC+IM',
      str_detect(Phase, 'FCC') & str_detect(Phase, 'IM') ~ 'FCC+IM',
      str_detect(Phase, 'BCC') ~ 'BCC',
      str_detect(Phase, 'FCC') ~ 'FCC',
      TRUE ~ Phase
    )) |> 
  filter(!fase %in% c('SS', 'SS+IM', 'HCP')) |> 
  select(-Phase) |> 
  rename(Phase = fase)


clean_phases |> skimr::skim_without_charts()

# ---------------------------------------------------------------------------
# tidymodels
# ---------------------------------------------------------------------------
library(tidymodels)
tidymodels_prefer()

set.seed(123)
splits <- initial_split(clean_phases |> mutate(Phase = factor(Phase)), strata = 'Phase')
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

knn_spec <- nearest_neighbor(mode = 'classification', neighbors = tune(), dist_power = tune())
tree_spec <- decision_tree(mode = 'classification', tree_depth = tune(), min_n = tune())

# ---------------------------------------------------------------------------
# Hyperparameter grids
# ---------------------------------------------------------------------------

tree_grid <- grid_regular(
  tree_depth(),
  min_n(),
  levels = 5
)

knn_grid <- crossing(
  neighbors = seq(3, 30, by = 3),
  dist_power = c(1L, 2L)
)
# ---------------------------------------------------------------------------
# Workflows
# ---------------------------------------------------------------------------

knn_wf <- workflow(simple_recipe, knn_spec)
tree_wf <- workflow(simple_recipe, tree_spec)

# ---------------------------------------------------------------------------
# Tuning
# ---------------------------------------------------------------------------

library(doParallel)

all_cores <- parallel::detectCores(logical = FALSE)
cl <- makePSOCKcluster(all_cores - 1)
registerDoParallel(cl)
classification_kpi <- metric_set(roc_auc, accuracy, sensitivity, precision)

null_model_results <- null_model() |> 
  set_mode('classification') |> 
  set_engine('parsnip') |> 
  workflow(spec = _) |> 
  add_formula(Phase ~ .) |> 
  fit_resamples(
    resamples = cv_strategy,
    metrics = classification_kpi
  )

null_model_results |> collect_metrics()

# Null model metrics:
# accuracy = 0.252
# roc AUC Hand-Till = 0.5
# sensitivity = 0.125
# specificity = 0.875

knn_results <- 
  tune_grid(
    knn_wf,
    grid = knn_grid,
    resamples = cv_strategy,
    metrics = classification_kpi,
    control = control_grid(save_pred = TRUE)
  )

tree_results <- 
  tune_grid(
    tree_wf,
    grid = tree_grid,
    resamples = cv_strategy,
    metrics = classification_kpi,
    control = control_grid(save_pred = TRUE)
  )

# ---------------------------------------------------------------------------
# Model assessment
# ---------------------------------------------------------------------------

theme_set(theme_light())
theme_update(
  text = element_text(size = 14)
)

knn_results |> 
  collect_metrics() |> 
  ggplot(aes(factor(dist_power), mean, color = .metric, size = neighbors)) +
  geom_point(alpha = 0.5) +
  facet_wrap(~ .metric) +
  scale_size_area(max_size = 10)

tree_results |> 
  collect_metrics() |> 
  ggplot(aes(factor(tree_depth), mean, size = min_n, color = .metric)) +
  geom_point(alpha = 0.5) +
  facet_wrap(~ .metric) +
  scale_size_area(max_size = 10)

# The tree-based model is outperformed by the nearest neighbors model

knn_best_config <- 
  knn_results |>
  select_best(metric = 'sensitivity') 

best_config_preds <- 
  knn_results |> 
  collect_predictions() |>
  filter(.config == knn_best_config$.config) |> 
  select(.row, .pred_class, Phase) |> 
  left_join(
    train_phases |> 
      mutate(.row = row_number()) |> 
      select(-Phase),
    by = '.row'
  ) 


best_config_preds |> 
  conf_mat(truth = Phase, .pred_class) |> 
  autoplot()

best_config_preds |> 
  mutate(miss = if_else(Phase == .pred_class, FALSE, TRUE)) |> 
  ggplot(aes(log(dHmix ^ 2 + 0.1), log(Elect.Diff), color = miss)) +
  geom_point() +
  scale_color_viridis_d(end = 0.8, begin = 0.2, option = 1)

best_config_preds |> 
  mutate(miss = if_else(Phase == .pred_class, FALSE, TRUE)) |> 
  ggplot(aes(dHmix, VEC, color = miss)) +
  geom_point() +
  scale_color_viridis_d(end = 0.8, begin = 0.2, option = 1) 


# ---------------------------------------------------------------------------
# Retrain models: KNN
# ---------------------------------------------------------------------------

knn_spec_2 <- 
  nearest_neighbor(
    mode = 'classification',
    engine = 'kknn',
    neighbors = tune(),
    weight_func = tune(),
    dist_power = tune()
  )

knn_wf <- 
  workflow() |> 
  add_model(knn_spec_2) |> 
  add_recipe(simple_recipe)

knn_grid_2 <- 
  grid_regular(
    weight_func(),
    neighbors(),
    dist_power(),
    levels = 5
  )


library(doParallel)

all_cores <- parallel::detectCores(logical = FALSE)
cl <- makePSOCKcluster(all_cores - 1)
registerDoParallel(cl)
classification_kpi <- metric_set(roc_auc, accuracy, sensitivity, precision)

knn_results <- 
  tune_grid(
    knn_wf,
    grid = knn_grid_2,
    resamples = cv_strategy,
    metrics = classification_kpi,
    control = control_grid(save_pred = TRUE)
  )

knn_results |> 
  collect_metrics() |> 
  ggplot(aes(factor(dist_power), mean, color = .metric, size = neighbors)) +
  geom_point(alpha = 0.5) +
  facet_wrap(~ .metric) +
  scale_size_area(max_size = 10)


knn_results |> show_best('precision')












