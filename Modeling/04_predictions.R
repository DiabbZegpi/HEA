library(here)
library(tidyverse)
library(tidymodels)
library(stacks)
tidymodels_prefer()

# knn_results <- readRDS(here('Model results', 'knn_results.rds'))
# rf_results <- readRDS(here('Model results', 'rf_results.rds'))
# xgb_results <- readRDS(here('Model results', 'xgb_results.rds'))
# multinomial_results <- readRDS(here('Model results', 'multinomial_results.rds'))
# ensemble_1 <- readRDS(here('Model results', 'stacks_fit.rds'))
# ensemble_2 <- readRDS(here('Model results', 'stacks_fit_2.rds'))
# ensemble_3 <- readRDS(here('Model results', 'stacks_fit_3.rds'))

knn_fit <- readRDS(here('Model results', 'knn_fit.rds'))
rf_fit <- readRDS(here('Model results', 'rf_fit.rds'))
xgb_fit <- readRDS(here('Model results', 'xgb_fit.rds'))
multinomial_fit <- readRDS(here('Model results', 'multinomial_fit.rds'))
ensemble_1 <- readRDS(here('Model results', 'stacks_fit.rds'))
ensemble_2 <- readRDS(here('Model results', 'stacks_fit_2.rds'))
ensemble_3 <- readRDS(here('Model results', 'stacks_fit_3.rds'))

# hea <- read_csv(here('Data', 'HEA cleaned.csv'))
# set.seed(123)
# splits <- initial_split(
#   hea |> mutate(Phase = factor(Phase)), 
#   strata = 'Phase',
#   prop = 7/10
# )

# Model specifications ====

# knn_spec <- nearest_neighbor(
#   neighbors = tune(), 
#   dist_power = tune(),
#   weight_func = tune()
# ) |> 
#   set_mode('classification') |> 
#   set_engine('kknn')
# 
# multinomial_spec <- multinom_reg(
#   penalty = tune(), 
#   mixture = tune()
# ) |> 
#   set_mode('classification') |> 
#   set_engine('glmnet')
# 
# rf_spec <- rand_forest(
#   trees = 2000,
#   mtry = tune(),
#   min_n = tune()
# ) |> 
#   set_mode('classification') |> 
#   set_engine('ranger')
# 
# xgb_spec <- boost_tree(
#   trees = tune(),
#   mtry = tune(),
#   min_n = tune(), 
#   learn_rate = tune()
# ) |> 
#   set_mode('classification') |> 
#   set_engine('xgboost')
# 
# base_recipe <- recipe(Phase ~ Elect.Diff + VEC + dHmix, data = training(splits)) 
# 
# normalize_recipe <- base_recipe |> 
#   step_zv(all_numeric_predictors()) |> 
#   step_normalize(all_numeric_predictors()) 
# 
# 
# # Workflows --------------------------------------------------------------
# 
# knn_wf <- workflow(normalize_recipe, knn_spec)
# multinomial_wf <- workflow(normalize_recipe, multinomial_spec)
# rf_wf <- workflow(base_recipe, rf_spec)
# xgb_wf <- workflow(base_recipe, xgb_spec)
# 
# knn_best <- finalize_workflow(knn_wf, select_best(knn_results, 'accuracy'))
# multinomial_best <- finalize_workflow(multinomial_wf, select_best(multinomial_results, 'accuracy'))
# rf_best <- finalize_workflow(rf_wf, select_best(rf_results, 'accuracy'))
# xgb_best <- finalize_workflow(xgb_wf, select_best(xgb_results, 'accuracy'))
# 
# knn_fit <- last_fit(knn_best, splits)
# multinomial_fit <- last_fit(multinomial_best, splits)
# rf_fit <- last_fit(rf_best, splits)
# xgb_fit <- last_fit(xgb_best, splits)


# Preds -------------------------------------------------------------------

new_hea <- tribble(
  ~id, ~dHmix, ~Elect.Diff, ~VEC,
  'HEA 1', -4.27, 0.1069, 7.9285,
  'HEA 2', 0.0014, 0.124, 7.593
)


prob_predictions <- bind_rows(
  predict(multinomial_fit, new_data = new_hea, type = 'prob') |> mutate(model = 'Multinomial regression'),
  predict(knn_fit, new_data = new_hea, type = 'prob') |> mutate(model = 'KNN'),
  predict(rf_fit, new_data = new_hea, type = 'prob') |> mutate(model = 'Random forest'),
  predict(xgb_fit, new_data = new_hea, type = 'prob') |> mutate(model = 'XGBoost'),
  predict(ensemble_1, new_data = new_hea, type = 'prob') |> mutate(model = 'Ensemble Multinomial + KNN + RF, + XGB'),
  predict(ensemble_2, new_data = new_hea, type = 'prob') |> mutate(model = 'Ensemble KNN + RF, + XGB'),
  predict(ensemble_3, new_data = new_hea, type = 'prob') |> mutate(model = 'Ensemble KNN + RF')
) |> 
  mutate(id = rep(new_hea$id, times = length(unique(cur_data()$model))))


class_preds <- prob_predictions |> 
  pivot_longer(cols = .pred_AM:.pred_IM, names_to = '.pred_class') |> 
  group_by(model, id) |> 
  slice_max(value, with_ties = FALSE) |> 
  ungroup() |> 
  select(-value)
  

preds_df <- prob_predictions |> 
  left_join(class_preds, by = c('model', 'id')) 


# Plots -------------------------------------------------------------------

theme_set(theme_minimal())
theme_update(text = element_text(size = 14),
             axis.title = element_text(face = 'bold'),
             strip.background = element_rect(fill = '#ddddee'),
             strip.text = element_text(size = 12),
             legend.key = element_rect(fill = NA),
             legend.text = element_text(size = 12),
             axis.ticks = element_blank(),
             aspect.ratio = 1,
             panel.spacing = unit(2, 'lines'))

(
  prediction_plot_1 <- preds_df |> 
    pivot_longer(.pred_AM:.pred_IM, names_to = 'class', values_to = 'probability') |> 
    filter(!model %in% c('Ensemble KNN + RF', 'Ensemble KNN + RF, + XGB')) |> 
    mutate(across(.pred_class:class, \(name) str_remove(name, '\\.pred_'))) |> 
    ggplot(aes(x = reorder(class, probability), y = probability, fill = probability)) +
    geom_col(show.legend = FALSE, alpha = 0.8) +
    scale_y_continuous(labels = label_percent(), breaks = seq(0.25, 1, by = 0.25)) +
    scale_fill_viridis_c(option = 'B', begin = 0.1, end = 0.9) +
    coord_flip() +
    facet_grid(id ~ model, labeller = label_wrap_gen(width = 10)) +
    labs(y = 'Predicted probability', x = NULL) +
    theme(strip.background = element_blank(),
          strip.text.x = element_text(face = 'bold'),
          panel.grid.minor = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(linetype = 3, color = 'gray'))
)

(
  prediction_plot_2 <- preds_df |> 
    pivot_longer(.pred_AM:.pred_IM, names_to = 'class', values_to = 'probability') |> 
    filter(str_detect(model, '(?i)ensemble')) |> 
    mutate(across(.pred_class:class, \(name) str_remove(name, '\\.pred_'))) |> 
    ggplot(aes(x = reorder(class, probability), y = probability, fill = probability)) +
    geom_col(show.legend = FALSE, alpha = 0.8) +
    scale_y_continuous(labels = label_percent(), breaks = seq(0.25, 1, by = 0.25)) +
    scale_fill_viridis_c(option = 'B', begin = 0.1, end = 0.9) +
    coord_flip() +
    facet_grid(id ~ model, labeller = label_wrap_gen(width = 10)) +
    labs(y = 'Predicted probability', x = NULL) +
    theme(strip.background = element_blank(),
          strip.text.x = element_text(face = 'bold'),
          panel.grid.minor = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(linetype = 3, color = 'gray'))
)


plot_width <- 11
plot_height <- 9
plot_dpi <- 700
plot_path <- function(name) here('Plots', paste0(name, '.png'))
plot_save <- function(name, plot, width = plot_width, height = plot_height, dpi = plot_dpi) {
  ggsave(
    filename = plot_path(name), 
    plot = plot,
    width = width, 
    height = height, 
    device = 'png',
    dpi = plot_dpi
  )
}

plot_save('predicted_probabilities', prediction_plot_1)
plot_save('predicted_probabilities_ensemble', prediction_plot_2)

# saveRDS(extract_workflow(multinomial_fit), here('Model results', 'multinomial_fit.rds'))
# saveRDS(extract_workflow(knn_fit), here('Model results', 'knn_fit.rds'))
# saveRDS(extract_workflow(rf_fit), here('Model results', 'rf_fit.rds'))
# saveRDS(extract_workflow(xgb_fit), here('Model results', 'xgb_fit.rds'))

write_csv(preds_df, here('Model results', 'predictions.csv'))







