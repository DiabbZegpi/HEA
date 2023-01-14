library(tidyverse)
library(tidymodels)
library(here)
library(ggpubr)
library(stacks)

tidymodels_prefer()
theme_set(theme_pubclean())
theme_update(text = element_text(size = 14),
             axis.title = element_text(face = 'bold'),
             strip.background = element_rect(fill = '#ddddee'),
             strip.text = element_text(size = 12),
             legend.key = element_rect(fill = NA),
             legend.text = element_text(size = 12),
             axis.ticks = element_blank(),
             aspect.ratio = 1,
             panel.spacing = unit(2, 'lines'))

list.files(here('Model results')) |> 
  enframe(name = NULL, value = 'filename') |> 
  transmute(loaded_file = map(filename, ~ here('Model Results', .x) |> readRDS())) |> 
  pull(loaded_file) |> 
  set_names(nm = str_remove(list.files(here('Model results')), '\\.rds$')) |> 
  list2env(envir = .GlobalEnv)

hea <- read_csv(here('Data', 'HEA cleaned.csv'))
set.seed(123)
splits <- initial_split(
  hea |> mutate(Phase = factor(Phase)), 
  strata = 'Phase',
  prop = 7/10
)

# Plot function -----------------------------------------------------------

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

plot_confusion_matrix <- function(.data, fill_by) {
  
  if (deparse(substitute(fill_by)) == 'Truth') {
    direction <- 'columns'
  } else {
    direction <- 'rows'
  }
  
  .data |> 
    conf_mat_resampled(parameters = select_best(.data, "accuracy")) |> 
    group_by({{ fill_by }}) |> 
    mutate(Freq = Freq / sum(Freq)) |> 
    ungroup() |> 
    ggplot(aes(
      x = Truth, y = Prediction, fill = Freq, 
      label = ifelse(!is.na(Freq), round(Freq, 2), '-')
    )) +
    geom_tile(color = "gray") +
    geom_text() +
    scale_fill_gradient2(low = "#ffffff", high = "#ef8a62", mid = "#ffffff", 
                         limits = c(0, 1), breaks = seq(0, 1, by = 0.2),
                         na.value = 'white') +
    guides(fill = guide_colorbar(barwidth = 10)) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
          panel.grid = element_blank(),
          legend.title = element_text(vjust = 0.5, hjust = 0.5, margin = margin(r = 10)),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.ticks = element_blank()) +
    labs(fill = str_glue('Frequency\n({direction} add to 1)'), x = 'True class', y = 'Predicted class')
}


# Making plots ------------------------------------------------------------

knn_confmat_recall <- plot_confusion_matrix(knn_results, Truth)
knn_confmat_precision <- plot_confusion_matrix(knn_results, Prediction)  
multinomial_confmat_recall <- plot_confusion_matrix(multinomial_results, Truth)
multinomial_confmat_precision <- plot_confusion_matrix(multinomial_results, Prediction)
rf_confmat_recall <- plot_confusion_matrix(rf_results, Truth)
rf_confmat_precision <- plot_confusion_matrix(rf_results, Prediction)
xgb_confmat_recall <- plot_confusion_matrix(xgb_results, Truth)
xgb_confmat_precision <- plot_confusion_matrix(xgb_results, Prediction)

stacks_results <- stacks_fit |> predict(new_data = training(splits)) |> 
  bind_cols(training(splits)) |> 
  select(Phase, .pred_class)

(
  stacks_confmat_recall <- stacks_results |> 
    rename(Prediction = .pred_class, Truth = Phase) |> 
    count(Prediction, Truth) |> 
    complete(Prediction, Truth, fill = list(n = 0)) |> 
    group_by(Truth) |> 
    mutate(Freq = n / sum(n)) |> 
    ungroup() |> 
    ggplot(aes(
      x = Truth, y = Prediction, fill = Freq, 
      label = ifelse(!is.na(Freq), round(Freq, 2), '-')
    )) +
    geom_tile(color = "gray") +
    geom_text() +
    scale_fill_gradient2(low = "#ffffff", high = "#ef8a62", mid = "#ffffff", 
                         limits = c(0, 1), breaks = seq(0, 1, by = 0.2),
                         na.value = 'white') +
    guides(fill = guide_colorbar(barwidth = 10)) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
          panel.grid = element_blank(),
          legend.title = element_text(vjust = 0.5, hjust = 0.5, margin = margin(r = 10)),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.ticks = element_blank()) +
    labs(fill = str_glue('Frequency\n(columns add to 1)'), x = 'True class', y = 'Predicted class')
)

(
  stacks_confmat_precision <- stacks_results |> 
    rename(Prediction = .pred_class, Truth = Phase) |> 
    count(Prediction, Truth) |> 
    complete(Prediction, Truth, fill = list(n = 0)) |> 
    group_by(Prediction) |> 
    mutate(Freq = n / sum(n)) |> 
    ungroup() |> 
    ggplot(aes(
      x = Truth, y = Prediction, fill = Freq, 
      label = ifelse(!is.na(Freq), round(Freq, 2), '-')
    )) +
    geom_tile(color = "gray") +
    geom_text() +
    scale_fill_gradient2(low = "#ffffff", high = "#ef8a62", mid = "#ffffff", 
                         limits = c(0, 1), breaks = seq(0, 1, by = 0.2),
                         na.value = 'white') +
    guides(fill = guide_colorbar(barwidth = 10)) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
          panel.grid = element_blank(),
          legend.title = element_text(vjust = 0.5, hjust = 0.5, margin = margin(r = 10)),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.ticks = element_blank()) +
    labs(fill = str_glue('Frequency\n(rows add to 1)'), x = 'True class', y = 'Predicted class')
)

(
  knn_plot <- knn_results |> 
    collect_metrics() |> 
    mutate(dist_power = paste0('Minkowski Distance Order:', dist_power)) |> 
    ggplot(aes(x = neighbors, y = mean, color = weight_func)) +
    geom_line() +
    geom_point(size = 1.5) +
    scale_color_brewer(palette = 'Set1') +
    facet_grid(.metric ~ dist_power, scales = 'free_y',
               labeller = labeller(.metric = c(
                 'accuracy' = 'Accuracy', 
                 'roc_auc' = 'ROC AUC',
                 "precision" = "Precision"
               ))) +
    labs(x = '# Nearest Neighbors', y = NULL, color = 'Distance Weighting Function') 
)

(
  multinomial_plot <- multinomial_results |> 
    collect_metrics() |> 
    mutate(penalty = log10(penalty)) |> 
    pivot_longer(cols = penalty:mixture) |> 
    mutate(name = ifelse(name == 'penalty', 'Amount of Regularization (log-10)',
                         'Proportion of Lasso Penalty')) |>
    ggplot(aes(x = value, y = mean, color = name)) +
    geom_point(size = 1.5, show.legend = FALSE) +
    scale_color_brewer(palette = 'Set1') +
    facet_grid(.metric ~ name, scales = 'free',
               labeller = labeller(.metric = c(
                 'accuracy' = 'Accuracy', 
                 'roc_auc' = 'ROC AUC',
                 "precision" = "Precision"
               ))) +
    labs(x = NULL, y = NULL) 
)

(
  rf_plot <- rf_results |> 
    collect_metrics() |> 
    mutate(mtry = factor(mtry)) |> 
    ggplot(aes(x = min_n, y = mean, color = mtry)) +
    geom_line() +
    geom_point(size = 1.5) +
    scale_color_brewer(palette = 'Set1') +
    facet_grid(.metric ~ ., scales = 'free_y',
               labeller = labeller(.metric = c(
                 'accuracy' = 'Accuracy', 
                 'roc_auc' = 'ROC AUC',
                 "precision" = "Precision"
               ))) +
    labs(x = 'Minimal Node Size', y = NULL, color = '# Randomly Selected Predictors') 
)

(
  xgb_plot <- xgb_results |> 
    collect_metrics() |> 
    mutate(learn_rate = log10(learn_rate)) |> 
    pivot_longer(cols = mtry:learn_rate) |> 
    ggplot(aes(x = value, y = mean, color = name)) +
    geom_point(size = 1.5, show.legend = FALSE) +
    scale_color_brewer(palette = 'Set1') +
    facet_grid(.metric ~ name, scales = 'free',
               labeller = labeller(.metric = c(
                 'accuracy' = 'Accuracy', 
                 'roc_auc' = 'ROC AUC',
                 "precision" = "Precision"
               ), name = c(
                 'mtry' = '# Randomly Selected Predictors',
                 'trees' = '# Trees',
                 'min_n' = 'Minimal Node Size',
                 'learn_rate' = 'Learning Rate (log-10)'
               ))) +
    labs(x = NULL, y = NULL) 
)

(
  stacks_plot <- stacks_blended$metrics |> 
    filter(.metric == 'accuracy') |> 
    mutate(penalty = log10(penalty)) |> 
    ggplot(aes(x = penalty, y = mean)) +
    geom_line() +
    geom_pointrange(aes(ymin = mean - 1.96 * std_err, ymax = mean + 1.96 * std_err)) +
    geom_text(
      data = stacks_blended$metrics |> 
        select(penalty, .metric, mean) |> 
        pivot_wider(names_from = .metric, values_from = mean) |> 
        mutate(num_members = round(num_members),
               penalty = log10(penalty)),
      aes(x = penalty, y = accuracy, label = num_members),
      inherit.aes = FALSE,
      size = 5, 
      nudge_x = 0.012,
      nudge_y = 0.010
    ) +
    labs(x = 'Amount of Lasso Regularization (log-10)',
         y = 'Mean Accuracy and 95% CI',
         subtitle = 'The number indicates the quantity of remaining\npredictors (models) in the ensemble')
)

(
  overall_results <- bind_rows(
    knn_results |> show_best('accuracy', n = 1) |> transmute(model = 'KNN', mean, std_err),
    multinomial_results |> show_best('accuracy', n = 1) |> transmute(model = 'Multinomial Reg', mean, std_err),
    xgb_results |> show_best('accuracy', n = 1) |> transmute(model = 'XGBoost', mean, std_err),
    rf_results |> show_best('accuracy', n = 1) |> transmute(model = 'Random Forest', mean, std_err),
    stacks_blended$metrics |> filter(.metric == 'accuracy') |> slice_max(mean, with_ties = FALSE) |> transmute(model = 'Ensemble', mean, std_err)
  ) |> 
    mutate(low_95 = mean - 1.96 * std_err,
           high_95 = mean + 1.96 * std_err,
           model = fct_reorder(model, mean)) |> 
    ggplot(aes(x = model, y = mean, color = model, ymin = low_95, ymax = high_95, label = round(mean, 2))) +
    geom_pointrange(show.legend = FALSE) +
    geom_text(color = 'black', nudge_x = 0.3, size = 5) +
    scale_color_brewer(palette = 'Set1') +
    labs(x = NULL, y = 'Cross-validation accuracy and 95% CI')
)




plot_save('knn_results', knn_plot)
plot_save('multinomial_results', multinomial_plot)
plot_save('rf_results', rf_plot)
plot_save('xgb_results', xgb_plot)
plot_save('ensemble_results', stacks_plot)
plot_save('overall_results', overall_results)


plot_save('knn_confmat_recall', knn_confmat_recall)
plot_save('multinomial_confmat_recall', multinomial_confmat_recall)
plot_save('rf_confmat_recall', rf_confmat_recall)
plot_save('xgb_confmat_recall', xgb_confmat_recall)
plot_save('ensemble_confmat_recall', stacks_confmat_recall)

plot_save('knn_confmat_precision', knn_confmat_precision)
plot_save('multinomial_confmat_precision', multinomial_confmat_precision)
plot_save('rf_confmat_precision', rf_confmat_precision)
plot_save('xgb_confmat_precision', xgb_confmat_precision)
plot_save('ensemble_confmat_precision', stacks_confmat_precision)
