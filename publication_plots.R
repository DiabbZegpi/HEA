library(tidyverse)
library(tidymodels)
library(here)
library(ggpubr)

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


(
  knn_confusion_matrix <- knn_results |> 
    conf_mat_resampled(parameters = select_best(knn_results, "accuracy")) |> 
    ggplot(aes(x = Truth, y = Prediction, fill = Freq, label = Freq)) +
    geom_tile(color = "gray") +
    geom_text() +
    scale_fill_gradient2(low = "#ffffff", high = "#ef8a62", mid = "#ffffff", midpoint = 3) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
          panel.grid = element_blank(),
          legend.title = element_text(vjust = 0.7),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.ticks = element_blank()) +
    labs(fill = "Frequency (%)", x = "True class", y = "Predicted class")
)

plot_width <- 11
plot_height <- 9
plot_dpi <- 700
plot_path <- function(name) here('Plots', paste0(name, '.tiff'))
plot_save <- function(name, plot, width = plot_width, height = plot_height, dpi = plot_dpi) {
  ggsave(
    filename = plot_path(name), 
    plot = plot,
    width = width, 
    height = height, 
    device = 'tiff',
    dpi = plot_dpi
  )
}

plot_save('knn_results', knn_plot)
plot_save('knn_confusion_matrix', knn_confusion_matrix)

bind_rows(
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
  labs(x = NULL, y = 'Cross-validation accuracy with 95% CI')





