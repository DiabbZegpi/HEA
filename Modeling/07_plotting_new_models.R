library(tidyverse)
library(tidymodels)
tidymodels_prefer()

source(here('Modeling', '00_functions.R'))
rf_results <- readRDS(here('Model results', 'RF v2', 'rf_results_v2.rds'))
rf_fit <- readRDS(here('Model results', 'RF v2', 'rf_fit_v2.rds'))

# Recall matrix
rf_recall_mat <- plot_confusion_matrix(rf_results, Truth)
# Precision matrix
rf_precision_mat <- plot_confusion_matrix(rf_results, Prediction)

roc_curves <- 
  rf_results |> 
  collect_predictions(parameters = select_by_one_std_err(rf_results, -min_n, metric = 'accuracy')) |> 
  group_by(id) |> 
  roc_curve(Phase, .pred_AM:.pred_IM) |>
  ungroup()

rf_auc <- 
  rf_results |> 
  collect_predictions(parameters = select_by_one_std_err(rf_results, -min_n, metric = 'accuracy')) |> 
  roc_auc(truth = Phase, .pred_AM:.pred_IM, estimator = 'macro_weighted') |> 
  pull(.estimate)

# ROC curves
rf_ROC_AUC <- 
  roc_curves |>
  mutate(group = paste(id, .level)) |>
  ggplot(aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(alpha = 0.8, aes(group = group)) +
  geom_abline(linetype = 2) +
  facet_wrap(~.level) +
  labs(
    title = str_glue('Overall ROC AUC evaluated on 10-fold CV: {round(rf_auc, 2)}'),
    subtitle = 'Best Random Forest hyperparameter configuration'
  )

# Save plots
plot_save('rf_v2_recall', rf_recall_mat)
plot_save('rf_v2_precision', rf_precision_mat)
plot_save('rf_v2_ROC', rf_ROC_AUC)