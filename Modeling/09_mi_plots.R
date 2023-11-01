library(tidyverse)
library(tidymodels)
library(DALEX)
library(DALEXtra)
source('Modeling/00_functions.R')

rf_explainer <- readRDS('Model results/Explainers/general_rf_explainer.rds')
rf_varimp <- readRDS('Model results/Explainers/permutation_varimp_rf.rds')
rf_pdp <- readRDS('Model results/Explainers/pdp_rf.rds')


# Feature permutation importance plot -------------------------------------


reference_values <- rf_varimp |> 
  filter(variable %in% c('_full_model_', '_baseline_')) |> 
  group_by(variable) |> 
  summarise(dropout_loss = mean(dropout_loss)) |> 
  pull(name = variable)

(
  p1 <- rf_varimp |> 
    filter(!variable %in% c('_baseline_', '_full_model_')) |> 
    mutate(variable = fct_reorder(variable, dropout_loss, .fun = mean)) |> 
    ggplot(aes(x = variable, y = dropout_loss)) +
    geom_bar(
      stat = 'summary', 
      fun = 'mean',
      aes(fill = variable),
      show.legend = FALSE,
      alpha = 0.8
    ) +
    geom_boxplot(width = 0.3) +
    geom_hline(
      yintercept = reference_values['_full_model_'],
      linetype = 'dashed'
    ) +
    annotate(
      geom = 'text',
      x = 3.7,
      y = reference_values['_full_model_'] + 100,
      label = 'Full model reference value',
      size = 5
    ) +
    scale_x_discrete(
      labels = c(
        'Elect.Diff' = 'Elect. Diff',
        'dHmix' = 'dHmix',
        'VEC' = 'VEC',
        'gamma_factor' = 'Packing factor'
      )
    ) +
    scale_fill_viridis_d(
      option = 'B',
      begin = 0.1,
      end = 0.9
    ) +
    labs(
      x = NULL,
      y = 'Cross-entropy loss due to permutations'
    )
)


# Partial dependence profile ----------------------------------------------


(
  p2 <- rf_pdp$agr_profiles |> 
    set_names(str_remove_all(colnames(rf_pdp$agr_profiles), '_')) |> 
    mutate(
      label = str_remove(label, 'rf\\.'),
      vname = case_when(
        vname == 'Elect.Diff' ~ 'Elect. Diff',
        vname == 'gamma_factor' ~ 'Packing factor',
        .default = vname
      )
    ) |> 
    ggplot(aes(x = x, y = yhat, color = label)) +
    geom_line(size = 1, alpha = 0.8) +
    facet_wrap(~vname, scales = 'free_x') +
    scale_color_brewer(palette = 'Dark2') +
    scale_y_continuous(labels = label_percent()) +
    labs(
      x = NULL,
      y = 'Predicted probability by class',
      color = NULL
    ) +
    theme(
      strip.background = element_blank(),
      strip.text = element_text(size = 14, face = 'bold')
    )
)


# Plot save ---------------------------------------------------------------


plot_save('cross_entropy_loss', p1)
plot_save('partial_dependence_profiles', p2)















