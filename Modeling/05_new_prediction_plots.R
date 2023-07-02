library(here)
library(tidyverse)
library(tidymodels)
tidymodels_prefer()

rf_fit <- readRDS(here('Model results', 'rf_fit.rds'))

new_hea <- 
  tribble(
    ~id, ~dHmix, ~Elect.Diff, ~VEC,
    'Alloy #1', 0.07504, 0.125, 7.606,
    'Alloy #2', 0.4704, 0.124, 7.646
  )

prob_predictions <- 
  predict(rf_fit, new_data = new_hea, type = 'prob') |> 
  mutate(id = c('Alloy #1', 'Alloy #2'))

class_preds <- 
  prob_predictions |> 
  pivot_longer(cols = .pred_AM:.pred_IM, names_to = '.pred_class') |> 
  group_by(id) |> 
  slice_max(value, with_ties = FALSE) |> 
  ungroup() |> 
  select(-value)

preds_df <- 
  prob_predictions |> 
  left_join(class_preds, by = 'id') 


# Plot --------------------------------------------------------------------

theme_set(theme_minimal())
theme_update(
  text = element_text(size = 14),
  axis.title = element_text(face = 'bold'),
  strip.background = element_rect(fill = '#ddddee'),
  strip.text = element_text(size = 12),
  legend.key = element_rect(fill = NA),
  legend.text = element_text(size = 12),
  axis.ticks = element_blank(),
  aspect.ratio = 1,
  panel.spacing = unit(2, 'lines')
)

labels <- 
  new_hea |> 
  mutate(label = paste0('VEC: ', VEC, '\ndx: ', Elect.Diff, '\ndHmix: ', dHmix))

(
  prediction_plot <- preds_df |> 
    pivot_longer(.pred_AM:.pred_IM, names_to = 'class', values_to = 'probability') |> 
    mutate(across(.pred_class:class, \(name) str_remove(name, '\\.pred_'))) |> 
    ggplot(aes(x = reorder(class, probability), y = probability, fill = probability)) +
    geom_col(show.legend = FALSE, alpha = 0.8) +
    geom_text(aes(label = paste0(round(probability, 4) * 100, '%')), nudge_y = 0.05, size = 5) +
    geom_label(
      data = labels, 
      aes(label = label, x = 2, y = 0.27),
      size = 5,
      inherit.aes = FALSE,
      hjust = 0,
      fill = NA
    ) +
    scale_y_continuous(
      labels = label_percent(), 
      breaks = seq(0.25, 1, by = 0.25),
      expand = expansion(add = c(0, 0.04))
    ) +
    scale_fill_viridis_c(option = 'B', begin = 0.1, end = 0.9) +
    coord_flip() +
    facet_wrap(vars(id), labeller = label_wrap_gen(width = 10)) +
    labs(y = 'Predicted probability', x = NULL) +
    theme(strip.background = element_blank(),
          strip.text.x = element_text(face = 'bold', size = 18),
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

plot_save('new_alloys_july2023', prediction_plot)
