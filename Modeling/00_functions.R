library(tidyverse)
library(ggpubr)

theme_set(theme_pubclean())
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
    geom_text(size = 6) +
    scale_fill_gradient2(
      low = "#ffffff", 
      high = "#ef8a62", 
      mid = "#ffffff", 
      limits = c(0, 1), 
      breaks = seq(0, 1, by = 0.2),
      na.value = 'white'
    ) +
    guides(fill = guide_colorbar(barwidth = 10)) +
    theme(
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 14),
      axis.text.y = element_text(size = 14),
      axis.title = element_text(size = 16),
      panel.grid = element_blank(),
      legend.title = element_text(vjust = 0.5, hjust = 0.5, margin = margin(r = 10), size = 16),
      legend.text = element_text(size = 14),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.ticks = element_blank()
    )  +
    labs(fill = str_glue('Frequency\n({direction} add to 1)'), x = 'True class', y = 'Predicted class')
}
