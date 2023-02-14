# Loading require packages and data ====

library(tidyverse)
library(tidymodels)
library(stacks)
library(doParallel)
library(here)

tidymodels_prefer()
theme_set(theme_bw())
theme_update( text = element_text(size = 14))
hea <- read_csv(here('Data', 'HEA cleaned.csv'))
set.seed(123)
splits <- initial_split(
  hea |> mutate(Phase = factor(Phase)), 
  strata = 'Phase',
  prop = 7/10
)

train_phases <- training(splits)
test_phases <- testing(splits)

all_cores <- parallel::detectCores(logical = FALSE)
cl <- makePSOCKcluster(all_cores)

list.files(here('Model results')) |> 
  enframe(name = NULL, value = 'filename') |> 
  filter(str_detect(filename, '\\.rds')) |> 
  transmute(loaded_file = map(filename, ~ here('Model Results', .x) |> readRDS())) |> 
  pull(loaded_file) |> 
  set_names(nm = str_remove(list.files(here('Model results'))[grepl(x = list.files(here('Model results')), pattern = '\\.rds')], '\\.rds$')) |> 
  list2env(envir = .GlobalEnv)

# Stacks ====

multiple_stack <- stacks() |> 
  add_candidates(knn_results) |> 
  # add_candidates(multinomial_results) |>
  add_candidates(rf_results)  
  # add_candidates(xgb_results)

registerDoParallel(cl)

stacks_blended <- blend_predictions(
  multiple_stack, 
  metric = metric_set(accuracy, precision, recall, roc_auc, f_meas), 
  penalty = seq(0.01, 0.2, length.out = 10),
  mixture = 1
)

saveRDS(stacks_blended, here('Model results', 'stacks_blended_3.rds'))

autoplot(stacks_blended)
autoplot(stacks_blended, type = 'member')
autoplot(stacks_blended, type = 'weights')

stacks_fit <- fit_members(stacks_blended)
stopImplicitCluster()
collect_parameters(stacks_fit, "knn_results")

saveRDS(stacks_fit, here('Model results', 'stacks_fit_3.rds'))

stack_vs_members <- test_phases |> 
  select(Phase) |> 
  bind_cols(predict(stacks_fit, new_data = test_phases, members = TRUE))

map_dfr(stack_vs_members, accuracy, truth = Phase, data = stack_vs_members) |> 
  mutate(member = colnames(stack_vs_members)) |> 
  arrange(desc(.estimate))
