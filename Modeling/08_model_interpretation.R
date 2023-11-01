library(tidyverse)
library(tidymodels)
library(DALEX)
library(DALEXtra)
tidymodels_prefer()

rf_fit <- readRDS('./rf_fit_v2.rds')
hea <- read_csv('./Data/HEA cleaned.csv')

# Data partition ----------------------------------------------------------

set.seed(123)
splits <- 
  initial_split(
    hea |> 
      mutate(
        Phase = factor(Phase),
        gamma_factor = ifelse(Phase %in% c('BCC', 'FCC', 'FCC+BCC'), 1.174, 1.1751)
      ), 
    strata = 'Phase',
    prop = 7/10
  )

train_phases <- training(splits) |> 
  arrange(desc(Elect.Diff)) |> 
  slice_tail(n = -2)

# Explainer ---------------------------------------------------------------

rf_explain <- explain_tidymodels(
  model = rf_fit,
  data = train_phases |> select(-Phase),
  y = train_phases |> pull(Phase),
  label = "rf"
)

# Model performance -------------------------------------------------------

model_performance(rf_explain) |> 
  plot(geom = 'boxplot')

# Permutation variable importance -----------------------------------------

rf_varimp <- model_parts(rf_explain)
plot(rf_varimp, show_boxplots = TRUE)


# Partial dependence plots (avg of ceteris-paribus profiles) --------------

set.seed(987)
rf_pdp <- model_profile(explainer = rf_explain, N = 500)
plot(rf_pdp)

# SHAP --------------------------------------------------------------------

set.seed(678)
new_obs <- testing(splits) |> 
  slice_sample(n = 10)

set.seed(456)
rf_shap <- shap_aggregated(
  explainer = rf_explain,
  new_observations = new_obs
)


















