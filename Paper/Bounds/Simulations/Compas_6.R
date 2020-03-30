rm(list=ls())

# ------------------------------------------------------------------------------
## load packages

library(dplyr)
library(readr)
library(ggplot2)
library(here)
here()

# ------------------------------------------------------------------------------
## load packages

df <- read_csv(here('Recidivism-tvb', 'Paper', 'Bounds', 'Simulations', 'datasets', 'compas', 'COMPAS_score_test.csv'),
         col_types = cols())

write_csv(
  data.frame(Risk = ifelse(df$decile_score>6, 1, 0)),
  here('Recidivism-tvb', 'Paper', 'Bounds', 'Simulations', 'datasets', 'compas', 'pred_compas6.csv')
)

