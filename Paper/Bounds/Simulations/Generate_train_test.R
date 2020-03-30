rm(list=ls())

# ------------------------------------------------------------------------------
## load packages

library(dplyr)
library(readr)
library(ggplot2)
library(here)
here()

# ------------------------------------------------------------------------------
## load and subset data

raw.data <- read_csv(here('DATA', 'COMPAS', 'compas-scores-two-years.csv'),
                     col_types = cols()) %>%
  filter(race == 'African-American' | race == 'Caucasian')

# Filter data
crime.data <- raw.data %>%
  # filter(days_b_screening_arrest <= 30) %>%
  # filter(days_b_screening_arrest >= -30) %>%
  filter(two_year_recid != -1) %>%
  filter(c_charge_degree != "O") %>%
  filter(score_text != 'N/A')

# Length of stay column
crime.data$length_of_stay <- as.numeric(as.Date(crime.data$c_jail_out) - as.Date(crime.data$c_jail_in))

# Reduce to just white and black defendants
df <- crime.data %>%
  filter(race %in% c("African-American", "Caucasian")) %>%
  mutate(race = factor(race))

# Extract data of interest
df.sub <- df %>%
  select(sex, age, race, juv_fel_count, juv_misd_count, juv_other_count, priors_count,
         c_charge_degree, two_year_recid, decile_score) 

# ------------------------------------------------------------------------------
## Random splitting, stratified

# Randomly split into train and test
set.seed(1)
df.sub <- df.sub %>%
  group_by(race, two_year_recid) %>%
  mutate(random = sample(n()),
         in.train = as.numeric(random <= 0.7*n()),
         in.valid = as.numeric(random <= 0.2*n())) %>%
  select(-random) %>%
  ungroup()


# Check sampling
df.sub %>%
  count(in.train, race, two_year_recid)


# create dummies
df.sub.matrix <- as_tibble(model.matrix(~ . - 1, data = df.sub)) %>%
  select(-sexMale)

##
# Output train and test data + COMPAS scores
train.df <- df.sub.matrix %>%
  filter(in.train == 1) %>%
  select(-in.train, -in.valid, -decile_score)
test.df <- df.sub.matrix %>%
  filter(in.train == 0) %>%
  select(-in.train, -in.valid, -decile_score)

write_csv(train.df, here('Recidivism-tvb', 'Paper', 'Bounds', 'Simulations', 'datasets', 'compas', 'train_and_validation.csv'))
write_csv(test.df, here('Recidivism-tvb', 'Paper', 'Bounds', 'Simulations', 'datasets', 'compas', 'test.csv'))

# COMPAS scores
cs_train.df <- df.sub.matrix %>%
  filter(in.train == 1) %>%
  select(decile_score)
cs_test.df <- df.sub.matrix %>%
  filter(in.train == 0) %>%
  select(decile_score)
#write_csv(train.df, here('DATA', 'COMPAS', 'Simulations', 'COMPAS_score_train_and_validation.csv'))
write_csv(cs_test.df, here('Recidivism-tvb', 'Paper', 'Bounds', 'Simulations', 'datasets', 'compas', 'COMPAS_score_test.csv'))


