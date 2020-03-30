rm(list = ls())

# ------------------------------------------------------------------------------
## load packages

library(dplyr)
library(readr)
library(ggplot2)
library(here)
here()

# ------------------------------------------------------------------------------
## load and subset data

# load data
df.train <- read_csv(here('Recidivism-tvb', 'Paper', 'Bounds', 'Simulations', 'datasets', 'compas', 'train_and_validation.csv'))
df.test <- read_csv(here('Recidivism-tvb', 'Paper', 'Bounds', 'Simulations', 'datasets', 'compas', 'test.csv'))


# regress and predict
logit.train <- glm(two_year_recid ~ ., data = df.train, family = binomial())
train.predictions <- predict(logit.train, newdata = df.train, type = 'response')
test.predictions <- predict(logit.train, newdata = df.test, type = "response")


# Output predictions
write_csv(data.frame(Risk = ifelse(test.predictions>0.5,1,0)), 
          here('Recidivism-tvb', 'Paper', 'Bounds', 'Simulations', 'datasets', 'compas', 'pred_uncon.csv'))


# Output predictions and race for Equalized odds method
write_csv(data.frame(prediction = train.predictions, group = df.train$raceCaucasian, label = df.train$two_year_recid),
          here('Recidivism-tvb', 'Paper', 'Bounds', 'Simulations', 'datasets', 'compas', 'df_train_uncon_for_eqodds.csv'))
write_csv(data.frame(prediction = test.predictions, group = df.test$raceCaucasian, label = df.test$two_year_recid),
          here('Recidivism-tvb', 'Paper', 'Bounds', 'Simulations', 'datasets', 'compas', 'df_test_uncon_for_eqodds.csv'))


