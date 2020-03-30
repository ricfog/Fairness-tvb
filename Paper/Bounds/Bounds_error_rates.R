rm(list=ls())

# ------------------------------------------------------------------------------
## load packages

library(dplyr)
library(readr)
library(ggplot2)
library(here)
here()

source(here('Recidivism-tvb', 'Paper', 'Bounds', 'functions_bounds.R'))

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
  select(race, decile_score, two_year_recid)  %>%
  # add Propublica's low/high scores
  mutate(risk = ifelse(decile_score > 4, 1, 0))



# ------------------------------------------------------------------------------
## Compute and plot bounds for error rates: varying alpha


# create tables
tb_race <- list(White = df.sub %>% filter(race == 'Caucasian'),
                Black = df.sub %>% filter(race == 'African-American')) %>%
  # create table
  purrr::map( ~ table(.x$two_year_recid, .x$risk))




# compute metrics and reformat into df
metric <- list(FPR = tb_race %>% purrr::map_dfr(~ fpr_compute(.x), .id = 'Race'), 
     FNR = tb_race %>% purrr::map_dfr(~ fnr_compute(.x), .id = 'Race'),
     PPV = tb_race %>% purrr::map_dfr(~ ppv_compute(.x), .id = 'Race')
     ) %>%
  #purrr::map( ~ .x %>%
  #              dplyr::bind_rows(.id = 'Metric')) %>%
  dplyr::bind_rows(.id = 'Metric') %>%
  rename(Type = ind)



## Plot for varyng alpha

## plot
ggplot(metric %>%
         filter((Race == 'Black' & Type == 'obs') | (Race == 'White' & Type != 'obs')),
       aes(alpha, values)) +
  
  # lines
  geom_path(aes(alpha, values, 
                group = paste0(Race, Metric, Type), 
                linetype = Metric, col = Race),
            size = 2) + 
  
  # set color manually
  scale_color_manual(values = c('grey60', "#E69F00")) +
  
  
  # set limit x axis
  xlim(0,0.2) +
  ylim(0,1) +
  
  # other theme
  theme_bw() + 
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=25)) + 
  theme(legend.text=element_text(size=20)) +
  labs(x = expression(alpha),
       y = 'Classification metric value') 




ggsave(width = 12, height = 6, here('Recidivism-tvb', 'Plots', 'Bounds_error_rates.pdf'))






# ------------------------------------------------------------------------------
## Compute metrics for error rates: fixed alpha




metrics_alpha_1 <- list(tb_race, list(0.125, 0)) %>%
  purrr::pmap( ~ .x %>% compute_metrics_alpha_1(.y)) %>%
  dplyr::bind_rows(.id = 'Race') %>%
  rename(Metric = ind)


 
# compute point where observed metric for whites = unobserved metric
positions_equality <- list(metrics = list(unique(compute_metrics_obs(tb_race$White)$value)[1],
        unique(compute_metrics_obs(tb_race$White)$value)[2],
        unique(compute_metrics_obs(tb_race$White)$value)[3]),
     names = list('FNR', 'FPR', 'PPV')) %>%
  #purrr::pmap( ~ print(paste(.x, .y)))
  purrr::pmap( ~ which(round(.x,3) == round(metrics_alpha_1$values,3) & metrics_alpha_1$Race == 'White' & metrics_alpha_1$Metric == .y)[1]) %>%
  unlist()


## plot
ggplot(metrics_alpha_1,
       aes(alpha_1, values)) +
  
  # lines
  geom_path(aes(alpha_1, values, 
                group = paste0(Race, Metric), 
                linetype = Metric, col = Race),
            size = 2) + 
  
  # set color manually
  scale_color_manual(values = c('grey60', "#E69F00")) +
  
  # observed metrics for whites
  geom_point(data=(metrics_alpha_1 %>% filter(Race == 'White'))[positions_equality,],
             aes(alpha_1, values),
             size = 4
  ) + 
  
  
  # set limit x axis
  scale_x_continuous(limits = c(0,0.13),
                     breaks = seq(0,0.12, by = 0.04)) +
  ylim(0,1) +
  
  # other theme
  theme_bw() + 
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=25)) + 
  theme(legend.text=element_text(size=20)) +
  labs(x = expression(alpha['1']),
       y = 'Classification metric value')



ggsave(width = 12, height = 6, here('Recidivism-tvb', 'Plots', 'Metric_disp_alpha.pdf'))





