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
## load data


tb <- list(
  
  COMPAS6 = read_csv(here('Recidivism-tvb', 'Paper', 'Bounds', 'Simulations', 'datasets', 'compas', 'pred_compas6.csv')),
  
  UNCON = read_csv(here('Recidivism-tvb', 'Paper', 'Bounds', 'Simulations', 'datasets', 'compas', 'pred_uncon.csv')),
  
  FERM = read_csv(here('Recidivism-tvb', 'Paper', 'Bounds', 'Simulations', 'datasets', 'compas', 'pred_ferm.csv'),
                 col_names = F) %>%
    rename(Risk = X1),
  
  EQODDS = read_csv(here('Recidivism-tvb', 'Paper', 'Bounds', 'Simulations', 'datasets', 'compas', 'pred_eqodds.csv')) %>%
    rename(Risk = `# Risk`)
  
) %>%
  
  # join df with race and outcome
  purrr::map( ~ .x %>% dplyr::bind_cols(read_csv(here('Recidivism-tvb', 'Paper', 'Bounds', 'Simulations', 'datasets', 'compas', 'test.csv'),
                                    col_types = cols()) %>%
                             mutate(raceCaucasian = ifelse(raceCaucasian == 1, 'White', 'Black')) %>%
                             rename(Race = raceCaucasian) %>%
                             select(Race, two_year_recid))) %>%
  
  # create tables
  purrr::map(
    
    ~ list(White = .x %>% filter(Race == 'White'),
                    Black = .x %>% filter(Race == 'Black')) %>%
      # create table
      purrr::map( ~ table(.x$two_year_recid, .x$Risk))
    
  )




# ------------------------------------------------------------------------------
## compute and plot metrics for varying alpha 
  
# compute metrics
metric <- tb %>%
  purrr::map( ~ list(FPR = .x %>% purrr::map_dfr(~ fpr_compute(.x), .id = 'Race'), 
                     FNR = .x %>% purrr::map_dfr(~ fnr_compute(.x), .id = 'Race'),
                     PPV = .x %>% purrr::map_dfr(~ ppv_compute(.x), .id = 'Race')
  ) %>%
    dplyr::bind_rows(.id = 'Metric') %>%
    rename(Type = ind)) %>%
  bind_rows(.id = 'Method')


##

g2 <- ggplot(metric %>%
         filter((Race == 'Black' & Type == 'obs') | (Race == 'White' & Type != 'obs')),
       aes(alpha, values)) +
  
  # lines
  geom_path(aes(alpha, values, 
                group = paste0(Race, Metric, Type), 
                linetype = Metric, col = Race),
            size = 2) + 
  
  # set color manually
  scale_color_manual(values = c('grey60', "#E69F00")) +
  
  # separate experiments
  facet_grid(rows = vars(Method)) +
  
  # set limit x axis
  xlim(0,0.2) +
  ylim(0,1) +
  
  # other theme
  theme_bw() + 
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=25)) + 
  labs(x = expression(alpha),
       y = 'Classification metric value')





ggsave(width = 6, height = 12, here('Recidivism-tvb', 'Plots', 'Bounds_error_rates_debias.pdf'))


  


# ------------------------------------------------------------------------------
## compute and plot metrics for varying alpha 


metrics_alpha_1 <- tb %>%
  purrr::map( ~ list(.x, list(0.125,0)) %>%
                purrr::pmap(~ .x %>% compute_metrics_alpha_1(.y)) %>%
                bind_rows(.id = 'Race') %>%
                rename(Metric = ind)) %>%
  bind_rows(.id = 'Method')



## plot
ggplot(metrics_alpha_1,
       aes(alpha_1, values)) +
  
  # lines
  geom_path(aes(alpha_1, values, 
                group = paste0(Race, Metric), 
                linetype = Metric, col = Race),
            size = 2) + 
  
  # method
  facet_grid(rows = vars(Method)) +
  
  # set color manually
  scale_color_manual(values = c('grey60', "#E69F00")) +
  
  # set limit x axis
  scale_x_continuous(limits = c(0,0.13),
                     breaks = seq(0,0.12, by = 0.04)) +
  ylim(0,1) +
  
  # other theme
  theme_bw() + 
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=25)) + 
  labs(x = expression(alpha['1']),
       y = 'Classification metric value')



ggsave(width = 6, height = 12, here('Recidivism-tvb', 'Plots', 'Metric_disp_alpha_debias.pdf'))
