rm(list=ls())

# ------------------------------------------------------------------------------
## load packages

library(dplyr)
library(readr)
library(ggplot2)
library(glmnet)
library(here)
library(future)
library(fastDummies)
library(purrr)
library(furrr)
here()

# ------------------------------------------------------------------------------

# load, subset data, & rename data to match PCS data
predictions <- read_csv(here('DATA', 'COMPAS', 'compas-scores-two-years.csv')) %>%
  filter(race == 'African-American' | race == 'Caucasian') %>%
  # transform file into same format as SC
  rename(y = two_year_recid, race_new = race, score = decile_score) %>%
  mutate(race_new = ifelse(race_new == 'Caucasian', 'White', 'Black')) %>%
  arrange(score)


# ------------------------------------------------------------------------------
## CODE BELOW IS COPIED FROM coefficients_analytical_SC.R - 
# write function for both/develop package
# ------------------------------------------------------------------------------
# get bounds

# function for testing the significance of the coefficient in a logistic regresison
# for some combinations of hidden recidivists
# gamma is the proportion of hidden recidivists in the white non recidivist pop
# n_rep specifies how fine the grid is
flipping_obs <- function(gamma, n_rep=200){
  
  # number of hidden recidivists
  K <- ceiling(gamma*n_non_recid_white)
  
  # storage
  storage <- c()
  
  # step for computations
  step <- (n_non_recid_white-K)/n_rep
  step_current <- 0
  
  for(alternating_score in c(1,rep(2,n_rep),3)){ # low, middle, high
    
    # flipping for White population
    if(alternating_score == 1){
      toflip <- non_recid_white_rows
    } else if(alternating_score != 1 & alternating_score != 3){
      step_current <- step_current + step
      toflip <- non_recid_white_rows[step_current:(round(step_current)+K)]
    } else{
      toflip <- rev(non_recid_white_rows)
    }
    
    if(K>0){
      
      # initialize true outcome
      y_true <- predictions$y
      
      # which outcomes are flipped
      toflip <- toflip[1:K]
      
      # flip outcomes
      y_true[toflip] <- rep(1, K)
    }
    
    logreg <- glm(y_true ~ score + race, 
                  data.frame(y_true = y_true,
                             score = predictions$score,
                             race = predictions$race_new), 
                  family=binomial(link='logit'))
    
    # chi-square test
    an <- anova(logreg,test="Chisq")
    if(an$`Pr(>Chi)`[3]<0.01){
      if(logreg$coefficients[3]<0){
        significant <- 1
      } else{
        significant <- 2
      }
    } else {
      significant <- 0
    }
    
    
    coeff <- coef(logreg); attributes(coeff) <- NULL
    storage <- rbind(storage, c(gamma, alternating_score, coeff[2], coeff[3], significant))
  }
  
  return(data.frame(Gamma = round(storage[,1],4), Flipped = storage[,2] %>% as.factor(),
                    Coef_decile_score = storage[,3], Coef_Race = storage[,4],
                    Significance = storage[,5]))
}

# indexes of whites not rearrested
non_recid_white_rows <- which(predictions$y == 0 & predictions$race_new == 'White')
# get number of white non recidivists
n_non_recid_white <- length(non_recid_white_rows)

# create list of value over which we can iterate
gamma_seq <- seq(0.005, 0.25, by=0.0025) # 'by' should be set to 0.0025 to get the plot
gamma_seq_list <- as.list(gamma_seq)
names(gamma_seq_list) <- paste0(gamma_seq)

# divide work into cores
future::plan(multiprocess, workers = 4)

# get output with parallelized process
grid_output <- gamma_seq_list %>% 
  future_map(~flipping_obs(., n_rep=200)) %>% # this should be set to 200 for fine grid
  bind_rows() 


# manually add Gamma=0 (no hidden recividist)
gamma <- 0
logreg <- glm(y ~ score + race_new, 
              predictions, family=binomial(link='logit'))
an <- anova(logreg,test="Chisq")
if(an$`Pr(>Chi)`[3]<0.01){
  significant <- 1
} else{
  significant <- 0
}
coeff <- coef(logreg); attributes(coeff) <- NULL
for(alternating_score in c(1,3)){
  grid_output <- rbind(grid_output, c(gamma, alternating_score, coeff[2], coeff[3], significant))
}


# ------------------------------------------------------------------------------
# plot bounds (figure 2a)


# rescale Gamma to definition of alpha as in the paper
n_white <- sum(predictions$race_new=='White')
grid_output$Gamma <- grid_output$Gamma*n_non_recid_white/n_white

# get extrema (bounds)
bounds <- data.frame(Gamma=unique(grid_output$Gamma), 
                     lb=grid_output$Coef_Race[which(grid_output$Flipped==1)],
                     ub=grid_output$Coef_Race[which(grid_output$Flipped==3)])

# plot
ggplot(bounds) + 
  ylab('Race (Caucasian) coefficient') +
  xlab(expression(alpha)) + 
  geom_ribbon(data=bounds, aes(x=Gamma, ymin=lb, ymax=ub),alpha=0.6,fill="grey80") + 
  geom_line(aes(x=Gamma, y=lb)) + 
  geom_line(aes(x=Gamma, y=ub)) +
  geom_point(data=grid_output, 
             mapping=aes(Gamma, Coef_Race, color=as.factor(Significance)), 
             inherit.aes = FALSE, size=2) + 
  theme_bw() + 
  guides(color=FALSE) + 
  scale_color_manual(values=c('forestgreen', 'orange', 'red')) + 
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=25)) 


ggsave(width = 12, height = 6, here('Recidivism-tvb', 'Plots', 'Logistic_bounds_race_COMPAS.pdf'))

