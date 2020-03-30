rm(list=ls())

# ------------------------------------------------------------------------------
## load packages

library(dplyr)
library(readr)
library(ggplot2)
library(here)
here()

# ------------------------------------------------------------------------------
## Define function for computation of error rates


# FPR
fpr_compute <- function(tb){
  
  K <- 1:(tb[1,1]+tb[1,2])
  
  list(
    ub = pmin(1, tb[1,2]/(tb[1,1]+tb[1,2]-K)),
    lb = pmax(0, (tb[1,2]-K)/(tb[1,1]+tb[1,2]-K)),
    obs = rep(tb[1,2]/(tb[1,1]+tb[1,2]), max(K))
  ) %>%
    stack() %>%
    mutate(alpha = rep(K/(sum(tb)),3))
}


# FNR
fnr_compute <- function(tb){
  
  K <- 1:(tb[1,1]+tb[1,2])
  
  list(
    ub = pmin(1, (tb[2,1]+K)/(tb[2,1]+tb[2,2]+K)),
    lb = pmax(0, tb[2,1]/(tb[2,1]+tb[2,2]+K)),
    obs = rep(tb[2,1]/(tb[2,1]+tb[2,2]), max(K))
  ) %>%
    stack() %>%
    mutate(alpha = rep(K/(sum(tb)),3))
}


ppv_compute <- function(tb){
  
  K <- 1:(tb[1,1]+tb[1,2])
  
  list(
    ub = pmin(1, (tb[2,2]+K)/(tb[1,2]+tb[2,2])),
    lb = rep(tb[2,2]/(tb[1,2]+tb[2,2]), max(K)),
    obs = rep(tb[2,2]/(tb[1,2]+tb[2,2]), max(K))
  ) %>%
    stack() %>%
    mutate(alpha = rep(K/(sum(tb)),3))
}


# ------------------------------------------------------------------------------
## Compute metrics for error rates: fixed alpha

compute_metrics_alpha_1 <- function(tb, alpha){
  
  
  if(alpha > 0 ){
    
    K <- 0:round(alpha*sum(tb))
    
    out <- list(
      FNR = (tb[2,1]+(max(K)-K))/(tb[2,1]+tb[2,2]+max(K)),
      FPR = (tb[1,2]-K)/(tb[1,1]+tb[1,2]-max(K)),
      PPV = (tb[2,2]+K)/(tb[1,2]+tb[2,2])
    ) %>%
      stack() %>%
      mutate(alpha_1 = rep(K/(sum(tb)),3))
    
  } else{
    
    list(
      FNR = rep(tb[2,1]/(tb[2,1]+tb[2,2]), 2),
      FPR = rep(tb[1,2]/(tb[1,1]+tb[1,2]), 2),
      PPV = rep(tb[2,2]/(tb[1,2]+tb[2,2]), 2) 
    ) %>%
      stack() %>%
      mutate(alpha_1 = rep(c(0,0.125),3))
    
  }

}


compute_metrics_obs <- function(tb){
  
  list(
    FNR = rep(tb[2,1]/(tb[2,1]+tb[2,2]), 2),
    FPR = rep(tb[1,2]/(tb[1,1]+tb[1,2]), 2),
    PPV = rep(tb[2,2]/(tb[1,2]+tb[2,2]), 2) 
  ) %>%
    stack() %>%
    mutate(alpha_1 = rep(c(0,0.125),3))
  
}






