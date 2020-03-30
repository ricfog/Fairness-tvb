rm(list=ls())

# ------------------------------------------------------------------------------
## load packages

library(dplyr)
library(readr)
library(ggplot2)
library(vroom)
library(glmnet)
library(here)
library(future)
library(fastDummies)
library(purrr)
library(furrr)
here()

# ------------------------------------------------------------------------------
## load and subset data

df <- vroom(here('DATA', 'PCS', 'pcs.csv')) %>%
  filter(race_new == 'Black' | race_new == 'White') %>% 
  droplevels() %>%
  filter(dofage >= 19, dofage <= 65) %>%
  rename(y = yr3arrest)
  


# ------------------------------------------------------------------------------
## get design matrix to construct instrument

# columns for regression
col_keep <- c('y', 
              'dofage', 'sex_new',
              'totalpriorcharges_adult', 'totalpriorarrests_adult', 
              'totalpriorarrests_juv', 'totalpriorcharges_juv',
              'tpropertyM_adult','tDrugM','tPersonalM_adult',
              'tDrugM_adult', 'tPublicOrder_adult',
              'tPublicAdm_adult','tDUI_adult',
              'ogs', 'prs')

# get design matrix
design_matrix <- df %>%
  select(c(col_keep, race_new)) %>%
  tidyr::drop_na()


# ------------------------------------------------------------------------------
# Use this to create a list folds with row indices in the training and test sets
# design matrix

# split train & test
design_matrix$set <- sample(0:1, dim(design_matrix)[1], replace = TRUE)

# get indices
set_folds <- caret::createFolds(unique(design_matrix$set), k = 2, returnTrain = TRUE)
set <- unique(design_matrix$set)
set_index_folds <- lapply(1:2,
                          function(fold_i) {
                            train_fold <- set_folds[[fold_i]]
                            train_set <- set[train_fold]
                            
                            train_i <- which(design_matrix$set %in% train_set)
                            test_i <- setdiff(1:nrow(design_matrix), train_i)
                            list("train_i" = train_i,
                                 "test_i" = test_i)
                          })

names(set_index_folds) <- paste0("test_", set)


# ------------------------------------------------------------------------------
## create matrix with dummies

x_matrix <- design_matrix %>%
  dplyr::select(-c(y, set, race_new))
# Use the fastDummies package:
init_colnames <- colnames(x_matrix)
cols_dummy <- c('sex_new', 'prs')
x_dummy_matrix <- x_matrix %>% 
  fastDummies::dummy_cols(remove_first_dummy = TRUE) %>%
  select(-cols_dummy)
new_colnames <- colnames(x_dummy_matrix)
dummy_colnames <- setdiff(new_colnames, init_colnames)
# Drop the dummy columns with less than 5% coverage - to deal with CV training:
less_than_10_perc <- dummy_colnames[which(apply(x_dummy_matrix[, dummy_colnames], 
                                                2, function(x) mean(x == 1) < .1))]
x_dummy_matrix <- x_dummy_matrix %>%
  dplyr::select(-less_than_10_perc)



# ------------------------------------------------------------------------------
# lasso function

lasso_train_predict <- function(X_dummy_matrix, design_matrix, set_index_folds, save_dir=NULL){
  plan(multiprocess, workers = length(set_index_folds)) 
  test_predictions <- future_map(set_index_folds,
                                 function(fold) {
                                   # Train the model on the training data:
                                   train_data_x <- as.matrix(X_dummy_matrix[fold$train_i,])
                                   
                                   # Create scaled version - REVISIT THIS
                                   # scaled_train_data_cov <- scale(train_data_x)
                                   
                                   # Response:
                                   train_data_y <- as.numeric(design_matrix$y[fold$train_i])
                                   
                                   # Now train the model (with the default objective as regression)
                                   # and for now only using the default settings:
                                   model <- glmnet::cv.glmnet(x=train_data_x, y=train_data_y,
                                                              family="binomial",
                                                              type.measure='auc',
                                                              nfolds=3)
                                   
                                   
                                   # Get the predictions using the lambda.1se
                                   test_data <- X_dummy_matrix[fold$test_i,]
                                   
                                   # output matrix
                                   out_data <- design_matrix[fold$test_i,]
                                   
                                   out_data$score <- as.numeric(predict(model,
                                                                         as.matrix(test_data),
                                                                         s="lambda.min",
                                                                         type='response'))
                                   
                                   return(out_data)
                                 })
  if(!(is.null(save_dir))) saveRDS(test_predictions, save_dir)
  return(test_predictions)
}


# ------------------------------------------------------------------------------
## fit lasso

predictions <- lasso_train_predict(x_dummy_matrix,
                          design_matrix,
                          set_index_folds) %>%
                bind_rows()

## convert score to integer (0-10) scale
predictions <- predictions %>%
  mutate(score = round(score,1)*10) %>%
  arrange(score)


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
# plot bounds (figure 2b)


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


ggsave(width = 12, height = 6, here('Recidivism-tvb', 'Plots', 'Logistic_bounds_race_SC.pdf'))
                    
        