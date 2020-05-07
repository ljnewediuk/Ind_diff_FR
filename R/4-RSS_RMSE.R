
library(dplyr)
library(data.table)
library(Metrics)


#### 2 - Load data ----

indiv_mods <- readRDS('results/RSF_outputs/id_hab_covariates.rds')
indiv_mods <- indiv_mods[type=='OOS']
gfr_mods <- readRDS('results/RSF_outputs/gfr_covariates.rds')
no_FR_mods <- readRDS('results/RSF_outputs/no_FR_covariates.rds')
ranef_mods <- readRDS('results/RSF_outputs/ranef_covariates.rds')

# Remove any indiv_mod iteration with fewer than 150 points
indiv_mods <- indiv_mods[numb_pts>=150]

####### BOOTSTRAPPING ###########

# For each year, randomly select 6 rows from the allmods data table and allbetas data table with replacement
# Make comparisons between combinations of betas and model estimates

# Calculate goodness of model fit (i.e. accuracy of model predictions
# of individual beta coefficients) as the RMSE - ADD THESE TO THE FIGURE

#### 2 - Calculate mean of covariates ----

for(mods in c('gfr', 'no_FR', 'ranef')){
  
  model <- get(paste(mods, 'mods', sep='_'))
  rd <- mean(model[term=='road']$estimate)
  mw <- mean(model[term=='mixedwood']$estimate)
  conif <- mean(model[term=='coniferous']$estimate)
  rd_rd <- mean(model[term=='road:mean_road']$estimate)
  mw_mw <- mean(model[term=='mixedwood:mean_mixedwood']$estimate)
  rd_mw <- mean(model[term=='road:mean_mixedwood']$estimate)
  mw_rd <- mean(model[term=='mixedwood:mean_road']$estimate)
  
  assign(paste(mods, 'means', sep='_'), data.table(model=rep(mods, 7), 
                     covariate=c('rd', 'mw', 'conif', 'rd_rd', 
                                 'mw_mw', 'rd_mw', 'mw_rd'), beta=c(rd, mw, 
                                                                   conif, rd_rd, mw_mw, rd_mw, mw_rd)))
  
}

#### 3 - Calculate correlations ----

all_rmse <- data.table()

for(i in unique(indiv_mods$elkyear)){
  
  indiv_sub <- indiv_mods[elkyear==i]
  
  gfr_mw_ests <- c()
  ranef_mw_ests <- c()
  
  indiv_sub_mw <- indiv_sub[term=='mixedwood']
  
  for(j in 1:nrow(indiv_sub_mw)){
    
    mw_gfr <- (gfr_means[covariate=='mw']$beta + gfr_means[covariate=='mw_rd']$beta*log(indiv_sub_mw[j,]$mean_road) + gfr_means[covariate=='mw_mw']$beta*log(indiv_sub_mw[j,]$mean_mixedwood))
    mw_ranef <- ranef_means[covariate=='mw']$beta
    
    gfr_mw_ests <- c(gfr_mw_ests, mw_gfr)
    ranef_mw_ests <- c(ranef_mw_ests, mw_ranef)
    
  }
  
  gfr_rd_ests <- c()
  ranef_rd_ests <- c()
  
  indiv_sub_rd <- indiv_sub[term=='road']
  
  for(k in 1:nrow(indiv_sub_rd)){
    
    rd_gfr <- (gfr_means[covariate=='rd']$beta + gfr_means[covariate=='rd_mw']$beta*log(indiv_sub_mw[j,]$mean_mixedwood) + gfr_means[covariate=='rd_rd']$beta*log(indiv_sub_mw[j,]$mean_road))
    rd_ranef <- ranef_means[covariate=='rd']$beta
    
    gfr_rd_ests <- c(gfr_rd_ests, rd_gfr)
    ranef_rd_ests <- c(ranef_rd_ests, rd_ranef)
    
  }
  
  indiv_mw_ests <- indiv_sub[term=='mixedwood']$estimate
  indiv_rd_ests <- indiv_sub[term=='road']$estimate
  
  rmse_mw_gfr <- rmse(indiv_mw_ests, gfr_mw_ests)
  rmse_mw_ranef <- rmse(indiv_mw_ests, ranef_mw_ests)
  rmse_rd_gfr <- rmse(indiv_rd_ests, gfr_rd_ests)
  rmse_rd_ranef <- rmse(indiv_rd_ests, ranef_rd_ests)
  
  rmse_id <- data.table(elkyear=i, ranef_mw=rmse_mw_ranef, gfr_mw=rmse_mw_gfr, ranef_rd=rmse_rd_ranef, gfr_rd=rmse_rd_gfr)
  
  all_rmse <- rbind(all_rmse, rmse_id)
}

#########
## SAVE OUTPUT

means <- rbind(gfr_means, no_FR_means, ranef_means)

saveRDS(means, 'results/RSF_outputs/model_means.rds')

saveRDS(all_rmse, 'results/gof/all_rmse.rds')

