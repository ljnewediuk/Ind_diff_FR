#################################################################
#################################################################
## 4 - RSS RMSE - COMPARE PERFORMANCE OF GFR AND RANEF MODELS
#################################################################
#################################################################
## Returns:
## Calculate mean model estimates to use for the RSS plots in
## the next script
## 
## RMSE calculations:
##    a) Creates RSS vectors for each individual/iteration,
##       plus a vector from the gfr and ranef with individual
##       home range characteristics
##    b) Compares the fit of the gfr and ranef RSS vectors
##       to the individual RSS using RMSE

### Packages ----
libs <- c('data.table', 'dplyr', 'Metrics')
lapply(libs, require, character.only = TRUE)

#### 1 - Load data ----

indiv_mods <- readRDS('results/RSF_outputs/mw_rd_RSFs.rds')
indiv_mods <- indiv_mods[type=='OOS']
gfr_mods <- readRDS('results/RSF_outputs/gfr_covariates.rds')
no_FR_mods <- readRDS('results/RSF_outputs/no_FR_covariates.rds')
ranef_mods <- readRDS('results/RSF_outputs/ranef_covariates.rds')

# Remove any indiv_mod iteration with fewer than 150 points
indiv_mods <- indiv_mods[numb_pts>=150]

#### 2 - Calculate mean of covariates for RSS plots (next script) ----

for(mods in c('gfr', 'no_FR', 'ranef')){
  
  model <- get(paste(mods, 'mods', sep='_'))
  rd <- mean(model[term=='road']$estimate)
  mw <- mean(model[term=='mixedwood']$estimate)
  rd_mean <- mean(model[term=='mean_road']$estimate)
  mw_mean <- mean(model[term=='mean_mixedwood']$estimate)
  rd_rd <- mean(model[term=='road:mean_road']$estimate)
  mw_mw <- mean(model[term=='mixedwood:mean_mixedwood']$estimate)
  rd_mw <- mean(model[term=='road:mean_mixedwood']$estimate)
  mw_rd <- mean(model[term=='mixedwood:mean_road']$estimate)
  
  assign(paste(mods, 'means', sep='_'), data.table(model=rep(mods, 8), 
                                                   covariate=c('rd', 'mw', 'rd_mean', 'mw_mean', 'rd_rd', 
                                                               'mw_mw', 'rd_mw', 'mw_rd'), beta=c(rd, mw, rd_mean, mw_mean,
                                                                                                  rd_rd, mw_mw, rd_mw, mw_rd)))
  
}

#### 3 - Calculate correlations ----

# Initiate data table
all_rmse <- data.table()

for(i in unique(indiv_mods$elkyear)){
  
  # Subset out an individual
  indiv_sub <- indiv_mods[elkyear==i]
  
  # Loop through all model iterations
  for(itn in unique(indiv_sub$iteration)){
    
    # Calculate for both habitats
    for(hab in c('road', 'mixedwood')){
      
      # Set condtions for habitat type
      if(hab=='mixedwood'){
        hab1 <- 'mixedwood'
        hab2 <- 'road'
        delta_hi <- 1-seq(1,0,length.out=100)
      } else {
        hab1 <- 'road'
        hab2 <- 'mixedwood'
        delta_hi <- seq(8.5,3.9,length.out=100)
      }
      
      # Set parameters from iteration
      # Mean of individual home range
      hi <- indiv_sub[, get(paste("mean", hab1, sep='_'))[term==hab1 & iteration==itn]]
      hj <- indiv_sub[, get(paste("mean", hab2, sep='_'))[term==hab1 & iteration==itn]]
      # Individual coefficients 
      Bi_ind <- indiv_sub[iteration==itn & term==hab1]$estimate
      Bj_ind <- indiv_sub[iteration==itn & term==hab2]$estimate
      # ranef coefficients
      Bi_ranef <- ranef_mods[iteration==itn & term==hab1]$estimate
      Bj_ranef <- ranef_mods[iteration==itn & term==hab2]$estimate
      # gfr coefficients
      Bi_gfr <- gfr_mods[iteration==itn & term==hab1]$estimate
      Bj_gfr <- gfr_mods[iteration==itn & term==hab2]$estimate
      Bi_mu <- gfr_mods[term==paste('mean', hab1, sep='_') & iteration==itn]$estimate
      Bj_mu <- gfr_mods[term==paste('mean', hab2, sep='_') & iteration==itn]$estimate
      Bij <- gfr_mods[term==paste(hab1, paste('mean', hab2, sep='_'), sep=':') & iteration==itn]$estimate
      Bji <- gfr_mods[term==paste(hab2, paste('mean', hab1, sep='_'), sep=':') & iteration==itn]$estimate
      Bii <- gfr_mods[term==paste(hab1, paste('mean', hab1, sep='_'), sep=':') & iteration==itn]$estimate
      Bjj <- gfr_mods[term==paste(hab2, paste('mean', hab2, sep='_'), sep=':') & iteration==itn]$estimate
      
      # Calculate individual RSS
      if(hab=='mixedwood'){
        rss_indiv <- (hi-delta_hi)*Bi_ind + log(log(hj)^Bj_ind)
      } else {
        rss_indiv <- log((log(hi)/(log(hi)-(log(hi)-delta_hi)))^Bi_ind) + hj*Bj_ind
      }
      # Calculate gfr RSS
      if(hab=='mixedwood'){
        rss_gfr <- (hi-delta_hi)*(Bi_gfr+Bij*log(hj)+Bii*hi) + log(hj^(Bj_gfr+Bji*hi+Bjj*log(hj))) + hi*Bi_mu + log(hj)^Bj_mu
      }
      if(hab=='road'){
        rss_gfr <- log((log(hi)/(log(hi)-(log(hi)-delta_hi)))^(Bi_gfr+Bij*hj+Bii*log(hi))) + hj*(Bj_gfr+Bji*log(hi)+Bjj*hj) + log(hi)^Bi_mu + hj*Bj_mu
      }
      # Calculate ranef RSS
      if(hab=='mixedwood'){
        rss_ranef <- (hi-delta_hi)*Bi_ranef + log(log(hj)^Bj_ranef)
      }
      if(hab=='road'){
        rss_ranef <- log((log(hi)/(log(hi)-(log(hi)-delta_hi)))^Bi_ranef) + hj*Bj_ranef
      }
      
      # Calculate RMSE
      rmse_gfr <- rmse(rss_indiv, rss_gfr)
      rmse_ranef <- rmse(rss_indiv, rss_ranef)
      
      # Compile into data.table
      rmse_indiv <- data.table(id=i, iteration=itn, habitat=hab, rmse_gfr, rmse_ranef)
      all_rmse <- rbind(all_rmse, rmse_indiv)
    }
    
  }
  
}

#########
## SAVE OUTPUT

means <- rbind(gfr_means, no_FR_means, ranef_means)

saveRDS(means, 'results/RSF_outputs/model_means.rds')

saveRDS(all_rmse, 'results/gof/all_rmse.rds')
