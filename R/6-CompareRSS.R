
library(tidyverse)

# Start dataframe to summarize RSS for each model by elkyear
rss_summ <- data.frame()
# Start dataframe to summarize variance in RSS for each model by elkyear
var_summ <- data.frame()

for(habitat in c('Road', 'Mixed_forest')) {

  # Get corresponding condition (use one comparison for each covariate)
  cond_type <- ifelse(habitat == 'Road', 'x1-mw0-rd0.05_x2-mw0-rd0.50',
                      'x1-mw1-rd0.95_x2-mw0-rd0.95')
  
  # Load RSS results and filter by condition
  rss_dat <- readRDS('output/rss_results_predict.rds') %>%
    filter(cond == cond_type)
  # Load home range covers
  hr_covs <- readRDS('output/hr_covers.rds')
  
  # Start dataframe to collect z-scores
  rss_zscores <- data.frame()
  # Start dataframe to collect variance
  rss_vars <- data.frame()
  
  # Loop through individuals
  for(indiv in unique(rss_dat$elkyear)) {
    
    # Subset RSS data by model
    rss_indiv_test <- rss_dat %>%
      filter(elkyear == indiv & model == 'test')
    rss_indiv_train <- rss_dat %>%
      filter(elkyear == indiv & model == 'train')
    rss_indiv_gfr <- rss_dat %>%
      filter(elkyear == indiv & model == 'gfr')
    rss_indiv_ranef <- rss_dat %>%
      filter(elkyear == indiv & model == 'ranef')
    # Subset home range cover data by training or testing
    hr_indiv_test <- hr_covs %>%
      filter(elkyear == indiv & block == rss_indiv_test$block)
    hr_indiv_train <- hr_covs %>%
      filter(elkyear == indiv & block == rss_indiv_test$block + 1)
    
    # Get HR cover for opposite covariate to covariate being summarized
    hr_diff <- ifelse(rss_indiv_test$cond %in% c('x1-mw1-rd0.95_x2-mw0-rd0.95'),
                      hr_indiv_test$roaddist_hr - hr_indiv_train$roaddist_hr,
                      hr_indiv_test$mixedwood_hr - hr_indiv_train$mixedwood_hr)
    
    # Calculate difference in individual selection
    beta_diff <- rss_indiv_test$rss - rss_indiv_train$rss
    # Calculate z-scores for each model
    z_gfr <- rss_indiv_test$rss - mean(rss_indiv_gfr$rss) / sd(rss_indiv_gfr$rss)
    z_ranef <- rss_indiv_test$rss - mean(rss_indiv_ranef$rss) / sd(rss_indiv_ranef$rss)
    
    # Create row for individual z-score
    zscore_row <- data.frame(elkyear = indiv,
                             beta_diff,
                             hr_diff,
                             gfr = z_gfr,
                             ranef = z_ranef)
    # Create row for individual variance
    var_row <- data.frame(elkyear = indiv, 
                          model = c('gfr', 'ranef'), 
                          var = c(var(rss_indiv_gfr$rss), var(rss_indiv_ranef$rss)),
                          cond = habitat)
    # Bind together
    rss_vars <- rbind(rss_vars, var_row)
    rss_zscores <- rbind(rss_zscores, zscore_row)
    
  }
  
  # Pivot models into column
  rss_row <- rss_zscores %>%
    pivot_longer(cols = c(gfr, ranef), 
                 values_to = 'zscore',
                 names_to = 'model') %>%
    mutate(cond = habitat)
  
  # Bind together
  rss_summ <- rbind(rss_summ, rss_row)
  var_summ <- rbind(var_summ, rss_vars)
  
}

# Join RSS scores with variance
rss_summ <- rss_summ %>%
  left_join(var_summ, by = c('elkyear', 'model', 'cond'))

# Save
saveRDS(rss_summ, 'output/rss_summary.rds')

