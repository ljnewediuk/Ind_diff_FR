
library(tidyverse)
library(lme4)
library(glmmTMB)

# Load models
rsf_dat <- readRDS('output/rsf_data.rds') %>%
  # Create column for unscaled home range cover
  mutate(unsc_mixedwood_hr = mixedwood_hr,
         unsc_roaddist_hr_km = roaddist_hr/1000) %>%
  # Scale and centre variables
  mutate(across(c(roaddist, mixedwood_hr, roaddist_hr), scale)) %>%
  # Add column for weighted likelihood of 1000 for available points
  mutate(weight = 1000^(1-case))

# Get 0.025, 0.975, and median quantiles of roaddist from population
l_rd <- quantile(rsf_dat$roaddist, probs = 0.025)
m_rd <- quantile(rsf_dat$roaddist, probs = 0.5)
u_rd <- quantile(rsf_dat$roaddist, probs = 0.975)

# Summarize individuals by average home range cover
hr_covs <- rsf_dat %>%
  group_by(elkyear, block, elkblock) %>%
  summarize(roaddist_hr = mean(roaddist_hr),
            mixedwood_hr = mean(mixedwood_hr),
            unsc_roaddist_hr_km = mean(unsc_roaddist_hr_km),
            unsc_mixedwood_hr = mean(unsc_mixedwood_hr))

# Create data frame for RSS comparison conditions
rss_conditions <- data.frame(cond = c('x1-mw1-rd0.95_x2-mw0-rd0.95',
                                      'x1-mw1-rd0.50_x2-mw0-rd0.50',
                                      'x1-mw1-rd0.05_x2-mw0-rd0.05',
                                      'x1-mw1-rd0.95_x2-mw1-rd0.50',
                                      'x1-mw1-rd0.05_x2-mw1-rd0.50',
                                      'x1-mw0-rd0.95_x2-mw0-rd0.50',
                                      'x1-mw0-rd0.05_x2-mw0-rd0.50'),
                             mixedwood_x1 = c(1, 1, 1, 1, 1, 0, 0),
                             mixedwood_x2 = c(0, 0, 0, 1, 1, 0, 0),
                             roaddist_x1 = c(u_rd, m_rd, l_rd, u_rd, 
                                             l_rd, u_rd, l_rd),
                             roaddist_x2 = c(u_rd, m_rd, l_rd, rep(m_rd, 4)))

# Loop through population models to collect RSS for each individual (30 x 200)

# Load results in progress if started
if(file.exists('output/temp/rss_results.rds')) {
  rss_results <- readRDS('output/temp/rss_results.rds')
}

# Start loop
for(comp in 1: nrow(rss_conditions)) {
  
  # Get conditions for comparison
  cond <- rss_conditions[comp ,]$cond
  roaddist_x1 <- rss_conditions[comp ,]$roaddist_x1
  roaddist_x2 <- rss_conditions[comp ,]$roaddist_x2
  mixedwood_x1 <- rss_conditions[comp ,]$mixedwood_x1
  mixedwood_x2 <- rss_conditions[comp ,]$mixedwood_x2
  
  # Initiate df for comparison column or load existing results
  # if(file.exists('output/temp/rss_results_temp.rds')) {
  #   rss_col <- readRDS('output/temp/rss_results_temp.rds')
  # } else {
  #   rss_col <- data.frame()
  # }
  # 
  # 
  # Start loop
  for(i in list.files('output/population_models/')) {
    
    print(i)
    
    # Get elkyear
    sub_ey <- str_split(i, '_')[[1]][1]
    sub_ey <- as.numeric(str_extract(sub_ey, '[[:digit:]]+'))
    # Get block number
    sub_block <- str_split(i, '_')[[1]][2]
    sub_block <- as.numeric(str_extract(sub_block, '[[:digit:]]+'))
    
    # Subset appropriate row from HR covs to testing data
    sub_hr <- hr_covs %>%
      ungroup %>%
      filter(elkyear == sub_ey) %>%
      filter(block == sub_block + 1)
    
    # Individual testing and training RSS
    # Set x1 and x2 locations
    x1 <- data.frame(mixedwood = mixedwood_x1,
                     roaddist = roaddist_x1)
    x2 <- data.frame(mixedwood = mixedwood_x2,
                     roaddist = roaddist_x2)
    
    # Loop through model types
    for(mod in c('train', 'test')) {
      
      # Set appropriate block (testing or training)
      rss_block <- ifelse(mod == 'train', sub_block, sub_block + 1)
      
      # Get appropriate model
      sub_mod <- readRDS(paste0('output/individual_models/', mod, '_ey', 
                                sub_ey, '_block', rss_block, '.rds'))
      
      # Predict and calculate RSS
      logp_1 <- predict(sub_mod, newdata = x1)
      logp_2 <- predict(sub_mod, newdata = x2)
      logRSS <- logp_1 - logp_2
      assign(paste(mod, 'logRSS', sep = '_'), 
             data.frame(iteration = NA,
                        model = mod,
                        elkyear = sub_ey,
                        block = sub_block,
                        rss = logRSS,
                        cond,
                        row.names = NULL))
      
    }
    
    # Predict individual testing RSS using population model
    # Set x1 and x2 locations
    x1_pop <- data.frame(mixedwood = mixedwood_x1,
                         roaddist = roaddist_x1,
                         roaddist_hr = sub_hr$roaddist_hr,
                         mixedwood_hr = sub_hr$mixedwood_hr,
                         elkyear = NA, 
                         weight = NA)
    x2_pop <- x1_pop %>%
      mutate(mixedwood = mixedwood_x2,
             roaddist = roaddist_x2)
    
    # If the comparison condition has been done, move on
    if(exists('rss_results')) 
      if(nrow(plyr::match_df(rss_results, data.frame(cond, 
                                                     elkyear = sub_ey))) > 1) next
    
    # Loop through each model type
    for(mod in c('gfr', 'ranef')) {
      
      print(mod)
      
      # Initiate data frame for results
      pop_logRSS <- data.frame()
      
      for(its in 1:200) {
        
        # If the comparison condition has been done for iteration, move on
        # # Comparison column
        # if(exists('rss_col')) if(nrow(plyr::match_df(rss_col, 
        #                   data.frame(cond, model = mod,  elkyear = sub_ey, 
        #                              iteration = its))) == 1) next
        # Full results
        # if(exists('rss_results')) if(nrow(plyr::match_df(rss_results, 
        #                   data.frame(cond, model = mod,  elkyear = sub_ey, 
        #                              iteration = its))) == 1) next
          
        # Get appropriate model
        pop_mod <- readRDS(paste0('output/population_models/', i, '/', 
                                  mod, '_it', its, '.rds'))
        
        # Predict and calculate RSS
        logp_1 <- predict(pop_mod, newdata = x1_pop, type = 'link', re.form = NA)
        logp_2 <- predict(pop_mod, newdata = x2_pop, type = 'link', re.form = NA)
        # Log RSS
        logRSS <- logp_1 - logp_2
        
        # Create dataframe row
        pop_logRSS_row <- data.frame(iteration = its,
                                     model = mod,
                                     elkyear = sub_ey,
                                     block = sub_block,
                                     rss = logRSS,
                                     cond,
                                     row.names = NULL)
        # Bind with rest of dataframe
        pop_logRSS <- rbind(pop_logRSS, pop_logRSS_row)
        
      }
      
      # Assign population RSS dataframe
      assign(paste(mod, 'logRSS', sep = '_'), pop_logRSS)
      
    }
    
    # Bind all RSS together for comparison
    if(exists('rss_results')) {
      rss_results <- rbind(rss_results, 
                           train_logRSS, test_logRSS, gfr_logRSS, ranef_logRSS)
    } else {
      rss_results <- rbind(train_logRSS, test_logRSS, gfr_logRSS, ranef_logRSS)
    }
  
    # Save a temporary copy
    saveRDS(rss_results, 'output/temp/rss_results_temp.rds')
    
  }
  
  # # Bind comparison column to results
  # if(exists('rss_results')) {
  #   rss_results <- cbind(rss_results, rss_col)
  # } else {
  #   rss_results <- rss_col
  # }
  
  
}

# Save
saveRDS(rss_results, 'output/rss_results.rds')

