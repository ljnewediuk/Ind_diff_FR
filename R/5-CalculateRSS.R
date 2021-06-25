
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
lower_roaddist <- quantile(rsf_dat$roaddist, probs = 0.025)
median_roaddist <- quantile(rsf_dat$roaddist, probs = 0.5)
upper_roaddist <- quantile(rsf_dat$roaddist, probs = 0.975)

# Summarize individuals by average home range cover
hr_covs <- rsf_dat %>%
  group_by(elkyear, block, elkblock) %>%
  summarize(roaddist_hr = mean(roaddist_hr),
            mixedwood_hr = mean(mixedwood_hr),
            unsc_roaddist_hr_km = mean(unsc_roaddist_hr_km),
            unsc_mixedwood_hr = mean(unsc_mixedwood_hr))

# Loop through population models to collect RSS for each individual (30 x 200)

# Initiate df
rss_results <- data.frame()

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
  x1 <- data.frame(mixedwood = 1,
                        roaddist = median_roaddist)
  x2 <- data.frame(mixedwood = 0,
                   roaddist = median_roaddist)
  
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
           data.frame(model = mod,
                      elkyear = sub_ey,
                      block = sub_block,
                      rss = logRSS,
                      row.names = NULL))
    
  }

  # Predict individual testing RSS using population model
  # Set x1 and x2 locations
  x1_pop <- data.frame(mixedwood = 1,
                       roaddist = median_roaddist,
                       roaddist_hr = sub_hr_test$roaddist_hr,
                       mixedwood_hr = sub_hr_test$mixedwood_hr,
                       elkyear = NA, 
                       weight = NA)
  x2_pop <- x1_pop %>%
    mutate(mixedwood = 0,
           roaddist = median_roaddist)
  
  # Loop through each model type
  for(mod in c('gfr', 'ranef')) {
    
    # Initiate data frame for results
    pop_logRSS <- data.frame()
    
    for(its in 1:200) {
      
      # Get appropriate model
      pop_mod <- readRDS(paste0('output/population_models/', i, '/', 
                                mod, '_it', its, '.rds'))
      
      # Predict and calculate RSS
      logp_1 <- predict(pop_mod, newdata = x1_pop, type = 'link', re.form = NA)
      logp_2 <- predict(pop_mod, newdata = x2_pop, type = 'link', re.form = NA)
      # Log RSS
      logRSS <- logp_1 - logp_2
      
      # Create dataframe row
      pop_logRSS_row <- data.frame(model = mod,
                                   elkyear = sub_ey,
                                   block = sub_block,
                                   rss = logRSS,
                                   row.names = NULL)
      # Bind with rest of dataframe
      pop_logRSS <- rbind(pop_logRSS, pop_logRSS_row)
      
    }
    
    # Assign population RSS dataframe
    assign(paste(mod, 'logRSS', sep = '_'), pop_logRSS)
    
  }
  
  # Bind all RSS together
  rss_results <- rbind(rss_results, 
                       train_logRSS, test_logRSS, gfr_logRSS, ranef_logRSS)
  
}

# Save
saveRDS(rss_results, 'output/rss_results.rds')







