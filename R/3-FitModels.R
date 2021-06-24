
library(tidyverse)
library(glmmTMB)
library(lme4)

# Load rsf data
rsf_dat <- readRDS('output/rsf_data.rds') %>%
  # Scale and centre variables
  mutate(across(c(roaddist, mixedwood_hr, roaddist_hr), scale)) %>%
  # Add column for weighted likelihood of 1000 for available points
  mutate(weight = 1000^(1-case))

# Define covariates
# Fixed effects
fixed_vars <- c('mixedwood', 'roaddist')
ranef_vars <- c('(1 | elkyear)', 
             '(0 + roaddist | elkyear) + (0 + mixedwood | elkyear)')
gfr_vars <- c('mixedwood*roaddist_hr', 'mixedwood*mixedwood_hr',
           'roaddist*roaddist_hr', 'roaddist*mixedwood_hr')


# Initiate folders to collect models
dir.create('output/individual_models/')
dir.create('output/population_models/')

# Loop to fit models
for(elk in unique(rsf_dat$elkyear)) {
  
  # Split out data from individual
  elk_dat <- rsf_dat %>%
    filter(elkyear == elk)
  # Split out the second block for testing data
  test_elk <- elk_dat %>%
    filter(block == max(block))
  # Split out the first block for training data
  train_elk <- elk_dat %>%
    filter(block == min(block))
  
  # Split out data from remaining population in same training block
  pop_dat <- rsf_dat %>%
    filter(! elkyear == elk & block == unique(train_elk$block))
  
  # Stop if the individual does not have ≥ 2 blocks to compare
  if(length(unique(elk_dat$elkblock)) < 2) next
  
  # Fit individual-level models
  # Test model (as.vector to help with predict function)
  test_elk_mod <- glm(case ~ mixedwood + as.vector(roaddist), 
                      data = test_elk,
                      family = binomial)
  # Training model (as.vector to help with predict function)
  train_elk_mod <- glm(case ~ mixedwood + as.vector(roaddist), 
                      data = train_elk,
                      family = binomial)
  # Save
  saveRDS(test_elk_mod, paste0('output/individual_models/test_ey', 
                              elk, '_block', unique(test_elk$block), '.rds'))
  saveRDS(train_elk_mod, paste0('output/individual_models/train_ey', 
                              elk, '_block', unique(train_elk$block), '.rds'))
  
  # Create folder for specific population models
  pop_folder <- paste0('output/population_models/ey', 
                      elk, '_block', unique(train_elk$block))
  
  # Skip to the next elkyear if there are already 200 bootstraps
  if(file.exists(paste0(pop_folder, '/gfr_it200.rds')) | 
     file.exists(paste0(pop_folder, '/ranef_it200.rds'))) next
  
  # Create the folder
  dir.create(pop_folder)
  
  # Fit bootstrapped population-level models in repeat loop
  # Set initial iteration to zero
  iteration <- 0
  repeat {
    # Set repeat iteration
    iteration <- iteration + 1
    
    # Generate file names for what will be the saved models
    gfr_file <- paste0(pop_folder, '/gfr_it', iteration, '.rds')
    ranef_file <- paste0(pop_folder, '/ranef_it', iteration, '.rds')
    # Skip repeat if iteration already exists
    if(file.exists(gfr_file) & file.exists(ranef_file)) next
    
    # Take bootstrap sample of individuals same length as n individuals
    boots <- data.frame(elkyear = sample(unique(pop_dat$elkyear),
                                         size = length(unique(pop_dat$elkyear)),
                                         replace = T)) %>%
      rownames_to_column(var = 'sample_no')
    # Subset those individuals from the data frame
    boots_dat <- right_join(pop_dat, boots)
    
    # Fit the models
    # GFR model
    gfr_mod <- glmmTMB(reformulate(c(gfr_vars, ranef_vars), response = 'case'),
                       data = boots_dat,
                       weights = weight,
                       family = binomial())
    # Ranef model
    ranef_mod <- glmmTMB(reformulate(c(fixed_vars, ranef_vars), response = 'case'),
                         data = boots_dat,
                         weights = weight,
                         family = binomial())
    
    # Save models
    saveRDS(gfr_mod, gfr_file)
    saveRDS(ranef_mod, ranef_file)
    
    # Break script when 200 iterations are reached
    if(iteration == 200) break
    
  }
  
}


