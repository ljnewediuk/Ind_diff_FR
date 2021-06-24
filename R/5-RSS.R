
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

for(i in list.files('output/population_models/')) {
  
  sub_ey <- str_split(i, '_')[[1]][1]
  sub_ey <- as.numeric(str_extract(sub_ey, '[[:digit:]]+'))
  
  sub_block <- str_split(i, '_')[[1]][2]
  sub_block <- as.numeric(str_extract(sub_block, '[[:digit:]]+'))
  
  sub_hr_test <- hr_covs %>%
    ungroup %>%
    filter(elkyear == sub_ey) %>%
    filter(block == max(block))
  
  sub_hr_train <- hr_covs %>%
    ungroup %>%
    filter(elkyear == sub_ey) %>%
    filter(block == min(block))
  
  sub_test_mod <- readRDS(paste0('output/individual_models/test_ey', 
                                 sub_ey, '_block', sub_block + 1, '.rds'))
  
  sub_train_mod <- readRDS(paste0('output/individual_models/train_ey', 
                                  sub_ey, '_block', sub_block, '.rds'))
  
  # Data frame for predicted data from loc x1
  x1_test <- data.frame(mixedwood = 1,
                        roaddist = lower_roaddist)
  
  # Data frame for predicted data from loc x2 (comparison to x1)
  x2_test <- x1_test %>%
    mutate(mixedwood = 0)
  
  # Predict and calculate
  logp_1 <- predict(sub_test_mod, newdata = x1_test)
  logp_2 <- predict(sub_test_mod, newdata = x2_test)
  # Log RSS
  logRSS <- logp_1 - logp_2
  
  for(mods in 1:200) {
    
    gfr_mod <- readRDS(paste0('output/population_models/', i, '/', 
                              'gfr_it', mods, '.rds'))
    ranef_mod <- readRDS(paste0('output/population_models/', i, '/', 
                              'ranef_it', mods, '.rds'))
      
    # Data frame for predicted data from loc x1
    x1_gfr <- data.frame(mixedwood = 1,
                     roaddist = lower_roaddist,
                     roaddist_hr = sub_hr_test$roaddist_hr,
                     mixedwood_hr = sub_hr_test$mixedwood_hr,
                     elkyear = NA, 
                     weight = NA) 
    # Data frame for predicted data from loc x2 (comparison to x1)
    x2_gfr <- x1_gfr %>%
      mutate(mixedwood = 0)
    
    # Predict and calculate
    logp_1 <- predict(gfr_mod, newdata = x1_gfr, type = 'link', re.form = NA)
    logp_2 <- predict(gfr_mod, newdata = x2_gfr, type = 'link', re.form = NA)
    # Log RSS
    logRSS <- logp_1 - logp_2
    
  }
  
}










# Data frame for predicted data from loc x1
x1 <- data.frame(cort_ng_g_sc = seq(from = med_cort, to = max_cort, 
                                    length.out = 100),
                 hab = 1,
                 step_id_ = NA,
                 cos_ta_ = mean_ta,
                 log_sl_ = mean_sl,
                 id = NA) 
# Data frame for predicted data from loc x2 (comparison to x1)
x2 <- x1 %>%
  mutate(cort_ng_g_sc = med_cort)

# Loop to calculate RSS
# Separate into cover and crop models
for(i in c('cover', 'crop')) {
  # Initiate df to collect results
  mod_rss <- data.frame()
  # Set initial iteration
  iteration <- 0
  # Loop through each model separately
  for(mods in list.files('output/model_boots/', pattern = i)) {
    # Increase iteration number
    iteration <- iteration + 1
    # Rename hab column as habitat name
    colnames(x1)[2] <- paste(i)
    colnames(x2)[2] <- paste(i)
    # Get the appropriate model
    rss_mod <- readRDS(paste('output/model_boots/', mods, sep = ''))
    # Calculate log RSS between loc x1 and x2
    for(j in c('pre', 'post')) {
      x1 <- x1 %>% 
        mutate(period = paste(j, 'calv', sep = '_'))
      x2 <- x2 %>% 
        mutate(period = paste(j, 'calv', sep = '_'))
      # Predict and calculate
      logp_1 <- predict(rss_mod, newdata = x1, type = 'link', re.form = NA, se.fit = F)
      logp_2 <- predict(rss_mod, newdata = x2, type = 'link', re.form = NA, se.fit = F)
      # Log RSS
      logRSS <- logp_1 - logp_2
      # Assign predicted selection to new df for binding
      assign(paste(i, j, sep = '_'), 
             data.frame(period = j, 
                        habitat = i, 
                        cort = seq(from = median(model_dat$cort_ng_g, na.rm = T)/1000, 
                                   to = max(model_dat$cort_ng_g, na.rm = T)/1000, 
                                   length.out = 100), 
                        iteration,
                        logRSS))
      
    }
    # Bind predicted selection to larger data frame
    mod_rss <- mod_rss %>% 
      rbind(get(paste(i, 'post', sep = '_'))) %>% 
      rbind(get(paste(i, 'pre', sep = '_')))
    
  }
  
  # Assign
  assign(paste(i, 'mod_rss', sep = '_'), mod_rss)
}

# Bind together
all_rss <- cover_mod_rss %>%
  rbind(crop_mod_rss) %>%
  # Calculate median and CI of RSS
  group_by(period, habitat, cort) %>%
  summarize(selection = median(logRSS),
            lower = quantile(logRSS, probs = 0.05),
            upper = quantile(logRSS, probs = 0.95))

# # Save RSS data
# saveRDS(all_rss, 'output/rss_results.rds')

# Plot
# tiff('figures/rss_gc_habitat.tiff', width = 10, height = 8, units = 'in', res = 300)
ggplot(all_rss, aes(x = cort, y = exp(selection), group = period, col = period)) +
  geom_hline(yintercept = exp(0), linetype = 'dashed') +
  scale_colour_manual(values = c('#e4bb3f', '#5ac18e')) +
  scale_fill_manual(values = c('#e4bb3f', '#5ac18e')) +
  geom_ribbon(alpha = 0.3, col = NA,
              aes(ymin = exp(selection) - exp(lower), 
                  ymax = exp(selection) + exp(upper), fill = period)) +
  geom_line(size = 1) +
  theme(panel.background = element_rect(colour = 'black', fill = 'white'),
        strip.background = element_rect(fill = 'white'),
        strip.text = element_text(size = 18),
        panel.grid = element_blank(),
        axis.text = element_text(size = 15), 
        axis.title = element_text(size = 18)) +
  facet_wrap(~ habitat, scales = 'free') +
  xlab('Fecal cortisol (microgram/g)') +
  ylab('log RSS for habitat')

