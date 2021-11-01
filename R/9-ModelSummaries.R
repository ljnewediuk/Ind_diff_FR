
library(tidyverse)
library(lme4)
library(glmmTMB)

# Load home range coverage
hr_covs <- readRDS('output/hr_covers.rds') %>%
  # Rename blocks into first and last
  mutate(block = ifelse(block %in% c(1, 3), 'first', 'second')) %>%
  # Remove extra columns
  select(- c(elkblock, roaddist_hr, mixedwood_hr)) %>%
  # Pivot to calculate difference in hr cover between blocks
  pivot_wider(id_cols = c(elkyear), 
              names_from = block, 
              values_from = c(unsc_roaddist_hr_km, unsc_mixedwood_hr)) %>%
  # Calculate difference
  mutate(mw_hr_diff = abs(unsc_mixedwood_hr_first - unsc_mixedwood_hr_second),
         rd_hr_diff = abs(unsc_roaddist_hr_km_first - unsc_roaddist_hr_km_second))

# Load model outputs
all_mods <- data.frame()
# Start looping through model outputs
for(i in list.files('output/individual_models/')) {
  
  print(i)
  # Get model type
  mod_type <- str_split(i, '_')[[1]][1]
  # Get elkyear
  sub_ey <- str_split(i, '_')[[1]][2]
  sub_ey <- as.numeric(str_extract(sub_ey, '[[:digit:]]+'))
  # Get block number
  sub_block <- str_split(i, '_')[[1]][3]
  sub_block <- as.numeric(str_extract(sub_block, '[[:digit:]]+'))
  
  # Load model output
  mod_output <- readRDS(paste0('output/individual_models/', i))
  # Tidy the model
  mod_tidy <- broom::tidy(mod_output) %>%
    # Remove intercept
    filter(term != '(Intercept)') %>%
    mutate(term = factor(term, labels = c('Mixed_forest', 'Road')),
           elkyear = sub_ey,
           block = sub_block,
           mod_type)
  
  # Bind together
  all_mods <- rbind(all_mods, mod_tidy)
  
}

selection_moments <- all_mods %>%
  group_by(term) %>%
  summarize(median_b = median(estimate, na.rm = T),
            perc_lower = quantile(estimate, probs = 0.025, na.rm = T),
            perc_upper = quantile(estimate, probs = 0.975, na.rm = T))

# Join model outputs to home range covers
dat_summary <- all_mods %>%
  # Rename blocks into first and last
  mutate(block = ifelse(block %in% c(1, 3), 'first', 'second')) %>%
  ungroup() %>%
  left_join(hr_covs) %>%
  # Remove extra columns 
  select(- c(unsc_mixedwood_hr_first, unsc_mixedwood_hr_second,
         unsc_roaddist_hr_km_first, unsc_roaddist_hr_km_second,
         statistic, p.value, mod_type, std.error)) %>%
  # Pivot to calculate difference in selection between blocks
  pivot_wider(names_from = c(term, block), values_from = estimate) %>%
  # Calculate differences
  mutate(mw_b_diff = abs(Mixed_forest_first - Mixed_forest_second),
         rd_b_diff = abs(Road_first - Road_second)) %>%
  # Remove extra columns
  select(- c(Mixed_forest_first, Mixed_forest_second,
             Road_first, Road_second))
  
  


