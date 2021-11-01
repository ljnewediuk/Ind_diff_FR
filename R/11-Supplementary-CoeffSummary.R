
library(tidyverse)
library(glmmTMB)

# Compile coefficients from models for summarizing
# Initiate df (either from temp file or new df)
if(file.exists('output/temp/ranef_coeff_summary_temp.rds') &
   file.exists('output/temp/gfr_coeff_summary_temp.rds')) {
  all_gfr_coeffs <- readRDS('output/temp/gfr_coeff_summary_temp.rds')
  all_ranef_coeffs <- readRDS('output/temp/ranef_coeff_summary_temp.rds')
} else {
  all_gfr_coeffs <- data.frame()
  all_ranef_coeffs <- data.frame()
}

for(i in list.files('output/population_models')) {
  # Extract elk and block
  # Get elkyear
  sub_ey <- str_split(i, '_')[[1]][1]
  sub_ey <- as.numeric(str_extract(sub_ey, '[[:digit:]]+'))
  # Get block number
  sub_block <- str_split(i, '_')[[1]][2]
  sub_block <- as.numeric(str_extract(sub_block, '[[:digit:]]+'))
  
  cat(c('elkyear:', sub_ey, 'block:', sub_block, '\n\n'))
  
  # If elk-block already completed, move on to next
  if(sub_ey %in% all_ranef_coeffs$elkyear & 
     sub_block %in% all_ranef_coeffs$block) next
  # Initiate data frames
  ranef_coeffs <- data.frame()
  gfr_coeffs <- data.frame()
  # Loop through bootstrapped models
  for(j in 1:200){
    for(mod in c('ranef', 'gfr')) {
      # Load model
      sub_mod <- readRDS(paste0('output/population_models/', i,
                                  '/', mod, '_it', j, '.rds'))  %>%
        # Tidy
        broom.mixed::tidy() %>%
        # Remove intercept term
        filter(! term %in% c('(Intercept)', 'sd__(Intercept)')) %>%
        mutate(elkyear = sub_ey,
               block = sub_block)
      # Assign
      assign(paste(mod, 'mod', sep = '_'), sub_mod)
    }
    
    # Bind together
    ranef_coeffs <- rbind(ranef_coeffs, ranef_mod)
    gfr_coeffs <- rbind(gfr_coeffs, gfr_mod)
    
  }
  
  # Bind to other elk blocks
  all_gfr_coeffs <- rbind(all_gfr_coeffs, gfr_coeffs)
  all_ranef_coeffs <- rbind(all_ranef_coeffs, ranef_coeffs)
  
  # Save temporary file
  saveRDS(all_gfr_coeffs, 'output/temp/gfr_coeff_summary_temp.rds')
  saveRDS(all_ranef_coeffs, 'output/temp/ranef_coeff_summary_temp.rds')
  
}

# Summarize coefficients from population models
for(mod in c('gfr', 'ranef')) {
  mod_summary <- get(paste0('all_', mod, '_coeffs')) %>% 
    # Convert standard deviation of random effect to variance
    mutate(estimate = ifelse(effect == 'ran_pars', estimate^2, estimate)) %>%
    group_by(elkyear, block, term) %>%
    # Calculate mean and 95% CI for estimates
    summarize(mean_ests = round(mean(estimate), digits = 2),
              lower = round(mean(estimate) - (1.96*sd(estimate)/sqrt(200)), 
                            digits = 2),
              upper = round(mean(estimate) + (1.96*sd(estimate)/sqrt(200)), 
                            digits = 2))
    
    # Arrange mean and CI for presenting in table
    table_summary <- mod_summary %>%
      mutate(mean_ests = paste0(mean_ests, ' (', lower, ', ', upper, ')')) %>%
      select(! c(lower, upper)) %>%
      pivot_wider(names_from = term, values_from = mean_ests)
  
  # Assign
  assign(paste0(mod, '_table_summary'), table_summary)
  assign(paste0(mod, '_plot_summary'), mod_summary)
  
}

# Compile coefficients from individual test models
# Initiate data frame
indiv_plot_summary <- data.frame()
indiv_table_summary <- data.frame()
# Loop through individuals
for(i in list.files('output/individual_models/', pattern = 'test')) {
  # Extract elkyear
  sub_ey <- str_split(i, '_')[[1]][2]
  sub_ey <- as.numeric(str_extract(sub_ey, '[[:digit:]]+'))
  
  # Get model
  indiv_coeffs <- readRDS(paste0('output/individual_models/', i)) %>%
    #Tidy
    broom.mixed::tidy() %>%
    # Remove unnecessary terms/cols
    filter(! term == '(Intercept)') %>%
    select(! c(statistic, p.value)) %>%
    # Pivot terms into columns
    pivot_wider(names_from = term, values_from = c(estimate, std.error)) %>%
    rename('mixedwood' = estimate_mixedwood, 
           'roaddist' = `estimate_as.vector(roaddist)`) %>%
    mutate(lower_mixedwood = mixedwood - `std.error_mixedwood`*1.96,
           upper_mixedwood = mixedwood + `std.error_mixedwood`*1.96,
           lower_roaddist = roaddist - `std.error_as.vector(roaddist)`*1.96,
           upper_roaddist = roaddist + `std.error_as.vector(roaddist)`*1.96,
           # Add elkyear column
           elkyear = sub_ey) %>%
    # Remove extra columns
    select(! c(`std.error_as.vector(roaddist)`, `std.error_mixedwood`))
    
  # Rename variables and add 95% CIs
  table_coeffs <- indiv_coeffs %>%
  mutate(mixedwood = paste0(round(mixedwood, digits = 2), ' (',
                            round(lower_mixedwood, digits = 2), ', ',
                            round(upper_mixedwood, digits = 2), ')'),
         roaddist = paste0(round(roaddist, digits = 2), ' (',
                           round(lower_roaddist, digits = 2), ', ',
                           round(upper_roaddist, digits = 2), ')')) %>%
    # Remove extra cols
    select(c(elkyear, mixedwood, roaddist))
  
  # Bind to data frame
  indiv_plot_summary <- rbind(indiv_plot_summary, indiv_coeffs)
  indiv_table_summary <- rbind(indiv_table_summary, table_coeffs)
}

# Save summaries
saveRDS(ranef_table_summary, 'output/ranef_coeff_summary.rds')
saveRDS(gfr_table_summary, 'output/gfr_coeff_summary.rds')
saveRDS(indiv_table_summary, 'output/indiv_test_summary.rds')
# For plotting
saveRDS(ranef_plot_summary, 'output/ranef_coeff_plotting.rds')
saveRDS(gfr_plot_summary, 'output/gfr_coeff_plotting.rds')
saveRDS(indiv_plot_summary, 'output/indiv_coeff_plotting.rds')

# Save csvs
write.csv(ranef_table_summary, 'output/ranef_coeff_summary.csv')
write.csv(gfr_table_summary, 'output/gfr_coeff_summary.csv')
write.csv(indiv_table_summary, 'output/indiv_test_summary.csv')

