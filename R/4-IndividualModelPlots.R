
library(tidyverse)
library(lme4)
library(glmmTMB)

# Load rsf data
hr_covs <- readRDS('output/rsf_data.rds') %>%
  # Summarize home range covers by individual
  group_by(elkyear, block, elkblock) %>%
  summarize(roaddist_hr = mean(roaddist_hr)/1000,
            mixedwood_hr = mean(mixedwood_hr)) %>%
  # Rename road distance to reflect new km units
  rename('roaddist_km_hr' =  roaddist_hr)

# Load individual models and bind outputs
indiv_mods <- data.frame()
# Start loop
for(mods in list.files('output/individual_models/')){
  
  # Load model
  sub_mod <- readRDS(paste0('output/individual_models/', mods))
  
  # Extract model elkyear and block number for matching to HR data
  # Elkyear
  sub_ey <- str_split(mods, '_')[[1]][2]
  sub_ey <- as.numeric(str_extract(sub_ey, '[[:digit:]]+'))
  # Block
  sub_block <- str_split(mods, '_')[[1]][3]
  sub_block <- as.numeric(str_extract(sub_block, '[[:digit:]]+'))
  
  # Tidy
  tidy_mod <- broom::tidy(sub_mod) %>%
    # Remove intercept term
    filter(term != '(Intercept)') %>%
    select(! c(statistic, p.value)) %>%
    # Pivot coefficients to separate columns
    pivot_wider(names_from = term, values_from = c(estimate, std.error)) %>% 
    # Rename road coefficient
    rename('roaddist_b' = `estimate_as.vector(roaddist)`,
           'mixedwood_b' = estimate_mixedwood,
           'roaddist_se' = `std.error_as.vector(roaddist)`,
           'mixedwood_se' = `std.error_mixedwood`) %>%
    # Add columns for elkyear and block
    mutate(elkyear = sub_ey,
           block = sub_block)
  
  # Bind together
  indiv_mods <- rbind(indiv_mods, tidy_mod)
  
}

# Join model coefficients and HR cover
FR_dat <- left_join(indiv_mods, hr_covs)

# Plot ß mixedwood by mean HR road distance
ggplot() +
  geom_hline(yintercept = 0, linetype = 'dashed', colour = '#000000') +
  geom_point(data = FR_dat, 
             aes(x = roaddist_km_hr, y = mixedwood_b), 
             col = '#46454a', alpha = 0.6) +
  geom_errorbar(data = FR_dat,
                aes(x = roaddist_km_hr,
                    ymin = mixedwood_b - mixedwood_se,
                    ymax = mixedwood_b + mixedwood_se),
                width = 0.2, col = '#46454a', alpha = 0.6) +
  geom_smooth(data = FR_dat,
              aes(x = roaddist_km_hr, y = mixedwood_b),
              col = '#46454a', method = 'lm') +
  theme(panel.background = element_rect(fill = 'white', colour = 'black'),
        plot.margin = unit(c(0.5, 0.5, 1, 1), 'cm'),
        panel.grid = element_line(colour = '#00000020'),
        axis.text = element_text(size = 18, colour = '#000000'),
        axis.title.y = element_text(size = 18, colour = '#000000', vjust = 4),
        axis.title.x = element_text(size = 18, colour = '#000000', vjust = -4)) +
  ylab('ß coefficient mixedwood forest') + 
  xlab('Mean distance to road in home range')

# Plot ß road distance by mean HR mixedwood proportion
ggplot() +
  geom_hline(yintercept = 0, linetype = 'dashed', colour = '#000000') +
  geom_point(data = FR_dat, 
             aes(x = mixedwood_hr, y = roaddist_b), 
             col = '#46454a', alpha = 0.3) +
  geom_errorbar(data = FR_dat,
                aes(x = mixedwood_hr,
                    ymin = roaddist_b - roaddist_se,
                    ymax = roaddist_b + roaddist_se),
                width = 0.02, col = '#46454a', alpha = 0.3) +
  geom_smooth(data = FR_dat, 
              aes(x = mixedwood_hr, y = roaddist_b), 
              col = '#46454a', method = 'lm') +
  theme(panel.background = element_rect(fill = 'white', colour = 'black'),
        plot.margin = unit(c(0.5, 0.5, 1, 1), 'cm'),
        panel.grid = element_line(colour = '#00000020'),
        axis.text = element_text(size = 18, colour = '#000000'),
        axis.title.y = element_text(size = 18, colour = '#000000', vjust = 4),
        axis.title.x = element_text(size = 18, colour = '#000000', vjust = -4)) +
  ylab('ß coefficient distance to road') + 
  xlab('Proportional cover mixedwood forest in home range')


