
library(tidyverse)
library(lme4)
library(glmmTMB)
library(cowplot)

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

# Calculate r-squared for plots
r_squareds <- data.frame()
# Loop through blocks
for(i in 1:4) {
  # Subset block
  sub_block <- FR_dat %>%
    filter(block == i)
  # Calculate R-squared for mixedwood-distance to road FR in block
  rsq_mw_rd <- format(round(summary(lm(sub_block$mixedwood_b ~ 
                                  sub_block$roaddist_km_hr))$r.squared, 
                            digits = 2), nsmall = 2)
  # Calculate R-squared for distance to road-mixedwood FR in block
  rsq_rd_mw <- format(round(summary(lm(sub_block$roaddist_b ~ 
                                  sub_block$mixedwood_hr))$r.squared, 
                     digits = 2), nsmall = 2)
  # Bind together
  block_rsquareds <- data.frame(block = i, rsq_mw_rd, rsq_rd_mw)
  r_squareds <- rbind(r_squareds, block_rsquareds)
  
}

# Plot ß mixedwood by mean HR road distance
mw_rd_plot <- ggplot() +
  scale_colour_viridis_d() +
  geom_hline(yintercept = 0, linetype = 'dashed') +
  geom_text(data = r_squareds, 
            aes(label = paste0('Block', block, 'R^2 == ', rsq_mw_rd), 
                x = 6, y = c(1.3, 1.6, 1.9, 2.2), 
                colour = factor(block)),
            hjust = 0, size = 8, parse = T) +
  geom_errorbar(data = FR_dat,
                aes(x = roaddist_km_hr,
                    ymin = mixedwood_b - mixedwood_se,
                    ymax = mixedwood_b + mixedwood_se),
                width = 0.2, col = '#46454a', alpha = 0.3) +
  geom_point(data = FR_dat, size = 3,
             aes(x = roaddist_km_hr, y = mixedwood_b, 
                 colour = factor(block), group = factor(block))) +
  geom_smooth(data = FR_dat,
              aes(x = roaddist_km_hr, y = mixedwood_b, 
                  colour = factor(block), group = factor(block)), 
              method = 'lm', se = F) +
  theme(panel.background = element_rect(fill = 'white'),
        plot.margin = unit(c(0.5, 3, 1, 1), 'cm'),
        plot.title = element_text(size = 23, colour = '#000000', hjust = 0.05),
        panel.grid = element_blank(),
        axis.line.x = element_line(),
        axis.line.y = element_line(),
        axis.text = element_text(size = 23, colour = '#000000'),
        axis.title.y = element_text(size = 23, colour = '#000000', vjust = 4),
        axis.title.x = element_text(size = 23, colour = '#000000', vjust = -4),
        legend.position = 'none') +
  ylab('ß coefficient mixedwood forest') + 
  xlab('Mean distance to road in home range') +
  ggtitle('(b)')

# Plot ß road distance by mean HR mixedwood proportion
rd_mw_plot <- ggplot() +
  scale_colour_viridis_d() +
  geom_hline(yintercept = 0, linetype = 'dashed', colour = '#000000') +
  geom_text(data = r_squareds, 
            aes(label = paste0('Block', block, 'R^2 == ', rsq_rd_mw), 
                x = 0.5, y = c(4.5, 6, 7.5, 9), 
                colour = factor(block)),
            hjust = 0, size = 8, parse = T) +
  geom_errorbar(data = FR_dat,
                aes(x = mixedwood_hr,
                    ymin = roaddist_b - roaddist_se,
                    ymax = roaddist_b + roaddist_se),
                width = 0.02, col = '#46454a', alpha = 0.3) +
  geom_point(data = FR_dat, size = 3,
             aes(x = mixedwood_hr, y = roaddist_b, 
                 colour = factor(block), group = factor(block))) +
  geom_smooth(data = FR_dat, 
              aes(x = mixedwood_hr, y = roaddist_b, 
                  colour = factor(block), group = factor(block)), 
              method = 'lm', se = F) +
  theme(panel.background = element_rect(fill = 'white'),
        plot.margin = unit(c(0.5, 3, 1, 1), 'cm'),
        plot.title = element_text(size = 23, colour = '#000000', hjust = 0.05),
        panel.grid = element_blank(),
        axis.line.x = element_line(),
        axis.line.y = element_line(),
        axis.text = element_text(size = 23, colour = '#000000'),
        axis.title.y = element_text(size = 23, colour = '#000000', vjust = 4),
        axis.title.x = element_text(size = 23, colour = '#000000', vjust = -4),
        legend.position = 'none') +
  ylab('ß coefficient distance to road') + 
  xlab('Proportional cover mixedwood forest in home range') +
  ggtitle('(a)')

# Plot panel figure
# tiff('figures/individual_FRs.tiff', width = 15, height = 8, units = 'in', res = 300)
cowplot::plot_grid(rd_mw_plot, mw_rd_plot, ncol = 2, nrow = 1)

dev.off()
