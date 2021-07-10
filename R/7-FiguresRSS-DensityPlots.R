
library(lme4)
library(glmmTMB)
library(tidyverse)

# Load RSS results
rss_dat <- readRDS('output/rss_results_predict.rds')

# Plot density distributions for each covariate
for(habitat in c('Road', 'Mixed_forest')) {
  
  # Get corresponding condition (use one comparison for each covariate)
  cond_type <- ifelse(habitat == 'Road', 'x1-mw0-rd0.05_x2-mw0-rd0.50',
                      'x1-mw1-rd0.95_x2-mw0-rd0.95')
  
  # Filter RSS data to habitat
  rss_dat_sub <- rss_dat %>%
    filter(cond == cond_type)
  # Filter models into separate dataframes
  rss_gfr <- rss_dat_sub %>%
    filter(model == 'gfr')
  rss_ranef <- rss_dat_sub %>%
    filter(model == 'ranef')
  rss_test <- rss_dat_sub %>%
    filter(model == 'test')
  rss_train <- rss_dat_sub %>%
    filter(model == 'train')
  
  # Plot
  ggplot() +
    geom_density(data = rss_gfr, aes(x = rss), 
                 colour = '#ffad60', fill = '#ffad6030') +
    geom_density(data = rss_ranef, aes(x = rss), 
                 colour = '#4a4a88', fill = '#4a4a8830') +
    geom_vline(data = rss_test, aes(xintercept = rss), linetype = 'dashed') +
    facet_wrap(~ elkyear) +
    theme(panel.background = element_rect(fill = 'white', colour = 'black'),
          plot.margin = unit(c(0.5, 0.5, 1, 1), 'cm'),
          panel.grid = element_blank(),
          axis.text = element_text(size = 15, colour = '#000000'),
          axis.title.y = element_text(size = 18, colour = '#000000', vjust = 4),
          axis.title.x = element_text(size = 18, colour = '#000000', vjust = -4),
          strip.background = element_blank(),
          strip.text = element_text(size = 15, colour = '#000000')) +
    ylab('Density') + 
    xlab('Relative selection strength')

  # Save plot
  ggsave(filename = paste0(habitat, '_rss_dens.tiff'), 
         path = 'figures/', plot = last_plot(), 
         width = 12, height = 10, device = 'tiff', units = 'in', dpi = 300)
  
}
