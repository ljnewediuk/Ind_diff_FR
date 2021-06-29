
library(tidyverse)
library(lme4)
library(glmmTMB)

# Load RSS results
rss_dat <- readRDS('output/rss_results.rds')

rss_gfr <- rss_dat %>%
  filter(model == 'gfr')

rss_ranef <- rss_dat %>%
  filter(model == 'ranef')

rss_test <- rss_dat %>%
  filter(model == 'test')

rss_train <- rss_dat %>%
  filter(model == 'train')

rss_zscores <- data.frame()

  for(indiv in unique(rss_dat$elkyear)) {
  
  rss_indiv_test <- rss_dat %>%
    filter(elkyear == indiv & model == 'test')
  rss_indiv_train <- rss_dat %>%
    filter(elkyear == indiv & model == 'train')
  rss_indiv_gfr <- rss_dat %>%
    filter(elkyear == indiv & model == 'gfr')
  rss_indiv_ranef <- rss_dat %>%
    filter(elkyear == indiv & model == 'ranef')
  
  beta_diff <- rss_indiv_test$rss - rss_indiv_train$rss
  
  z_gfr <- rss_indiv_test$rss - mean(rss_indiv_gfr$rss) / sd(rss_indiv_gfr$rss)
  z_ranef <- rss_indiv_test$rss - mean(rss_indiv_ranef$rss) / sd(rss_indiv_ranef$rss)
  
  zscore_row <- data.frame(elkyear = indiv,
                         beta_diff,
                         gfr = z_gfr,
                         ranef = z_ranef)
  
  rss_zscores <- rbind(rss_zscores, zscore_row)
  
  }

rss_summ <- rss_zscores %>%
  pivot_longer(cols = c(gfr, ranef), 
               values_to = 'zscore',
               names_to = 'model')

# tiff("figures/rss_dens.tiff", width = 12, height = 10, units = 'in', res = 300)
ggplot() +
  geom_density(data = rss_gfr, aes(x = rss),
               colour = '#ffad60', fill = '#ffad6030') +
  geom_density(data = rss_ranef, aes(x = rss),
               colour = '#4a4a88', fill = '#4a4a8830') +
  geom_vline(data = rss_test, aes(xintercept = rss)) +
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

# tiff("figures/zsc_boxplot.tiff", width = 6, height = 6, units = 'in', res = 300)
ggplot() +
  geom_boxplot(data = rss_summ,
               aes(x = model, y = abs(zscore),
                   colour = model, fill = model), notch = T, width = 0.3) +
  scale_fill_manual(values = c('#ffad6030', '#4a4a8830')) +
  scale_colour_manual(values = c('#ffad60', '#4a4a88')) +
  theme(panel.background = element_rect(fill = 'white', colour = 'black'),
        plot.margin = unit(c(0.5, 0.5, 1, 1), 'cm'),
        panel.grid = element_blank(),
        axis.text = element_text(size = 15, colour = '#000000'),
        axis.title.y = element_text(size = 18, colour = '#000000', vjust = 4),
        axis.title.x = element_blank(),
        legend.position = 'none') +
  ylab('Absolute value of z-score')
  
# tiff("figures/zsc_by_sel.tiff", width = 8, height = 6, units = 'in', res = 300)
ggplot(data = rss_summ,
       aes(x = beta_diff, y = zscore, group = model, colour = model)) +
  scale_color_manual(values = c('#ffad60', '#4a4a88')) +
  geom_hline(yintercept = 0, linetype = 'dashed', colour = '#00000030') +
  geom_vline(xintercept = 0, linetype = 'dashed', colour = '#00000030') +
  geom_point() +
  geom_smooth(method = 'lm', se = F) +
  theme(panel.background = element_rect(fill = 'white', colour = 'black'),
        plot.margin = unit(c(0.5, 0.5, 1, 1), 'cm'),
        panel.grid = element_blank(),
        axis.text = element_text(size = 15, colour = '#000000'),
        axis.title.y = element_text(size = 18, colour = '#000000', vjust = 4),
        axis.title.x = element_text(size = 18, colour = '#000000', vjust = -4),
        legend.position = 'none',
        strip.background = element_blank(),
        strip.text = element_text(size = 15, colour = '#000000')) +
  ylab('Population model z-score') + 
  xlab('Individual selection difference') +
  facet_wrap(~ model)
  
  
  

