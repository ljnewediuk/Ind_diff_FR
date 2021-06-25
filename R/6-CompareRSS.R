
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

ggplot() +
  geom_density(data = rss_gfr, aes(x = rss),
               colour = '#ffad6060', fill = '#ffad6030') +
  geom_density(data = rss_ranef, aes(x = rss),
               colour = '#4a4a8860', fill = '#4a4a8830') +
  geom_vline(data = rss_test, aes(xintercept = rss)) +
  facet_wrap(~ elkyear) +
  theme(panel.background = element_rect(fill = 'white', colour = 'black'),
        plot.margin = unit(c(0.5, 0.5, 1, 1), 'cm'),
        panel.grid = element_blank(),
        axis.text = element_text(size = 15, colour = '#000000'),
        axis.title.y = element_text(size = 18, colour = '#000000', vjust = 4),
        axis.title.x = element_text(size = 18, colour = '#000000', vjust = -4)) +
  ylab('Density') + 
  xlab('Relative selection strength')




