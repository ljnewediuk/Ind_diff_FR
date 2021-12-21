
library(lme4)
library(glmmTMB)
library(tidyverse)

# Load RSS summary
rss_summ <- readRDS('output/rss_summary.rds') %>%
  mutate(cond = recode(cond, 
                       'Mixed_forest' = '(b) Mixed forest', 
                       'Road' = '(a) Distance to road (km)'))

# Summarize stats to display on plots
stats_summ <- data.frame()

for(habitat in c('(a) Distance to road (km)', '(b) Mixed forest')) {

  # p-value of Z-scores by model (boxplots)
  bplot_pval <- round(broom::tidy(lm(zscore ~ model, 
                               data = rss_summ[rss_summ$cond == habitat ,], 
                        weights = 1/var))[2 ,]$p.value, digits = 3)
  # Boxplot model summary
  assign(paste('bplot', habitat, sep = '_'), lm(zscore ~ model, 
                   data = rss_summ[rss_summ$cond == habitat ,], weights = 1/var))
  
  # R-squared of Z-scores with habitat and model
  for(model in c('gfr', 'ranef')) {
    # Fit a weighted linear regression to test whether Z-score depends on individual
    # variation in selection or availability between blocks
    assign(paste(model, habitat, 'lm', sep = '_'), 
           lm(abs(zscore) ~ abs(beta_diff) + abs(hr_diff), 
                            data =  rss_summ[rss_summ$model == model & 
                                               rss_summ$cond == habitat ,], 
                            weights = 1/var))
    # R-squared Z-scores with individual selection difference
    rsq_beta_diff <- round(summary(lm(zscore ~ beta_diff, 
                                data =  rss_summ[rss_summ$model == model & 
                                                   rss_summ$cond == habitat ,], 
                      weights = 1/var))$r.squared, digits = 3)
    # R-squared Z-scores with difference in HR cover
    rsq_hr_diff <- round(summary(lm(zscore ~ hr_diff, 
                              data = rss_summ[rss_summ$model == model & 
                                                rss_summ$cond == habitat ,], 
                              weights = 1/var))$r.squared,  digits = 3)
    
    # Bind together into row
    stats_row <- data.frame(cond = habitat, model, 
                            bplot_pval = paste0('p = ', bplot_pval), 
                            rsq_beta_diff = paste0('R^2', '==', rsq_beta_diff), 
                            rsq_hr_diff = paste0('R^2', '==', rsq_hr_diff),
                            y_hr = max(abs(rss_summ[rss_summ$cond == habitat ,]$zscore)),
                            y_beta = max((rss_summ[rss_summ$cond == habitat ,]$zscore)),
                            x_hr = mean(abs(rss_summ[rss_summ$cond == habitat ,]$hr_diff)),
                            x_beta = mean(rss_summ[rss_summ$cond == habitat ,]$beta_diff))
    
    stats_summ <- rbind(stats_summ, stats_row)
    
  }
  
}

# Plot boxplots for each covariate
# tiff("figures/zsc_boxplot.tiff", width = 8, height = 6, units = 'in', res = 300)
ggplot() +
  geom_hline(yintercept = 0, linetype = 'dashed', colour = '#00000030') +
  geom_boxplot(data = rss_summ,
               aes(x = model, y = zscore,
                   colour = model, fill = model), notch = T, width = 0.3) +
  geom_text(data = stats_summ[stats_summ$model == 'gfr' ,], 
            aes(x = model, y = 2.8, label = bplot_pval), size = 6) +
  scale_fill_manual(values = c('#ffad6030', '#4a4a8830')) +
  scale_colour_manual(values = c('#ffad60', '#4a4a88')) +
  theme(panel.background = element_rect(fill = 'white'),
        plot.margin = unit(c(0.5, 0.5, 1, 1), 'cm'),
        panel.grid = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_text(size = 18, colour = '#000000', hjust = 0),
        axis.text = element_text(size = 15, colour = '#000000'),
        axis.title.y = element_text(size = 18, colour = '#000000', vjust = 4),
        axis.title.x = element_blank(),
        axis.line.y = element_line(),
        axis.line.x = element_line(),
        legend.position = 'none') +
  annotate("segment", x=0, xend=0, y=-Inf, yend=Inf, colour ='#000000', size = 1) +
  ylab('Z-score') + 
  facet_wrap(~ cond, scales = 'fixed') +
  # Set ylims to remove outliers
  ylim(-5, 4)

dev.off()
  
# tiff("figures/zsc_by_sel.tiff", width = 8, height = 6, units = 'in', res = 300)
ggplot(data = rss_summ,
       aes(x = beta_diff, y = zscore, group = model, colour = model)) +
  scale_color_manual(values = c('#ffad60', '#4a4a88')) +
  geom_hline(yintercept = 0, linetype = 'dashed', colour = '#00000030') +
  geom_vline(xintercept = 0, linetype = 'dashed', colour = '#00000030') +
  geom_point() +
  geom_text(data = stats_summ[stats_summ$model == 'ranef' ,], 
            aes(x = x_beta, y = y_beta, label = rsq_beta_diff), size = 6, parse = T) +
  geom_text(data = stats_summ[stats_summ$model == 'gfr' ,], 
            aes(x = x_beta, y = y_beta*.7, label = rsq_beta_diff), size = 6, parse = T) +
  geom_smooth(method = 'lm', fullrange = T) +
  theme(panel.background = element_rect(fill = 'white'),
        plot.margin = unit(c(0.5, 0.5, 1, 1), 'cm'),
        panel.grid = element_blank(),
        axis.text = element_text(size = 15, colour = '#000000'),
        axis.title.y = element_text(size = 18, colour = '#000000', vjust = 4),
        axis.title.x = element_text(size = 18, colour = '#000000', vjust = -4),
        axis.line.y = element_line(),
        axis.line.x = element_line(),
        legend.position = 'none',
        strip.background = element_blank(),
        strip.text.x = element_text(size = 15, colour = '#000000', hjust = 0)) +
  ylab('Population model z-score') + 
  xlab('Individual selection difference') + 
  facet_wrap(~ cond, scales = 'free')

dev.off()

# Recode habitat names for plotting z-score by home range difference
rss_summ <- rss_summ %>%
  mutate(cond = recode(cond, 
                       'Mixed forest' = 'Proportion mixed forest', 
                       'Distance to road (km)' = 'Mean distance to road (km)'))
stats_summ <- stats_summ %>%
  mutate(cond = recode(cond, 
                       'Mixed forest' = 'Proportion mixed forest', 
                       'Distance to road (km)' = 'Mean distance to road (km)'))

# tiff("figures/zsc_by_hr.tiff", width = 8, height = 6, units = 'in', res = 300)
ggplot(data = rss_summ,
       aes(x = abs(hr_diff), y = abs(zscore), group = model, colour = model)) +
  scale_color_manual(values = c('#ffad60', '#4a4a88')) +
  geom_hline(yintercept = 0, linetype = 'dashed', colour = '#00000030') +
  geom_point() +
  geom_text(data = stats_summ[stats_summ$model == 'ranef' ,], 
            aes(x = x_hr, y = y_hr, label = rsq_hr_diff), size = 6, parse = T) +
  geom_text(data = stats_summ[stats_summ$model == 'gfr' ,], 
            aes(x = x_hr, y = y_hr*.9, label = rsq_hr_diff), size = 6, parse = T) +
  geom_smooth(method = 'glm', 
              method.args=list(family=gaussian(link="log")),
              fullrange = T) +
  theme(panel.background = element_rect(fill = 'white'),
        plot.margin = unit(c(0.5, 0.5, 1, 1), 'cm'),
        panel.grid = element_blank(),
        axis.text = element_text(size = 15, colour = '#000000'),
        axis.title.y = element_text(size = 18, colour = '#000000', vjust = 4),
        axis.title.x = element_text(size = 18, colour = '#000000', vjust = -4),
        axis.line.y = element_line(),
        axis.line.x = element_line(),
        legend.position = 'none',
        strip.background = element_blank(),
        strip.text.x = element_text(size = 15, colour = '#000000', hjust = 0)) +
  ylab('|Population model z-score|') + 
  xlab('|Individual difference between HRs|') + 
  facet_wrap(~ cond, scales = 'free')

dev.off()  

