
library(tidyverse)

# Load summaries for plotting
indiv_coeffs <- readRDS('output/indiv_coeff_plotting.rds')
gfr_coeffs <- readRDS('output/gfr_coeff_plotting.rds')
ranef_coeffs <- readRDS('output/ranef_coeff_plotting.rds')

# Plot individual coefficients (coloured by covariate)
# tiff("figures/supp_individual_coeffs.tiff", width = 7, height = 6, units = 'in', res = 300)
ggplot() +
  geom_hline(yintercept = 0, linetype = 'dashed') +
  geom_point(data = indiv_coeffs, 
             aes(x = factor(elkyear), y = mixedwood), col = '#541352') +
  geom_point(data = indiv_coeffs, 
             aes(x = factor(elkyear), y = roaddist), col = '#2F9AA0') +
  geom_errorbar(data = indiv_coeffs, 
                aes(x = factor(elkyear), 
                    ymin = lower_mixedwood, ymax = upper_mixedwood), 
                col = '#541352', width = 0.5) +
  geom_errorbar(data = indiv_coeffs, 
                aes(x = factor(elkyear), 
                    ymin = lower_roaddist, ymax = upper_roaddist), 
                col = '#2F9AA0', width = 0.5) +   
  theme(legend.position = 'none',
        panel.background = element_rect(fill = 'white', colour = 'black'),
        panel.grid = element_blank(),
        axis.text = element_text(size = 10),
        axis.title.y = element_text(size = 15, vjust = 5),
        axis.title.x = element_text(size = 15, vjust = -5),
        plot.margin = unit(c(0.5, 0.5, 1, 1), 'cm'),
        strip.background = element_rect(fill = NA), 
        strip.text = element_text(size = 15)) +
  ylab('Coefficient estimate') +
  xlab('Individual')

dev.off()

# Plot GFR coefficients (facets)
# tiff("figures/supp_gfr_coeffs.tiff", width = 13, height = 10, units = 'in', res = 300)
ggplot(gfr_coeffs, aes(x = factor(elkyear), y = mean_ests)) +
  geom_hline(yintercept = 0, linetype = 'dashed') +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper)) +
  theme(legend.position = 'none',
        panel.background = element_rect(fill = 'white', colour = 'black'),
        panel.grid = element_blank(),
        axis.text = element_text(size = 10),
        axis.title.y = element_text(size = 15, vjust = 5),
        axis.title.x = element_text(size = 15, vjust = -5),
        plot.margin = unit(c(0.5, 0.5, 1, 1), 'cm'),
        strip.background = element_rect(fill = NA), 
        strip.text = element_text(size = 15)) +
  facet_wrap(~ term, scales = 'free', ncol = 2) +
  ylab('Coefficient estimate') +
  xlab('Individual')

dev.off()

# Plot random effects coefficients (facets)
# tiff("figures/supp_ranef_coeffs.tiff", width = 12, height = 7, units = 'in', res = 300)
ggplot(ranef_coeffs, aes(x = factor(elkyear), y = mean_ests)) +
  geom_hline(yintercept = 0, linetype = 'dashed') +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper)) +
  theme(legend.position = 'none',
        panel.background = element_rect(fill = 'white', colour = 'black'),
        panel.grid = element_blank(),
        axis.text = element_text(size = 10),
        axis.title.y = element_text(size = 15, vjust = 5),
        axis.title.x = element_text(size = 15, vjust = -5),
        plot.margin = unit(c(0.5, 0.5, 1, 1), 'cm'),
        strip.background = element_rect(fill = NA), 
        strip.text = element_text(size = 15)) +
  facet_wrap(~ term, scales = 'free', nrow = 2) +
  ylab('Coefficient estimate') +
  xlab('Individual')

dev.off()




