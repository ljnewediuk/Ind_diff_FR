#################################################################################
#################################################################################
## 6 - VISUALIZE RELATIONSHIP BETWEEN VARIANCE IN SELECTION AND MODEL PERFORMANCE
#################################################################################
#################################################################################
## Calculate:
##    a) Relative performance of ranef vs. GFR model for each individual
##    b) Variance in selection for mixedwood
## Figure:
##    Plots  relative performance vs. variance
##       

libs <- c('data.table', 'ggplot2', 'cowplot', 'grid', 'gridExtra')
lapply(libs, require, character.only=T)

# Load individual model performance
meanByID <- readRDS('input/mean_correlations.rds')
# Load individual model estimates
indiv_mods <- readRDS('results/RSF_outputs/mw_rd_RSFs.rds')


varByID <- data.table()
for(i in unique(indiv_mods$elkyear)){

  indiv_sub <- indiv_mods[elkyear==i]
  mean_sub <- meanByID[elkyear==i]
  
  diff_mw <- mean_sub[model=='ranef']$mean-mean_sub[model=='gfr']$mean
  var_mw <- var(indiv_sub[term=='mixedwood']$estimate, na.rm=T)
  
  indiv_row <- data.table(elkyear=i, var_estimate=var_mw, difference=diff_mw)
  varByID <- rbind(varByID, indiv_row)

}

# Calculate R2 correlation
variance_r2 <- summary(lm(data=varByID, var_estimate~difference))$r.squared

# tiff('figures/variance-performance.tiff', width = 7, height = 7, units = 'in', res = 300)
ggplot(varByID, aes(x=var_estimate,  y=difference)) +  geom_point(alpha=0.3, size=2) + 
  geom_hline(yintercept=0, colour='black', linetype='dashed') +
  geom_smooth(method='lm', colour='black') +
  annotate('text', x=0.4, y=0.4, label=paste('R^2 ==', round(variance_r2, 2)), parse=T, size=5) +
  theme(panel.background=element_rect(fill='white', colour='black'), panel.grid=element_blank(),
        axis.text=element_text(size=12, colour='black'), axis.title.x=element_text(size=15, colour='black', vjust=-3),
        axis.title.y=element_text(size=15, colour='black', vjust=3), plot.margin=unit(c(.25,.25,1,1), 'cm')) +
  ylab('Relative performance of random effects model') + xlab('Individual variance in selection for mixed forest') + ylim(-.7,.7)
  
  
  
  
  
  
  

