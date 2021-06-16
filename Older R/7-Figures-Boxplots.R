#####################################################
#####################################################
## 7 - BOXPLOT FIGURES COMPARING R-SQUARED VALUES ##
####################################################
####################################################
## Returns:
## Boxplot showing distributions of r-squareds for all models and 
## individual r-squareds for the best and worst models of each individual
## based on the mean r-squared

### Packages ----
libs <- c('data.table', 'ggplot2', 'cowplot', 'dplyr', 'DescTools')
lapply(libs, require, character.only = TRUE)

### Load data from RSF outputs folder ----
all_correlations <- readRDS('results/prediction_outputs/correlations.rds')

# Remove outlier iterations
all_correlations <- all_correlations[!(elkyear==22 & dates=='01-02_01-31' | elkyear==55 & dates=='01-02_01-31')]

# Calculate difference in all combinations of model comparisons for each iteration
# Individual & ranef model
MedianCI(all_correlations$ranef_corr, conf.level=0.95)
# Individual & GFR model
MedianCI(all_correlations$gfr_corr, conf.level=0.95)
# Individual & No FR
MedianCI(all_correlations$WS_corr, conf.level=0.95)

# Calculate mean correlations for all individuals
meanByID <- data.table()
for (i in unique(all_correlations$elkyear)){
  sub_corrs <- subset(all_correlations, elkyear==i)
  
  for(model in c('WS_corr', 'ranef_corr', 'gfr_corr')){
    sub_model <- unlist(sub_corrs %>% dplyr::select(model))
    if(length(sub_model)<6){
      next
    }
    assign(paste(model, 'mean', sep='_'), signif(mean(sub_model),3))
    assign(paste(model, 'lower', sep='_'), signif(as.numeric(MeanCI(sub_model, conf.level=0.95)[2]),3))
    assign(paste(model, 'upper', sep='_'), signif(as.numeric(MeanCI(sub_model, conf.level=0.95)[3]),3))
  }
  
  summaries <- data.table(elkyear=rep(unlist(i),3), model=c('WS_corr', 'ranef_corr', 'gfr_corr'), mean=c(WS_corr_mean, ranef_corr_mean, gfr_corr_mean), 
                          lower=c(WS_corr_lower, ranef_corr_lower, gfr_corr_lower), upper=c(WS_corr_upper, ranef_corr_upper, gfr_corr_upper))
  meanByID <- rbind(meanByID, summaries)
}

# Pick the best and worst model for each individual
bestmodelscores <- data.table()
worstmodelscores <- data.table()
for (i in unique(meanByID$elkyear)){
  indiv_sub <- meanByID[elkyear==i]
  indiv_sub <- indiv_sub[1:3,]
  best_mod <- indiv_sub[mean==max(indiv_sub$mean)]
  worst_mod <- indiv_sub[mean==min(indiv_sub$mean)]
  best <- data.table(elkyear=unlist(best_mod$elkyear), best=unlist(best_mod$model), best_score=unlist(best_mod$mean), lower=unlist(best_mod$lower), upper=unlist(best_mod$upper))
  worst <- data.table(elkyear=unlist(worst_mod$elkyear), worst=unlist(worst_mod$model), worst_score=unlist(worst_mod$mean), lower=unlist(worst_mod$lower), upper=unlist(worst_mod$upper))
  bestmodelscores <- rbind(bestmodelscores, best)
  worstmodelscores <- rbind(worstmodelscores, worst)
}

# Make data.table for supplementary material
supp_correlations <- merge(bestmodelscores[,1:3], worstmodelscores[,1:3], by='elkyear')
supp_correlations <- supp_correlations[order(elkyear)]

# Make data.table for boxplot
melt_correlations <- melt(all_correlations, measure.vars=c('WS_corr', 'ranef_corr', 'gfr_corr'))

# Make data.table for individual correlations
corr_indivs <- melt_correlations %>% group_by(elkyear, variable) %>% 
  summarize(mean=mean(value, na.rm=T), 
  lower=mean(value, na.rm=T)-(sd(value, na.rm=T)/sqrt(length(value))*1.96),
  upper=mean(value, na.rm=T)+(sd(value, na.rm=T)/sqrt(length(value))*1.96)) %>%
  mutate(elkyear=factor(elkyear))

# tiff('figures/fig2.tiff', width = 5, height = 5, units = 'in', res = 300)

ggplot(melt_correlations, aes(x=variable, y=value, col=variable)) + 
  # Add zero line at median of individual level model
  # Add boxplots
  geom_boxplot(notch=TRUE, fill='#C0C0C0', width=0.3, colour='#000000') + 
  # geom_errorbar(data=corr_indivs, aes(x=variable, y=mean, group=elkyear, ymin=lower, ymax=upper), colour='#00000050', width=.5, position=position_dodge(width=0.5)) +
  geom_hline(yintercept=median(melt_correlations[variable=='individual']$value), col='#000000', linetype='dashed') +
  # geom_point(data=corr_indivs, aes(x=variable, y=mean, group=elkyear, fill=elkyear, alpha=elkyear), colour='#000000', position=position_dodge(width=0.5), size=3, pch=21) +
  # geom_point(data=corr_indivs[corr_indivs$elkyear  %in% c(2,8,10,34,35,47),], aes(x=variable, y=mean, group=elkyear, fill=elkyear), colour='black', position=position_dodge(width=0.6), size=2, pch=21) +
  scale_fill_manual(values=c('#440154', '#472D7B', '#3B528B', rep('#C0C0C0',7), '#27AD81', '#5DC863', rep('#C0C0C0', 4), '#AADC32', '#C0C0C0')) +
  scale_alpha_manual(values=c(rep(1,3), rep(.5,7), rep(1,2), rep(.5, 4), 1, .5)) +
  # Set boxpot colours
  scale_x_discrete(labels=c('Individual', 'Ran. Eff.', 'GFR')) +
  # Theme
  theme(panel.background = element_rect(fill='white'), panel.border = element_rect(colour='black', fill=NA), 
        legend.position='none', axis.text = element_text(size=12, colour='black'), axis.title.y = element_text(size=15, colour='black', vjust=2.5)) +
  ylab(expression(paste('Correlation with individual level model (R'^2, ')'))) + xlab('')

# Save mean correlations by elk year
saveRDS(meanByID, 'input/mean_correlations.rds')

# Save table of scores for supplementary material
write.csv(supp_correlations, 'tables/supplementary_model_scores.csv')



