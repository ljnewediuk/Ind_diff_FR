############################################################################################
############################################################################################
## 3 - BOXPLOT FIGURES COMPARING R-SQUARED VALUES
############################################################################################
############################################################################################
## Returns:
## Boxplot showing distributions of r-squareds for all models and 
## individual r-squareds for the best and worst models of each individual
## based on the mean r-squared

### Packages ----
libs <- c('data.table', 'ggplot2', 'cowplot', 'dplyr', 'DescTools')
lapply(libs, require, character.only = TRUE)

### Load elkyears with fewer than 750 points ----
Npoints_750<- readRDS('input/exclusion_elk.rds')

### Load data from RSF outputs folder ----

# List of all folders
folder_list <- list.files(path="results/prediction_outputs/")

all_correlations <- data.table()

for (input_folder in folder_list){
  
  data <- readRDS(file = paste0("results/prediction_outputs/", paste(input_folder, "correlations.rds", sep = "/")))
  all_correlations <- rbind(all_correlations, data)
  
}

# Optionally exclude elk with fewer than 750 points
# all_correlations <- all_correlations[!elkyear %in% Npoints_750]
# Remove any indiv_mod iteration with fewer than 150 points
all_correlations <- all_correlations[npoints_OOS>=150]

# Calculate difference in all combinations of model comparisons for each iteration
# Individual & ranef model
MedianCI(all_correlations$individual-all_correlations$ranef, conf.level=0.95)
# Individual & GFR model
MedianCI(all_correlations$individual-all_correlations$gfr, conf.level=0.95)
# Individual & No FR
MedianCI(all_correlations$individual-all_correlations$no_FR, conf.level=0.95)

# Calculate mean correlations for all individuals
meanByID <- data.table()
for (i in unique(all_correlations$elkyear)){
  sub_corrs <- subset(all_correlations, elkyear==i)
  
  for(model in c('no_FR', 'ranef', 'gfr')){
    sub_model <- unlist(sub_corrs %>% select(model))
    if(length(sub_model)<6){
      next
    }
    assign(paste(model, 'mean', sep='_'), signif(mean(sub_model),3))
    assign(paste(model, 'lower', sep='_'), signif(as.numeric(MeanCI(sub_model, conf.level=0.95)[2]),3))
    assign(paste(model, 'upper', sep='_'), signif(as.numeric(MeanCI(sub_model, conf.level=0.95)[3]),3))
  }
  
  summaries <- data.table(elkyear=rep(unlist(i),3), model=c('no_FR', 'ranef', 'gfr'), mean=c(no_FR_mean, ranef_mean, gfr_mean),
                          lower=c(no_FR_lower, ranef_lower, gfr_lower), upper=c(no_FR_upper, ranef_upper, gfr_upper))
  meanByID <- rbind(meanByID, summaries)
}

# Pick the best and worst model for each individual
bestmodelscores <- data.table()
worstmodelscores <- data.table()
for (i in unique(meanByID$elkyear)){
  indiv_sub <- meanByID[elkyear==i]
  best_mod <- indiv_sub[mean==max(indiv_sub$mean)]
  worst_mod <- indiv_sub[mean==min(indiv_sub$mean)]
  best <- data.table(elkyear=unlist(best_mod$elkyear), best=unlist(best_mod$model), best_score=unlist(best_mod$mean), lower=unlist(best_mod$lower), upper=unlist(best_mod$upper))
  worst <- data.table(elkyear=unlist(worst_mod$elkyear), worst=unlist(worst_mod$model), worst_score=unlist(worst_mod$mean), lower=unlist(worst_mod$lower), upper=unlist(worst_mod$upper))
  bestmodelscores <- rbind(bestmodelscores, best)
  worstmodelscores <- rbind(worstmodelscores, worst)
}

melt_correlations <- melt(all_correlations, measure.vars=c('individual', 'no_FR', 'ranef', 'gfr'))

# tiff('figures/fig2.tiff', width = 7, height = 6, units = 'in', res = 300)

ggplot(melt_correlations, aes(x=variable, y=value, col=variable)) + 
  # Add zero line at median of individual level model
  geom_hline(yintercept=median(melt_correlations[variable=='individual']$value), col='#000000', size=0.5, linetype='solid', alpha=0.5) +
  # Add boxplots
  geom_boxplot(notch=TRUE, aes(fill=variable), width=0.5, colour='black', alpha=0.5) + 
  # Set boxpot colours
  scale_fill_manual(values=c("#65ABF3", "#DED9DC", "#DED9DC", "#DED9DC")) + 
  scale_x_discrete(labels=c('individual' = 'Individual', 'no_FR' = 'Base',
                            'ranef' = 'Ran. Eff.', 'gfr' = 'GFR')) +
  # Theme
  theme(panel.background = element_rect(fill='white'), panel.border = element_rect(colour='black', fill=NA), 
        legend.position='none', axis.text = element_text(size=12, colour='black'), axis.title.y = element_text(size=15, colour='black', vjust=2.5)) +
  ylab(expression(paste('Correlation with individual level model (R'^2, ')'))) + xlab('') +
  # Optionally add points to show worst/best models
  geom_point(data=worstmodelscores, aes(x=worst, y=worst_score), col='black', fill='white', position=position_jitter(width=.1), pch=21, alpha=0.5) +
  geom_point(data=bestmodelscores, aes(x=best, y=best_score), col='black', fill='black', position=position_jitter(width=.1), pch=24, alpha=0.5)
 



