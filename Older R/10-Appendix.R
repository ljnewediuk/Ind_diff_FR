############################
############################
## 10 - APPENDIX FIGURES ##
###########################
###########################

### Packages ----
libs <- c('data.table', 'ggplot2', 'cowplot', 'grid', 'gridExtra', 'dplyr')
lapply(libs, require, character.only = TRUE)

### Load data ----

# A) Elk data
dat <- readRDS('input/elkdatbyEYs.rds')

# Add elkyear column
dat[, elkyear := .GRP,
    by = c('EarTag', 'intyear')]

# Screen out elkyears with fewer than 150 points
fewEY <- vector()
for(i in unique(dat$elkyear)){
  subdat <- dat[elkyear==i]
  Nsub <- subdat[, .N, by='elkyear']
  if(Nsub$N < 150){
    fewEY <- c(fewEY, i)
  }else{
    next
  }
}
# Remove
dat <- dat[!elkyear %in% fewEY]

# Exclude elk from Duck Mountain population in 2006/07
dat <- dat[!c(intyear==2006 | intyear==2007)]

# B) Prediction outputs
correlations <- readRDS('results/prediction_outputs/correlations.rds')

# List of all folders
# folder_list <- list.files(path="results/prediction_outputs/")
# 
# all_correlations <- data.table()
# 
# for (input_folder in folder_list){
#   
#   data <- readRDS(file = paste0("results/prediction_outputs/", paste(input_folder, "correlations.rds", sep = "/")))
#   all_correlations <- rbind(all_correlations, data)
#   
# }

# Subset elkyears in correlations with less than 750 points
# (These individuals have large variation in coefficients)

# Npoints_750 <- Npoints_id[N<=750]
# Npoints_750 <- unlist(Npoints_750$elkyear)
# Npoints_750 <- readRDS('input/exclusion_elk.rds')
# all_correlations <- all_correlations[!elkyear %in% Npoints_750]


# C) Individual model outputs
covariates <- readRDS('results/RSF_outputs/mw_rd_RSFs.rds')

# Remove iterations with less than one week of data during the iteration (84 = 7 d x 12 pts)
covariates <- covariates %>% filter(numb_pts >= 84)
# Use only individual points from OOS samples
covariates <- covariates[type=='OOS']

# Summarize individuals
individual_summ <- covariates %>% dplyr::select(c('elkyear', 'term', 'estimate', 'mean_mixedwood', 'mean_road')) %>% 
  group_by(elkyear, term) %>% summarize(mean_est=mean(estimate, na.rm=T), 
     se_est=sd(estimate, na.rm=T)/sqrt(length(estimate)), 
     mean_mw=mean(mean_mixedwood, na.rm=T), mean_rd=mean(mean_road, na.rm=T),
     se_mw=sd(mean_mixedwood, na.rm=T)/sqrt(length(mean_mixedwood)), se_rd=sd(mean_road, na.rm=T)/sqrt(length(mean_road)))   

individual_summ <- individual_summ %>% mutate(upper_est=mean_est+(se_est*1.96), lower_est=mean_est-(se_est*1.96),
                                    upper_rd=(mean_rd+(se_rd*1.96))/1000, lower_rd=(mean_rd-(se_rd*1.96))/1000,
                                    upper_mw=mean_mw+(se_mw*1.96), lower_mw=mean_mw-(se_mw*1.96),
                                    mean_rd=mean_rd/1000, elkyear=factor(elkyear))

######################################################
### PART 1: DENSITY PLOT (N POINTS PER INDIVIDUAL) ###
######################################################
# Density plot of number of points per individual
# during study period in initial data set.

Npoints_id <- dat[, .N, by='elkyear']

# tiff('figures/suppAX', width = 6, height = 6, units = 'in', res = 300)

ggplot(Npoints_id, aes(x=N)) + geom_density(fill='#FF4449', alpha=0.5) +
  theme(panel.background = element_rect(fill='white'), axis.line = element_line(colour='black'), axis.text = element_text(size=15),
  axis.title.x = element_text(size=18, vjust=-3), axis.title.y = element_text(size=18, vjust=5), plot.margin = unit(c(1,1,1,1),'cm')) +
  xlab('Number location points/individual') + ylab('Density')

##########################################################################
### Supplementary Figure A1: Functional response mixedwood x road ###
##########################################################################
# Plots functional response for mixedwood with increasing distance to 
# road in the home range

# tiff('figures/suppA1.tiff', width = 9.5, height = 9.5, units = 'in', res = 300)

ggplot(individual_summ[individual_summ$term=='mixedwood',], aes(x=mean_rd, y=mean_est)) + 
  geom_smooth(method='lm', colour='black') +
  geom_hline(yintercept=0, size=0.5, colour='black', alpha=0.8) +
  geom_errorbar(aes(ymin=lower_est, ymax=upper_est), colour='black', alpha=0.8, width=0.1) +
  geom_errorbarh(aes(xmin=lower_rd, xmax=upper_rd), colour='black', alpha=0.8, height=0.1) +
  geom_point(alpha=0.8, colour='black', size=1.5) + 
  # Add coloured points for individuals not following the functional response
  geom_point(data=individual_summ[individual_summ$term=='mixedwood' & individual_summ$elkyear %in% c(2,8,10,21,22,34,35,47,55) ,], aes(fill=elkyear), colour='black', pch=21, size=1.5) +
  scale_fill_viridis_d() +
  # geom_text(aes(label=elkyear)) +
  annotate('text', x=1.5, y=-2, label=paste('R^2 ==', round(summary(lm(individual_summ[individual_summ$term=='mixedwood',]$mean_est~individual_summ[individual_summ$term=='mixedwood',]$mean_rd))$adj.r.squared,2)), parse=T, size=5) +
  theme(panel.background = element_rect(fill='white'), axis.line = element_line(colour='black'), axis.text = element_text(size=15),
        axis.title.x = element_text(size=18, vjust=-3), axis.title.y = element_text(size=18, vjust=5), plot.margin = unit(c(1,1,1,1),'cm'),
        strip.text = element_text(size=15), panel.spacing = unit(1,'cm'), legend.position = 'none') +
  ylab('Selection for mixed forest') + xlab('Mean distance to road (km)')

##########################################################################
### Supplementary Figure A2: Functional response road x mixedwood ###
##########################################################################
# Plots functional response for distance to road with increasing cover of 
# mixedwood in the home range

# tiff('figures/suppA2.tiff', width = 9.5, height = 9.5, units = 'in', res = 300)

ggplot(individual_summ[individual_summ$term=='I(log(road + 1))',], aes(x=mean_mw, y=mean_est)) + 
  geom_smooth(method='lm', colour='black') +
  geom_hline(yintercept=0, size=0.5, colour='black', alpha=0.5) +
  geom_errorbar(aes(ymin=lower_est, ymax=upper_est), colour='black', alpha=0.8, width=0.02) +
  geom_errorbarh(aes(xmin=lower_mw, xmax=upper_mw), colour='black', alpha=0.8, height=0.2) +
  geom_point(alpha=0.8, colour='black', size=1.5) + 
  # Add coloured points for individuals not following the functional response
  geom_point(data=individual_summ[individual_summ$term=='I(log(road + 1))' & individual_summ$elkyear %in% c(2,8,10,21,22,34,35,47,55) ,], aes(fill=elkyear), colour='black', pch=21, size=1.5) +
  scale_fill_viridis_d() +
  # geom_text(aes(label=elkyear)) +
  annotate('text', x=0.3, y=-6, label=paste('R^2 ==', round(summary(lm(individual_summ[individual_summ$term=='I(log(road + 1))',]$mean_est~individual_summ[individual_summ$term=='I(log(road + 1))',]$mean_mw))$adj.r.squared,2)), parse=T, size=5) +
  theme(panel.background = element_rect(fill='white'), axis.line = element_line(colour='black'), axis.text = element_text(size=15),
        axis.title.x = element_text(size=18, vjust=-3), axis.title.y = element_text(size=18, vjust=5), plot.margin = unit(c(1,1,1,1),'cm'),
        strip.text = element_text(size=15), panel.spacing = unit(1,'cm'), legend.position = 'none') +
  ylab('Selection for distance to road') + xlab('Mean cover mixed forest')

###################################################################
### Supplementary Figure A3: Change in R2 with number of points ###
###################################################################
# Plots number of testing data points in each iteration against
# correlation coefficient. Found no relationship.

melt_correlations <- melt(correlations[, c(1,4:7)], id.vars=c('elkyear', 'numb_points'), variable.name='model', value.name='correlation', variable.factor=T)

levels(melt_correlations$model) <- c('Individual', 'GFR', 'Ran. Eff.')

# tiff('figures/suppA3.tiff', width = 13, height = 6, units = 'in', res = 300)

ggplot(melt_correlations, aes(x=numb_points, y=correlation)) + geom_point() + facet_wrap(~model) +
  theme(panel.background = element_rect(fill='white'), axis.line = element_line(colour='black'), axis.text = element_text(size=15),
  axis.title.x = element_text(size=18, vjust=-3), axis.title.y = element_text(size=18, vjust=5), plot.margin = unit(c(1,1,1,1),'cm'),
  strip.text = element_text(size=15), panel.spacing = unit(1,'cm')) +
  ylab(expression(paste('Correlation with individual level model (R'^2, ')'))) + xlab('Number location points/iteration')

#####################################################################
### Supplementary Figure A4: Change in Beta with number of points ###
#####################################################################
# Plots total number of points in each elk-year against coefficient
# estimate. Determined that fewer than 750 points produces spurious
# coefficient estimates.

covariates$term <- factor(covariates$term)
levels(covariates$term) <- c('Road distance', 'Mixed forest')

# tiff('figures/suppA4.tiff', width = 9.5, height = 6, units = 'in', res = 300)

ggplot(covariates, aes(x=numb_pts, y=estimate)) + geom_point() + 
  facet_wrap(~term) +
  geom_point(data=covariates[elkyear==55 & dates=='01-02_01-31'], colour='red') +
  geom_vline(data = data.frame(xint=300,term="Road distance"), aes(xintercept = xint), color='red', linetype='dashed') +
  geom_vline(data = data.frame(xint=385,term="Road distance"), aes(xintercept = xint), color='red', linetype='dashed') +
  geom_vline(data = data.frame(xint=300,term="Mixed forest"), aes(xintercept = xint), color='red', linetype='dashed') +
  geom_vline(data = data.frame(xint=385,term="Mixed forest"), aes(xintercept = xint), color='red', linetype='dashed') +
  theme(panel.background = element_rect(fill='white'), axis.line = element_line(colour='black'), axis.text = element_text(size=15, colour='black'),
        axis.title.x = element_text(size=18, vjust=-3, colour='black'), axis.title.y = element_text(size=18, vjust=5, colour='black'), 
        plot.margin = unit(c(1,1,1,1),'cm'), strip.text = element_text(size=15), panel.spacing = unit(1,'cm')) +
        xlab('Number location points/iteration') + ylab('Selection coefficient')

###########################################################
### Supplementary Figure A5: Number of points histogram ###
###########################################################
# Plots histogram of number of points per iteration,
# highlighting the iterations with most common number
# of points.

# tiff('figures/suppA5.tiff', width = 9.5, height = 6, units = 'in', res = 300)

ggplot(covariates, aes(x=numb_pts)) + geom_histogram() +
  theme(panel.background = element_rect(fill='white'), axis.line = element_line(colour='black'), axis.text = element_text(size=15),
        axis.title.x = element_text(size=18, vjust=-3), axis.title.y = element_text(size=18, vjust=5), plot.margin = unit(c(1,1,1,1),'cm')) +
  geom_vline(xintercept = 300, color='red', linetype='dashed') +
  geom_vline(xintercept = 385, color='red', linetype='dashed') +
  xlab('Number location points/iteration') + ylab('N')

#####################################################################
### Supplementary Figure A6: Change in Beta with number of points ###
#####################################################################
# Plots total number of points in each elk-year against standard
# error. Determined that fewer than 150 points produces higher
# standard errors.

# tiff('figures/suppA6.tiff', width = 9.5, height = 6, units = 'in', res = 300)

ggplot(covariates, aes(x=numb_pts, y=std.error)) + geom_point() + 
  facet_wrap(~term) +
  geom_point(data=covariates[elkyear %in% c(22,55) & dates=='01-02_01-31'], colour='red') +
  theme(panel.background = element_rect(fill='white'), axis.line = element_line(colour='black'), axis.text = element_text(size=15, colour='black'),
        axis.title.x = element_text(size=18, vjust=-3, colour='black'), axis.title.y = element_text(size=18, vjust=5, colour='black'), 
        plot.margin = unit(c(1,1,1,1),'cm'), strip.text = element_text(size=15), panel.spacing = unit(1,'cm')) +
  geom_vline(xintercept = 300, color='red', linetype='dashed') +
  geom_vline(xintercept = 385, color='red', linetype='dashed') +
  xlab('Number location points/iteration') + ylab('Standard error')

