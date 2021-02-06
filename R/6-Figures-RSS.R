#################################################################
#################################################################
## 5 - RSS FIGURES - RECONCILING INDIVIDUALS WITH THE POPULATION
#################################################################
#################################################################
## Returns:
## RSS outputs (selection for hi against gradient of hj):
##    a) Individuals
##    b) Population, including random effects and GFR models
## Figures:
##    a) Four separate panels: one panel each for variable 
##       (road or mixed forest) x model (GFR or random effects)
##    b) Panel plots showing models comparisons for each
##       variable

### Packages ----
libs <- c('data.table', 'ggplot2', 'cowplot', 'grid', 'gridExtra', 'tidyverse')
lapply(libs, require, character.only = TRUE)

population_rss <- readRDS('results/prediction_outputs/population_rss_data.rds')
individual_rss <- readRDS('results/prediction_outputs/rss_data.rds')

# Make unique entry for each iteration-elkyear
individual_rss <- individual_rss %>% mutate(unique_ID = paste(elkyear, iteration, sep='_'))

# Sqrt of number of dates to help with 95% CI calculation
ci_denom <- sqrt(length(unique(population_rss$dates)))

# Pivot to long table with model type as group
population_rss <- melt(population_rss, id.vars = c('dates', 'road_avail', 'mixedwood_avail'), measure.vars = colnames(population_rss)[4:15], variable.name = 'model_type', value.name = 'rss')
# Summarize mean and standard error for each model type and habitat
population_rss <- population_rss %>% group_by(model_type, road_avail, mixedwood_avail) %>% summarise(mean=mean(rss), sd=sd(rss)) %>% mutate(lower=mean-(sd/ci_denom), upper=mean+(sd/ci_denom))
# Add columns for random effects and GFR models and habitat
population_rss <- population_rss %>% mutate(
  hab=ifelse(grepl('mixedwood', model_type), 'mixedwood', 'road'), 
  mod=ifelse(grepl('ranef', model_type), 'ranef', 'gfr') )
# Split based on random effects or gfr model, and condense random effects
gfr_rss <- population_rss[population_rss$mod=='gfr',] %>% select(model_type, road_avail, mixedwood_avail, mean, lower, upper, hab)
ranef_rss <- population_rss[population_rss$mod=='ranef' & population_rss$model_type %in% 
                              c('mixedwood_high_ranef', 'road_high_ranef'),] %>% ungroup(model_type) %>% select(road_avail, mixedwood_avail, mean, lower, upper, hab)

ggplot(gfr_rss[gfr_rss$hab=='road' & gfr_rss$road_avail > 2000,], aes(x=road_avail, y=mean)) +
  geom_hline(yintercept=0, linetype='dashed', alpha=0.5) +
  geom_line(aes(group=model_type)) + geom_ribbon(aes(x=road_avail, ymin=lower, ymax=upper, group=model_type, fill=model_type), alpha=0.4) + 
  geom_line(data=individual_rss[individual_rss$road_avail>2000,], aes(x=road_avail, y=OOS_road, group=unique_ID, colour=mean_mixedwood), alpha=0.1) + scale_colour_viridis_c()

ggplot(ranef_rss[ranef_rss$hab=='road' & ranef_rss$road_avail > 2000,], aes(x=road_avail, y=mean)) +
  geom_hline(yintercept=0, linetype='dashed', alpha=0.5) +
  geom_line() + geom_ribbon(aes(x=road_avail, ymin=lower, ymax=upper), alpha=0.4) 

ggplot(gfr_rss[gfr_rss$hab=='mixedwood',], aes(x=mixedwood_avail, y=mean)) +
  geom_hline(yintercept=0, linetype='dashed', alpha=0.5) +
  geom_line(aes(group=model_type)) + geom_ribbon(aes(x=mixedwood_avail, ymin=lower, ymax=upper, group=model_type, fill=model_type), alpha=0.4) +
  geom_line(data=individual_rss, aes(x=mixedwood_avail, y=OOS_mixedwood, group=unique_ID, colour=mean_road), alpha=0.1) + scale_colour_viridis_c()

ggplot(ranef_rss[ranef_rss$hab=='mixedwood',], aes(x=mixedwood_avail, y=mean)) +
  geom_hline(yintercept=0, linetype='dashed', alpha=0.5) +
  geom_line() + geom_ribbon(aes(x=mixedwood_avail, ymin=lower, ymax=upper), alpha=0.4) +
  geom_line(data=individual_rss, aes(x=mixedwood_avail, y=OOS_mixedwood, group=unique_ID, colour=mean_road), alpha=0.1) + scale_colour_viridis_c()



##############################
### PART 1: SET PARAMETERS ###
##############################

# mixedwood_high_gfr = mean(mixedwood_high_gfr), mixedwood_mid_gfr = mean(mixedwood_mid_gfr), mixedwood_low_gfr = mean(mixedwood_low_gfr),
# mixedwood_high_ranef = mean(mixedwood_high_ranef), mixedwood_mid_ranef = mean(mixedwood_mid_ranef), mixedwood_low_ranef = mean(mixedwood_low_ranef),
# road_high_gfr = mean(road_high_gfr), road_mid_gfr = mean(road_mid_gfr), road_low_gfr = mean(road_low_gfr),
# road_high_ranef = mean(road_high_ranef), road_mid_ranef = mean(road_mid_ranef), road_low_ranef = mean(road_low_ranef),
# 
# mixedwood_high_gfr_upper = sd(mixedwood_high_gfr)/ci_denom, mixedwood_high_gfr_lower = sd(mixedwood_high_gfr)/ci_denom,
# mixedwood_mid_gfr_upper = sd(mixedwood_mid_gfr)/ci_denom, mixedwood_mid_gfr_lower = sd(mixedwood_mid_gfr)/ci_denom,
# mixedwood_low_gfr_upper = sd(mixedwood_low_gfr)/ci_denom, mixedwood_low_gfr_lower = sd(mixedwood_low_gfr)/ci_denom,
# 
# mixedwood_high_ranef_upper = sd(mixedwood_high_ranef)/ci_denom, mixedwood_high_ranef_lower = sd(mixedwood_high_ranef)/ci_denom,
# mixedwood_mid_gfr_upper = sd(mixedwood_mid_gfr)/ci_denom, mixedwood_mid_gfr_lower = sd(mixedwood_mid_gfr)/ci_denom,
# mixedwood_low_gfr_upper = sd(mixedwood_low_gfr)/ci_denom, mixedwood_low_gfr_lower = sd(mixedwood_low_gfr)/ci_denom,

# Make list of 0.1 percentile, .9 percentile, and mean habitat values for each iteration
hab_data <- data.table()
for(row in 1:nrow(pull_list)){
  # Habitat data
  OOS_data <- readRDS(paste('input/RSF_data/', paste(pull_list[row,]$dates, 'OOS_data.rds',  sep='/'), sep=''))
  for(hab in c('mixedwood', 'road')){
    assign(paste(hab, 'max', sep='_'), quantile(OOS_data[,get(hab)], .75))
    assign(paste(hab, 'mean', sep='_'), mean(OOS_data[,get(hab)], na.rm=T))
    assign(paste(hab, 'min', sep='_'), quantile(OOS_data[,get(hab)], .25))
  }
  hab_row <- data.table(elkyear=unique(id_data$elkyear), mixedwood_mean, road_mean, mixedwood_min, road_min, mixedwood_max, road_max)
  hab_data <- rbind(hab_data, hab_row)
}

# Range of road and mixedwood values for x axis
road_avail <- seq(min(hab_data$road_min), max(hab_data$road_max), length.out=100)
mixedwood_avail <- seq(min(hab_data$mixedwood_min), max(hab_data$mixedwood_max), length.out=100)


# mixedwood availability:
# high
mixedwood_high <- mean(hab_data$mixedwood_max)
# intermediate
mixedwood_mid <- mean(hab_data$mixedwood_mean)
# low
mixedwood_low <- mean(hab_data$mixedwood_min)

# road availability:
# high
road_high <- mean(hab_data$road_max)
# intermediate
road_mid <- mean(hab_data$road_mean)
# low
road_low <- mean(hab_data$road_min)

#########################################
### PART 2: COMPLETE RSS CALCULATIONS ###
#########################################

for(hab in c('road', 'mixedwood')){
  
  hab2 <- ifelse(hab=='mixedwood', 'road', 'mixedwood')
  
  for(availability in c('high', 'mid', 'low')){
    
    for(row in 1:nrow(pull_list)){
    
      gfr_mod <- readRDS(paste('results/RSF_models_population', paste(pull_list[row,]$dates, 'gfr_mod.rds', sep='/'), sep='/'))
      ranef_mod <- readRDS(paste('results/RSF_models_population', paste(pull_list[row,]$dates, 'ranef_mod.rds', sep='/'), sep='/'))
    
      x1_dat <- data.table(hab = get(paste(hab, 'avail', sep='_')), 
                               hab2 = get(paste(hab2, availability, sep='_')),
                               mean_hab = get(paste(hab, 'mid', sep='_')),
                               mean_hab2 = get(paste(hab2, availability, sep='_')),
                               weight=1,
                               elkyear=NA)
    
      x2_dat <- data.table(hab = get(paste(hab, 'mid', sep='_')), 
                         hab2 = get(paste(hab2, availability, sep='_')),
                         mean_hab = get(paste(hab, 'mid', sep='_')),
                         mean_hab2 = get(paste(hab2, availability, sep='_')),
                         weight=1,
                         elkyear=NA)
  
      }
  
  }
  
  
  
}













for(hab in c('mixedwood', 'road')){

  # Set conditions for each habitat type
  if(hab=='mixedwood'){
    delta_hi <- delta_hi_mw
    mean_hi <- mean(indiv_mods$mean_mixedwood)
    mean_hj <- mean(indiv_mods$mean_road)
    low_col <- '#F0FFF0'
    high_col <- '#23A625'
    x_title <- bquote("Mixed forest cover at location x"[j])
    leg_title <- 'Mean road distance \n in home range (km)'
    y_title <- 'log RSS for mixed forest'
    ranef_RMSE <- paste('RMSE =', round(median(all_rmse[habitat=='mixedwood']$rmse_ranef, na.rm=T), digits=2))
    gfr_RMSE <- paste('RMSE =', round(median(all_rmse[habitat=='mixedwood']$rmse_gfr, na.=T), digits=2))
    pos_x <- 0.75
    pos_y <- -7.5
    pos_x_lev <- 0
  } else {
    delta_hi <- delta_hi_rd
    mean_hi <- mean(log(indiv_mods$mean_road))
    mean_hj <- mean(indiv_mods$mean_mixedwood)
    low_col <- '#FAF6D2'
    high_col <- '#FC7878'
    x_title <- bquote("Distance to road (km) at location x"[j])
    leg_title <- 'Mean mixed forest \n in home range'
    y_title <- 'log RSS for distance from road'
    ranef_RMSE <- paste('RMSE =', round(median(all_rmse[habitat=='road']$rmse_ranef, na.rm=T), digits=2))
    gfr_RMSE <- paste('RMSE =', round(median(all_rmse[habitat=='road']$rmse_gfr, na.rm=T), digits=2))
    pos_x <- 7.1
    pos_y <- -3
    pos_x_lev <- 3.912023
  }
  
  # Create tables for RSS values
  rss_population <- data.table(delta_hi)
  rss_individual <- data.table()
  
  ###############################
  ### PART 2 - A) INDIVIDUALS ###
  ###############################
  
  for(i in unique(indiv_mods$elkyear)){
    
    # Subset individual models to elk-year
    indiv_sub <- indiv_mods[elkyear==i]
    
    # Set conditions for habitat type
    if(hab=='mixedwood'){
      mean_hab2 <- indiv_sub$mean_road
      mean_hab <- indiv_sub$mean_mixedwood
      hab2 <- 'road'
    } else {
      mean_hab2 <- indiv_sub$mean_mixedwood
      mean_hab <- indiv_sub$mean_road
      hab2 <- 'mixedwood'
    }
    
    # Set parameters
    Bi <- mean(indiv_sub[term==hab]$estimate)
    Bj <- mean(indiv_sub[term==hab2]$estimate)
    if(hab=='mixedwood'){
      hj_mu <- mean(log(mean_hab2))
      hi_mu <- mean(mean_hab)
    } else {
      hi_mu <- mean(log(mean_hab))
      hj_mu <- mean(mean_hab2)
    }

    
    
    # Calculate RSS
    if(hab=='mixedwood'){
      rss <- (hi_mu-delta_hi)*Bi + log(hj_mu^Bj)
    } else {
      rss <- log((hi_mu/(hi_mu-(hi_mu-delta_hi)))^Bi) + hj_mu*Bj
    }
    
    rss_sub <- data.table(elkyear=rep(i, length(delta_hi)), delta_hi=delta_hi, rss=rss, mean=rep(hj_mu, length(Bi)))
    rss_individual <- rbind(rss_individual, rss_sub)
  }
  
  ##############################
  ### PART 2 - B) POPULATION ###
  ##############################
  
  for(mod_type in c('gfr', 'ranef')){
    for(covar_level in c('high', 'mid', 'low')){
      
      # Subset model type
      model_sub <- model_means[model==mod_type]
      
      # Set condtions for habitat type
      if(hab=='mixedwood'){
        v1 <- 'mw'
        v2 <- 'rd'
        hab2 <- 'road'
      } else {
        v1 <- 'rd'
        v2 <- 'mw'
        hab2 <- 'mixedwood'
      }
        
      # Set parameters
      hj <- get(paste(v2, covar_level, sep='_'))
      Bi <- model_sub[covariate==v1]$beta
      Bj <- model_sub[covariate==v2]$beta
      Bi_mu <- model_sub[covariate==paste(v1, 'mean', sep='_')]$beta
      Bj_mu <- model_sub[covariate==paste(v2, 'mean', sep='_')]$beta
      Bij <- model_sub[covariate==paste(v1, v2, sep='_')]$beta
      Bji <- model_sub[covariate==paste(v2, v1, sep='_')]$beta
      Bii <- model_sub[covariate==paste(v1, v1, sep='_')]$beta
      Bjj <- model_sub[covariate==paste(v2, v2, sep='_')]$beta
      
      
      # Set condtions for model type (which equation to use)
      if(mod_type=='gfr' & hab=='mixedwood'){
        rss <- (mean_hi-delta_hi)*(Bi+Bij*hj+Bii*mean_hi) + log(hj^(Bj+Bji*mean_hi+Bjj*hj)) + mean_hi*Bi_mu + hj^Bj_mu
      }
      if(mod_type=='ranef' & hab=='mixedwood'){
        rss <- (mean_hi-delta_hi)*Bi + log(log(mean_hj)^Bj)
      }
      if(mod_type=='gfr' & hab=='road'){
        rss <- log((mean_hi/(mean_hi-(mean_hi-delta_hi)))^(Bi+Bij*hj+Bii*mean_hi)) + hj*(Bj+Bji*mean_hi+Bjj*hj) + mean_hi^Bi_mu + hj*Bj_mu
      }
      if(mod_type=='ranef' & hab=='road'){
        rss <- log((mean_hi/(mean_hi-(mean_hi-delta_hi)))^Bi) + mean_hj*Bj
      }
      # Calculate RSS
      rss <- as.data.table(rss)
      colnames(rss) <- paste(mod_type, covar_level, sep='_')
      rss_population <- cbind(rss_population, rss)
  
    }
        
  }
  
  # Convert mean distance to road to km if hab = mixedwood
  if(hab=='mixedwood'){
    rss_individual$mean <- log(rss_individual$mean)
  }
  
  #########
  rss_hue_subset <- rss_individual[elkyear %in% c(17, 35, 28, 69)]
    
  #################################
  ### PART 3 - CREATE RSS PLOTS ###
  #################################
  
  ### GFR plot ----
  gfr_plot <- ggplot(data=rss_individual, aes(x=delta_hi, y=rss)) + geom_line(aes(group=elkyear, col=mean)) +
    # Add zero line
    geom_hline(yintercept=0, col='#000000', size=0.5, linetype='solid', alpha=0.5) +
    # Create gradient for individual mean home range cover of variable from low to high
    scale_color_gradient(low=low_col, high=high_col, name=leg_title) +
    # Add highlighted individuals
    geom_line(data=rss_hue_subset, aes(x=delta_hi, y=rss, group=elkyear), col='#9820c3', alpha=0.7, size=1) +
    # Add RSS lines
    geom_line(data=rss_population, aes(x=delta_hi, y=gfr_low), linetype='dotted', size=1.25, col='#000000') +
    # geom_line(data=rss_population, aes(x=delta_hi, y=gfr_mid), linetype='dotdash', size=1, col='#000000') +
    geom_line(data=rss_population, aes(x=delta_hi, y=gfr_high), linetype='longdash', size=1.25, col='#000000') +
    # Theme
    theme(panel.background = element_rect(fill='white'), panel.border = element_rect(colour='black', fill=NA), 
          legend.title = element_text(size=15, colour='black'), legend.text= element_text(size=15, colour='black'), 
          axis.text.x = element_text(size=15, colour='black'), axis.title = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(),
          plot.margin=unit(c(.25,.25,1,1), 'cm')) +
    # Annotate the rmse
    annotate('text', x=pos_x, y=pos_y, label=gfr_RMSE, size=5) +
    # Annotate high, medium, and low
    annotate('text', y=(rss_population[delta_hi==min(rss_population$delta_hi)]$gfr_low), x=pos_x_lev, label='L', size=6, fontface='bold') +
    # annotate('text', y=rss_population[delta_hi==min(rss_population$delta_hi)]$gfr_mid, x=pos_x_lev, label='M', size=4.5, fontface='bold') +
    annotate('text', y=(rss_population[delta_hi==min(rss_population$delta_hi)]$gfr_high), x=pos_x_lev, label='H', size=6, fontface='bold')
    
  # extract legend
  grad_legend <- get_legend(gfr_plot)
  
  # replot gfr without legend
  gfr_plot <- gfr_plot + theme(legend.position = 'none')
  
  ### Random effects plot ----
  ranef_plot <- ggplot(data=rss_individual, aes(x=delta_hi, y=rss)) + geom_line(aes(group=elkyear, col=mean)) +
    # Add zero line
    geom_hline(yintercept=0, col='#000000', size=0.5, linetype='solid', alpha=0.5) +
    # Create gradient for individual mean home range cover of variable from low to high
    scale_color_gradient(low=low_col, high=high_col) +
    # Add highlighted individuals
    geom_line(data=rss_hue_subset, aes(x=delta_hi, y=rss, group=elkyear), col='#9820c3', alpha=0.7, size=1) +
    # Add RSS line
    geom_line(data=rss_population, aes(x=delta_hi, y=ranef_mid), linetype='solid', size=1.25, col='#000000') +
    # Theme
    theme(panel.background = element_rect(fill='white'), panel.border = element_rect(colour='black', fill=NA), 
          axis.text = element_text(size=15, colour='black'), axis.title = element_blank(), legend.position = 'none',
          plot.margin=unit(c(.25,.25,1,1), 'cm')) +
    # Annotate the rmse
    annotate('text', x=pos_x, y=pos_y, label=ranef_RMSE, size=5)

  # make sure plot axes align with GFR plot along y
  ranef_plot <- ranef_plot + ylim(layer_scales(gfr_plot)$y$range$range[1],layer_scales(gfr_plot)$y$range$range[2])
  
  # Convert x axis to log if distance to road
  if(hab=='road'){
    gfr_plot <- gfr_plot + scale_x_continuous(breaks=c(3.912023,5.298317,6.214608,7.090077,8.006368), labels=c('0.05', '0.20', '0.50', '1.20', '3.00'))
    ranef_plot <- ranef_plot + scale_x_continuous(breaks=c(3.912023,5.298317,6.214608,7.090077,8.006368), labels=c('0.05', '0.20', '0.50', '1.20', '3.00'))
  }
  # Reset breaks to single digits if mixedwood
  if(hab=='mixedwood'){
    ranef_plot <- ranef_plot + scale_y_continuous(breaks=c(-8, -4, 0, 4), labels=c(-8, -4, 0, 4))
  }
  
  ##################################
  ### PART 4 - SAVE PLOT OUTPUTS ###
  ##################################
  
  # Arrange final plots in panels and save
  model_plots <- plot_grid(ranef_plot, gfr_plot, grad_legend, ncol=3, nrow=1, rel_widths = c(7,7,4), labels=c('A - Ran. Eff.','B - GFR'), label_x=c(.05,.05), label_y=c(0.95,0.95), label_size=20)
  # tiff(paste("figures/", '.tiff', sep=hab), width = 11, height = 6, units = 'in', res = 300)
  assign(paste(hab, 'plot', sep='_'), grid.arrange(arrangeGrob(model_plots, left = textGrob(y_title, rot=90, gp=gpar(fontsize=17), vjust=1, hjust=.35), bottom = textGrob(label=x_title, gp=gpar(fontsize=17), hjust=0.9, vjust=-0.2))))

  assign(paste(hab, 'rss_population', sep='_'), rss_population)
  assign(paste(hab, 'rss_individual', sep='_'), rss_individual)
  
  assign(paste(hab, 'ranef_plot', sep='_'), ranef_plot)
  assign(paste(hab, 'gfr_plot', sep='_'), gfr_plot)
  
}

# tiff("figures/rss_panel.tif", width = 11, height = 11, units = 'in', res = 300)
plot_grid(mixedwood_plot, road_plot, ncol=1, nrow=2)


