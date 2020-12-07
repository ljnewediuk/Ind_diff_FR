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
libs <- c('data.table', 'ggplot2', 'cowplot', 'grid', 'gridExtra')
lapply(libs, require, character.only = TRUE)

### Load elkyears with fewer than 750 points ----
# Npoints_750<- readRDS('input/exclusion_elk.rds')

# Load means
model_means <- readRDS('results/RSF_outputs/model_means.rds')

# Load RMSE
all_rmse <- readRDS('results/gof/all_rmse.rds')

# Load individual model outputs
indiv_mods <- readRDS('results/RSF_outputs/mw_rd_RSFs.rds')
indiv_mods <- indiv_mods[type=='OOS']

# Remove any indiv_mod iteration with fewer than 150 points
indiv_mods <- indiv_mods[numb_pts>=150]

# Optionally exclude elk with fewer than 750 points
# indiv_mods <- indiv_mods[!elkyear %in% Npoints_750]

##############################
### PART 1: SET PARAMETERS ###
##############################

### Road selection at three levels of forest availability ----

# Range of road values for x axis
rd_range <- seq(8.5,3.9,length.out=100)
delta_hi_rd <- rd_range

# mixedwood availability:
# high
mw_high <- 1.0
# intermediate
mw_mid <- 0.50
# low
mw_low <- 0.01

### Mixedwood selection at three levels of road availability ----

# Range of mixedwood values for x axis
mw_range <- seq(1,0,length.out=100)
delta_hi_mw <- 1-mw_range

# road availability:
# high
rd_high <- 8.5
# intermediate
rd_mid <- 6.2
# low
rd_low <- 3.9

#########################################
### PART 2: COMPLETE RSS CALCULATIONS ###
#########################################

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
  
  #################################
  ### PART 3 - CREATE RSS PLOTS ###
  #################################
  
  ### GFR plot ----
  gfr_plot <- ggplot(data=rss_individual, aes(x=delta_hi, y=rss)) + geom_line(aes(group=elkyear, col=mean)) +
    # Add zero line
    geom_hline(yintercept=0, col='#000000', size=0.5, linetype='solid', alpha=0.5) +
    # Create gradient for individual mean home range cover of variable from low to high
    scale_color_gradient(low=low_col, high=high_col, name=leg_title) +
    # Add RSS lines
    geom_line(data=rss_population, aes(x=delta_hi, y=gfr_low), linetype='dotted', size=1, col='#000000') +
    geom_line(data=rss_population, aes(x=delta_hi, y=gfr_mid), linetype='dotdash', size=1, col='#000000') +
    geom_line(data=rss_population, aes(x=delta_hi, y=gfr_high), linetype='longdash', size=1, col='#000000') +
    # Theme
    theme(panel.background = element_rect(fill='white'), panel.border = element_rect(colour='black', fill=NA), 
          legend.title = element_text(size=15, colour='black'), legend.text= element_text(size=15, colour='black'), 
          axis.text.x = element_text(size=15, colour='black'), axis.title = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
    # Annotate the rmse
    annotate('text', x=pos_x, y=pos_y, label=gfr_RMSE, size=5) +
    # Annotate high, medium, and low
    annotate('text', y=(rss_population[delta_hi==min(rss_population$delta_hi)]$gfr_low), x=pos_x_lev, label='L', size=4.5, fontface='bold') +
    annotate('text', y=rss_population[delta_hi==min(rss_population$delta_hi)]$gfr_mid, x=pos_x_lev, label='M', size=4.5, fontface='bold') +
    annotate('text', y=(rss_population[delta_hi==min(rss_population$delta_hi)]$gfr_high), x=pos_x_lev, label='H', size=4.5, fontface='bold')
    
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
    # Add RSS line
    geom_line(data=rss_population, aes(x=delta_hi, y=ranef_mid), linetype='solid', size=1, col='#000000') +
    # Theme
    theme(panel.background = element_rect(fill='white'), panel.border = element_rect(colour='black', fill=NA), 
          axis.text = element_text(size=15, colour='black'), axis.title = element_blank(), legend.position = 'none') +
    # Annotate the rmse
    annotate('text', x=pos_x, y=pos_y, label=ranef_RMSE, size=5)
  
  # make sure plot axes align with GFR plot along y
  ranef_plot <- ranef_plot + ylim(layer_scales(gfr_plot)$y$range$range[1],layer_scales(gfr_plot)$y$range$range[2])
  
  # Convert x axis to log if distance to road
  if(hab=='road'){
    gfr_plot <- gfr_plot + scale_x_continuous(breaks=c(3.912023,5.298317,6.214608,7.090077,8.006368), labels=c(0.05, 0.20, 0.50, 1.20, 3.00))
    ranef_plot <- ranef_plot + scale_x_continuous(breaks=c(3.912023,5.298317,6.214608,7.090077,8.006368), labels=c(0.05, 0.20, 0.50, 1.20, 3.00))
  }
  
  ##################################
  ### PART 4 - SAVE PLOT OUTPUTS ###
  ##################################
  
  # Arrange final plots in panels and save
  model_plots <- plot_grid(ranef_plot, gfr_plot, grad_legend, ncol=3, nrow=1, rel_widths = c(7,7,4), labels=c('A - Ran. Eff.','B - GFR'), label_x=c(-.05,-.05), label_y=c(0.95,0.95), label_size=20)
  # tiff(paste("figures/", '.tiff', sep=hab), width = 11, height = 6, units = 'in', res = 300)
  grid.arrange(arrangeGrob(model_plots, left = textGrob(y_title, rot=90, gp=gpar(fontsize=17), vjust=0.2), bottom = textGrob(label=x_title, gp=gpar(fontsize=17), hjust=0.7)))

  assign(paste(hab, 'rss_population', sep='_'), rss_population)
  assign(paste(hab, 'rss_individual', sep='_'), rss_individual)
  
  assign(paste(hab, 'ranef_plot', sep='_'), ranef_plot)
  assign(paste(hab, 'gfr_plot', sep='_'), gfr_plot)
  
}



