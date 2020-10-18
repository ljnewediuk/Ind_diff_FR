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
rd_range <- seq(10,6,length.out=100)
delta_hi_rd <- rd_range

# mixedwood availability:
# high
mw_high <- log(max(indiv_mods$mean_mixedwood))
# intermediate
mw_mid <- log(mean(indiv_mods$mean_mixedwood))
# low
mw_low <- log(min(indiv_mods$mean_mixedwood))

### Mixedwood selection at three levels of road availability ----

# Range of mixedwood values for x axis
mw_range <- seq(1,0,length.out=100)
delta_hi_mw <- 1-mw_range

# road availability:
# high
rd_high <- log(max(indiv_mods$mean_road))
# intermediate
rd_mid <- log(mean(indiv_mods$mean_road))
# low
rd_low <- log(min(indiv_mods$mean_road))

#########################################
### PART 2: COMPLETE RSS CALCULATIONS ###
#########################################

for(hab in c('mixedwood', 'road')){

  # Set conditions for each habitat type
  if(hab=='mixedwood'){
    delta_hi <- delta_hi_mw
    low_col <- '#F0FFF0'
    high_col <- '#23A625'
    x_title <- 'Mean mixed forest in home range'
    leg_title <- 'Mean road distance \n in home range (km)'
    y_title <- 'RSS for mixed forest'
    ranef_RMSE <- paste('RMSE =', round(mean(all_rmse$ranef_mw, na.rm=T), digits=2))
    gfr_RMSE <- paste('RMSE =', round(mean(all_rmse$gfr_mw, na.rm=T), digits=2))
    pos_x <- 0.25
    pos_y <- -0.6
    pos_L <- 1.43
    pos_M <- 0.98
    pos_H <- 0.81
    pos_x_lev <- 1
  } else {
    delta_hi <- delta_hi_rd
    low_col <- '#FAF6D2'
    high_col <- '#FC7878'
    x_title <- 'Mean log(distance to road) in home range'
    leg_title <- 'Mean mixed forest \n in home range'
    y_title <- 'RSS for distance from road'
    ranef_RMSE <- paste('RMSE =', round(mean(all_rmse$ranef_rd), digits=2))
    gfr_RMSE <- paste('RMSE =', round(mean(all_rmse$gfr_rd), digits=2))
    pos_x <- 7
    pos_y <- -28
    pos_L <- 37
    pos_M <- 23
    pos_H <- 20
    pos_x_lev <- 10
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
      mean_hab <- indiv_sub$mean_road
    } else {
      mean_hab <- indiv_sub$mean_mixedwood
    }
    
    # Set parameters
    hi <- mean(indiv_sub[term==hab]$estimate)
    hj_mu <- mean(mean_hab)
    
    # Calculate RSS
    rss <- delta_hi*hi
    rss_sub <- data.table(elkyear=rep(i, length(hi)), delta_hi=delta_hi, rss=rss, mean=rep(hj_mu, length(hi)))
    rss_individual <- rbind(rss_individual, rss_sub)
  }
  
  ##############################
  ### PART 2 - B) POPULATION ###
  ##############################
  
  for(mod_type in c('gfr', 'ranef', 'no_FR')){
    for(covar_level in c('high', 'mid', 'low')){
      
      # Subset model type
      model_sub <- model_means[model==mod_type]
      
      # Set condtions for habitat type
      if(hab=='mixedwood'){
        v1 <- 'mw'
        v2 <- 'rd'
      } else {
        v1 <- 'rd'
        v2 <- 'mw'
      }
        
      # Set parameters
      hj <- get(paste(v2, covar_level, sep='_'))
      hx <- model_sub[covariate==v1]$beta
      hxij <- model_sub[covariate==paste(v1, v2, sep='_')]$beta
      hxii <- model_sub[covariate==paste(v1, v1, sep='_')]$beta
      
      # Set condtions for model type (which equation to use)
      if(mod_type=='gfr'){
        rss <- delta_hi*(hx+hxij*hj+hxii)
      } else {
        rss <- delta_hi*hx
      }
      
      # Calculate RSS
      rss <- as.data.table(rss)
      colnames(rss) <- paste(mod_type, covar_level, sep='_')
      rss_population <- cbind(rss_population, rss)
  
    }
        
  }
  
  # Convert mean distance to road to km if hab = mixedwood
  if(hab=='mixedwood'){
    rss_individual$mean <-rss_individual$mean/1000
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
    annotate('text', y=pos_L, x=pos_x_lev, label='L', size=6, fontface='bold') +
    annotate('text', y=pos_M, x=pos_x_lev, label='M', size=6, fontface='bold') +
    annotate('text', y=pos_H, x=pos_x_lev, label='H', size=6, fontface='bold')
    
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
  
  ##################################
  ### PART 4 - SAVE PLOT OUTPUTS ###
  ##################################
  
  # Arrange final plots in panels and save
  model_plots <- plot_grid(ranef_plot, gfr_plot, grad_legend, ncol=3, nrow=1, rel_widths = c(7,7,4), labels=c('A - Ran. Eff.','B - GFR'), label_x=c(-.05,-.05), label_y=c(0.95,0.95), label_size=20)
  # tiff(paste("figures/", '.tiff', sep=hab), width = 11, height = 6, units = 'in', res = 300)
  grid.arrange(arrangeGrob(model_plots, left = textGrob(y_title, rot=90, gp=gpar(fontsize=17), vjust=0.2), bottom = textGrob(x_title, gp=gpar(fontsize=17), hjust=0.7)))

  assign(paste(hab, 'rss_population', sep='_'), rss_population)
  assign(paste(hab, 'rss_individual', sep='_'), rss_individual)
  
  assign(paste(hab, 'ranef_plot', sep='_'), ranef_plot)
  assign(paste(hab, 'gfr_plot', sep='_'), gfr_plot)
  
}

