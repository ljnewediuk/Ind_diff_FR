###################################################################
###################################################################
## 6 - RSS FIGURES - RECONCILING INDIVIDUALS WITH THE POPULATION ##
###################################################################
###################################################################
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

rmse_dat <- readRDS('results/prediction_outputs/rmse_data.rds')

# Remove outliers
individual_rss <- individual_rss[!c(elkyear==22 & dates=='01-02_01-31' | elkyear==55 & dates=='01-02_01-31')]

# Make unique entry for each iteration-elkyear and convert m to km
individual_rss <- individual_rss %>% mutate(unique_ID = paste(elkyear, iteration, sep='_'), road_avail = road_avail/1000, mean_road = mean_road/1000)

# Sqrt of number of dates to help with 95% CI calculation
ci_denom <- sqrt(length(unique(population_rss$dates)))

# Pivot to long table with model type as group
population_rss <- melt(population_rss, id.vars = c('dates', 'road_avail', 'mixedwood_avail'), measure.vars = colnames(population_rss)[4:15], variable.name = 'model_type', value.name = 'rss')
# Summarize mean and standard error for each model type and habitat
population_rss <- population_rss %>% group_by(model_type, road_avail, mixedwood_avail) %>% summarise(mean=mean(rss), sd=sd(rss)) %>% mutate(lower=mean-((sd/ci_denom)*1.96), upper=mean+((sd/ci_denom)*1.96))
# Add columns for random effects and GFR models and habitat
population_rss <- population_rss %>% mutate(
  hab=ifelse(grepl('mixedwood', model_type), 'mixedwood', 'road'), 
  mod=ifelse(grepl('ranef', model_type), 'ranef', 'gfr') )
# Convert m to km
population_rss <- population_rss %>% mutate(road_avail=road_avail/1000)
# Split based on random effects or gfr model, and condense random effects
gfr_rss <- population_rss[population_rss$mod=='gfr',] %>% select(model_type, road_avail, mixedwood_avail, mean, lower, upper, hab)
ranef_rss <- population_rss[population_rss$mod=='ranef' & population_rss$model_type %in% 
                              c('mixedwood_high_ranef', 'road_high_ranef'),] %>% ungroup(model_type) %>% select(road_avail, mixedwood_avail, mean, lower, upper, hab)


make_plots<- function(hab){
  
  vir_palette <- ifelse(hab=='road', 'inferno', 'viridis')
  hab2 <- ifelse(hab=='road', 'mixedwood', 'road')
  
  y_min <- floor(min(c(unlist(population_rss[population_rss$hab==hab,]$mean), unlist(individual_rss[,get(paste('OOS', hab, sep='_'))]))))
  y_max <- ceiling(max(c(unlist(population_rss[population_rss$hab==hab,]$mean), unlist(individual_rss[,get(paste('OOS', hab, sep='_'))]))))
  x_min <- 0
  x_max <- ceiling(max(c(as.numeric(unlist(population_rss[, paste(hab, 'avail', sep='_')])), as.numeric(unlist(individual_rss[, get(paste(hab, 'avail', sep='_'))])))))
  legend_title <- ifelse(hab=='mixedwood',  'Mean distance to road \n in home range (km)', 'Mean mixedwood \n cover in home range')
  y_title <- ifelse(hab=='mixedwood', 'log-RSS for mixed forest', 'log-RSS for distance to road')
  if(hab=='road'){
    x_title <- bquote("Distance to road (km) at location x"[j])
    panel_labels <- c('C - Ranef', 'D - GFR')
  } else {
    x_title <- bquote("Mixed forest cover at location x"[j])
    panel_labels <- c('A - Ranef', 'B - GFR')
  }
  
  gfr_plot <- ggplot() +
    geom_hline(yintercept=0, linetype='dotted', alpha=0.5) +
    geom_line(data=individual_rss, aes(x=get(paste(hab, 'avail', sep='_')), y=get(paste('OOS', hab, sep='_')), group=unique_ID, colour=get(paste('mean', hab2, sep='_'))), alpha=0.3, size=0.3) + scale_colour_viridis_c(option=vir_palette, name=legend_title) +
    geom_line(data=gfr_rss[gfr_rss$hab==hab & gfr_rss$model_type %in% c(paste(hab, 'high_gfr', sep='_'), paste(hab, 'low_gfr', sep='_')),], aes(x=get(paste(hab, 'avail', sep='_')), y=mean, group=model_type, linetype=model_type), size=1) + 
    geom_ribbon(data=gfr_rss[gfr_rss$hab==hab & gfr_rss$model_type %in% c(paste(hab, 'high_gfr', sep='_'), paste(hab, 'low_gfr', sep='_')),], aes(x=get(paste(hab, 'avail', sep='_')), ymin=lower, ymax=upper, group=model_type), fill='black', alpha=0.3) +
    theme(panel.background = element_rect(fill='white', colour='black'), axis.title=element_blank(), axis.text.x=element_text(size=15, colour='black'), axis.text.y=element_blank(), axis.ticks.y=element_blank(), 
          legend.title = element_blank(), legend.text= element_text(size=15, colour='black'), legend.position=c(((x_max-x_min)/2), (y_max-((y_max-y_min)/20))), legend.key=element_rect(fill='white'), plot.margin=unit(c(0.5,0.5,0.5,0.5), 'cm'), panel.grid=element_blank()) +
    guides(colour='none') +
    scale_linetype_manual(values= c('dashed', 'dotted'), labels=c('High', 'Low')) +
    scale_y_continuous(limits=c(y_min, y_max)) +
    scale_x_continuous(limits=c(x_min, x_max), breaks=seq(x_min, x_max, (x_max-x_min)/5)) +
    annotate(geom='text', label=paste('RMSE =', paste(signif(mean(rmse_dat[, get(paste('gfr', hab, sep='_'))]),2), paste('±', signif(sd(rmse_dat[, get(paste('gfr', hab, sep='_'))])/sqrt(nrow(rmse_dat)),2)))), size=5, x=(x_max-x_min)/2, y=y_min+((y_max-y_min)/10))
  
  ranef_plot <- ggplot() +
    geom_hline(yintercept=0, linetype='dotted', alpha=0.5) +
    geom_line(data=individual_rss, aes(x=get(paste(hab, 'avail', sep='_')), y=get(paste('OOS', hab, sep='_')), group=unique_ID, colour=get(paste('mean', hab2, sep='_'))), alpha=0.3, size=0.3) + scale_colour_viridis_c(option=vir_palette, name=legend_title) +
    geom_line(data=ranef_rss[ranef_rss$hab==hab,], aes(x=get(paste(hab, 'avail', sep='_')), y=mean), size=1) + 
    geom_ribbon(data=ranef_rss[ranef_rss$hab==hab,], aes(x=get(paste(hab, 'avail', sep='_')), ymin=lower, ymax=upper), fill='black', alpha=0.3) +
    theme(panel.background = element_rect(fill='white', colour='black'), axis.title=element_blank(), axis.text=element_text(size=15, colour='black'),
          legend.title = element_text(size=15, colour='black'), legend.text= element_text(size=15, colour='black'), plot.margin=unit(c(0.5,0.5,0.5,0.5), 'cm'), panel.grid=element_blank()) +
    scale_y_continuous(limits=c(y_min, y_max), breaks=seq(y_min, y_max, (y_max-y_min)/5)) +
    scale_x_continuous(limits=c(x_min, x_max), breaks=seq(x_min, x_max, (x_max-x_min)/5)) +
    annotate(geom='text', label=paste('RMSE =', paste(signif(mean(rmse_dat[, get(paste('ranef', hab, sep='_'))]),2), paste('±', signif(sd(rmse_dat[, get(paste('ranef', hab, sep='_'))])/sqrt(nrow(rmse_dat)),2)))), size=5, x=(x_max-x_min)/2, y=y_min+((y_max-y_min)/10))
  
  hab_legend <- get_legend(ranef_plot)
  
  # gfr_plot <- gfr_plot + theme(legend.position = 'none')
  ranef_plot <- ranef_plot + theme(legend.position = 'none')
  
  model_plots <- plot_grid(ranef_plot, gfr_plot, hab_legend, ncol=3, nrow=1, rel_widths = c(7,6,4), labels=panel_labels, label_x=c(.05,.05), label_y=c(0.95,0.95), label_size=20)
  model_plots <- grid.arrange(arrangeGrob(model_plots, left = textGrob(y_title, rot=90, gp=gpar(fontsize=17), vjust=.5, hjust=.35), bottom = textGrob(label=x_title, gp=gpar(fontsize=17), hjust=0.9, vjust=.5)))
  
  return(model_plots)
  
}

mixedwood_plot <- make_plots('mixedwood')
road_plot <- make_plots('road')

# tiff('figures/rss_panel.tiff', width = 13, height = 10, units = 'in', res = 300)
plot_grid(mixedwood_plot, road_plot, ncol=1, nrow=2)







