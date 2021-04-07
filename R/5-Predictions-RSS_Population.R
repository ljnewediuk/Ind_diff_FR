#############################################
#############################################
## 5 - COMPARE MODELS (RSS FOR POPULATION) ##
#############################################
#############################################

### Packages ----
libs <- c('data.table', 'ggplot2', 'cowplot', 'grid', 'gridExtra', 'dplyr')
lapply(libs, require, character.only = TRUE)

# Read individual model outputs
covariates <- readRDS('results/RSF_outputs/mw_rd_RSFs.rds')
# Check which individual iterations have >= 1 week of data and summarize by year and date
pull_list <- covariates %>% filter(numb_pts >= 84) %>% group_by(elkyear, iteration, dates, numb_pts, type) %>% summarise()
pull_list <- pull_list[pull_list$type=='OOS',]


##############################
### PART 1: SET PARAMETERS ###
##############################

# Make list of 0.1 percentile, .9 percentile, and mean habitat values for each iteration
hab_data <- data.table()
for(row in 1:nrow(pull_list)){
  # Habitat data
  OOS_data <- readRDS(paste('input/RSF_data/', paste(pull_list[row,]$dates, 'OOS_data.rds',  sep='/'), sep=''))
  for(hab in c('mixedwood', 'road')){
    # Collect quantiles
    assign(paste(hab, 'q75', sep='_'), quantile(OOS_data[,get(hab)], .75))
    assign(paste(hab, 'q50', sep='_'), mean(OOS_data[,get(hab)], na.rm=T))
    assign(paste(hab, 'q25', sep='_'), quantile(OOS_data[,get(hab)], .25))
    # Collect min/max
    assign(paste(hab, 'max', sep='_'), max(OOS_data[,get(hab)], na.rm=T))
    assign(paste(hab, 'mean', sep='_'), mean(OOS_data[,get(hab)], na.rm=T))
    assign(paste(hab, 'min', sep='_'), min(OOS_data[,get(hab)], na.rm=T))
  }
  hab_row <- data.table(elkyear=unique(OOS_data$elkyear), mixedwood_mean, road_mean, mixedwood_min, road_min, mixedwood_max, road_max,
                        mixedwood_q25, mixedwood_q50, mixedwood_q75, road_q25, road_q50, road_q75)
  hab_data <- rbind(hab_data, hab_row)
}

# Range of road and mixedwood values for x axis
road_avail <- seq(min(hab_data$road_min), max(hab_data$road_max), length.out=100)
mixedwood_avail <- seq(min(hab_data$mixedwood_min), max(hab_data$mixedwood_max), length.out=100)


# mixedwood availability:
# high
mixedwood_high <- mean(hab_data$mixedwood_q75)
# intermediate
mixedwood_mid <- mean(hab_data$mixedwood_q50)
# low
mixedwood_low <- mean(hab_data$mixedwood_q25)

# road availability:
# high
road_high <- mean(hab_data$road_q75)
# intermediate
road_mid <- mean(hab_data$road_q50)
# low
road_low <- mean(hab_data$road_q25)

#########################################
### PART 2: COMPLETE RSS CALCULATIONS ###
#########################################

# Add to existing results table
population_rss <- readRDS('results/prediction_outputs/temp_population_rss_data.rds')
# Remove completed rows from pull table
pull_list <- anti_join(pull_list, population_rss, by=c('dates'))

# Or create new table for results
# population_rss <- data.table()

for(dts in unique(pull_list$dates)){
  
  gfr_mod <- readRDS(paste('results/RSF_models_population', paste(dts, 'gfr_mod.rds', sep='/'), sep='/'))
  ranef_mod <- readRDS(paste('results/RSF_models_population', paste(dts, 'ranef_mod.rds', sep='/'), sep='/'))
  
  for(hab in c('mixedwood', 'road')){
    
    hab2 <- ifelse(hab=='mixedwood', 'road', 'mixedwood')
    
    for(availability in c('high', 'mid', 'low')){
    
      print(dts)
      print(hab)
      print(availability)
      
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
    
      # Rename columns to fit parameters
      hab_headings <- c(hab, hab2, paste('mean', hab, sep='_'), paste('mean', hab2, sep='_'), 'weight', 'elkyear')
      colnames(x1_dat) <- hab_headings
      colnames(x2_dat) <- hab_headings

      
      for(mod_type in c('gfr', 'ranef')){
        # RSS for gfr model
        pred_x1 <- suppressWarnings(predict(get(paste(mod_type, 'mod', sep='_')), x1_dat))
        pred_x2 <- suppressWarnings(predict(get(paste(mod_type, 'mod', sep='_')), x2_dat))
        # Log-RSS
        log_rss <- as.numeric(pred_x1-pred_x2)
        #  Assign
        assign(paste(mod_type, 'log_rss', sep='_'), log_rss)
      }
      
      assign(paste(availability, 'gfr', sep='_'), gfr_log_rss)
      assign(paste(availability, 'ranef', sep='_'), ranef_log_rss)
    
  }
  
  
    assign(paste(hab, paste('high', 'gfr', sep='_'), sep='_'), high_gfr)
    assign(paste(hab, paste('mid', 'gfr', sep='_'), sep='_'), mid_gfr)
    assign(paste(hab, paste('low', 'gfr', sep='_'), sep='_'), low_gfr)
    
    assign(paste(hab, paste('high', 'ranef', sep='_'), sep='_'), high_ranef)
    assign(paste(hab, paste('mid', 'ranef', sep='_'), sep='_'), mid_ranef)
    assign(paste(hab, paste('low', 'ranef', sep='_'), sep='_'), low_ranef)
    
  }
  
  date_row <- data.table(dates = dts, road_avail, mixedwood_avail, 
                         mixedwood_high_gfr, mixedwood_mid_gfr, mixedwood_low_gfr, road_high_gfr, road_mid_gfr, road_low_gfr,
                         mixedwood_high_ranef, mixedwood_mid_ranef, mixedwood_low_ranef, road_high_ranef, road_mid_ranef, road_low_ranef)
  
  population_rss <- rbind(population_rss, date_row)
  
  # Save temporary file
  saveRDS(population_rss, 'results/prediction_outputs/temp_population_rss_data.rds')
}

# Save final version
saveRDS(population_rss, 'results/prediction_outputs/population_rss_data.rds')
