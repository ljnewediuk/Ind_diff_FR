
############################################################################################
############################################################################################
## PART 3: COMPARE MODELS ##
############################################################################################
############################################################################################
## Loops through all elk-years from OOSData with standard errors less than 30

### Packages ----
libs <- c('data.table', 'rgdal', 'sp', 'adehabitatHR', 'raster', 'lme4', 'survival', 'glmmTMB', 'tidyverse', 'Metrics')
lapply(libs, require, character.only = TRUE)

# landcover
mixedwood <- raster("input/rasters/RMNP_mixedwood_ppn.tif")
names(mixedwood) <- 'mixedwood'
# distance to road
distToRoad <- raster("input/rasters/RMNP_dist_to_municipal_road.tif")
names(distToRoad) <- 'road'
# Read individual model outputs
covariates <- readRDS('results/RSF_outputs/mw_rd_RSFs.rds')
# Check which individual iterations have >= 1 week of data and summarize by year and date
pull_list <- covariates %>% filter(numb_pts >= 84) %>% group_by(elkyear, iteration, dates, numb_pts, type) %>% summarise()
pull_list <- pull_list[pull_list$type=='OOS',]

# Create data table to store correlations
# rss_data <- data.table()
# rmse_data <- data.table()

# Or load existing table to continue
rss_data <- readRDS('results/prediction_outputs/temp_rss.rds')
rmse_data <- readRDS('results/prediction_outputs/temp_rmse.rds')

# Remove all rows from the list that have already been completed
pull_list <- anti_join(pull_list, rss_data, by=c('elkyear', 'iteration', 'dates'))

for(row in 1:nrow(pull_list)){
  
  print(paste('elkyear:', pull_list[row,]$elkyear), sep=' ')
  print(pull_list[row,]$iteration)
  
  ### 1 - Confirm all the files are present to run the prediction
  
  # Confirm the individual has both an OOS and WSD model
  if(length(list.files(paste('results/RSF_models_individuals/', paste(pull_list[row,]$dates, pull_list[row,]$elkyear, sep='/'), sep='')))<2){
    next
  }
  # Check if OOS MCP does not exist for the individual in that iteration
  if(!any(list.files(paste('input/MCPs/', paste(pull_list[row,]$dates, pull_list[row,]$elkyear, sep='/'), sep='')) %in% 'OOS_mcp.rds')){
    next
  }
  
  ### 2 - Collect the files
  
  # Habitat data
  hab_data <- readRDS(paste('input/RSF_data/', paste(pull_list[row,]$dates, 'OOS_data.rds',  sep='/'), sep=''))
  # Subset to only observed data during iteration
  hab_data <- hab_data[elkyear==pull_list[row,]$elkyear & iteration==pull_list[row,]$iteration & sample==1]
  # MCP
  id_mcp <- readRDS(paste('input/MCPs/', paste(pull_list[row,]$dates, paste(pull_list[row,]$elkyear, 'OOS_mcp.rds', sep='/'), sep='/'), sep=''))
  # Individual models
  OOS_mod <- readRDS(paste('results/RSF_models_individuals/', paste(pull_list[row,]$dates, paste(pull_list[row,]$elkyear, 'OOS_mod.rds', sep='/'), sep='/'), sep=''))
  WS_mod <- readRDS(paste('results/RSF_models_individuals/', paste(pull_list[row,]$dates, paste(pull_list[row,]$elkyear, 'WS_mod.rds', sep='/'), sep='/'), sep=''))
  # Population level models
  gfr_mod <- readRDS(paste('results/RSF_models_population/', paste(pull_list[row,]$dates, 'gfr_mod.rds', sep='/'), sep=''))
  ranef_mod <- readRDS(paste('results/RSF_models_population/', paste(pull_list[row,]$dates, 'ranef_mod.rds', sep='/'), sep=''))
  
  ### 3 - Organize the raster
  mixedwood_crop <- mask(mixedwood, id_mcp)
  road_crop <- mask(distToRoad, id_mcp)
  road_crop <- resample(road_crop, mixedwood_crop, method='bilinear')
  
  # Extract raster means for FR
  mean_mixedwood <- mean(mixedwood_crop@data@values, na.rm=T)
  mean_road <- mean(road_crop@data@values, na.rm=T)
  
  # Define habitat availability
  road_avail <- seq(min(hab_data$road), max(hab_data$road), length.out=100)
  mixedwood_avail <- seq(min(hab_data$mixedwood), max(hab_data$mixedwood), length.out=100)
  
  ### 3 - Create new data for mixedwood predictions
  x1_mixedwood <- data.table(mixedwood = mixedwood_avail, 
                        road = mean_road,
                        mean_mixedwood,
                        mean_road,
                        weight=1,
                        elkyear=NA)
  
  x1_road <- data.table(mixedwood = mean_mixedwood, 
                             road = road_avail,
                             mean_mixedwood,
                             mean_road,
                             weight=1,
                             elkyear=NA)
  
  x2 <- data.table(mixedwood = mean_mixedwood, 
                             road = mean_road,
                             mean_mixedwood,
                             mean_road,
                             weight=1,
                             elkyear=NA)
  
  ### 4 - Predict and calculate relative probability of selection for each pixel
  for(mod_type in c('gfr', 'ranef', 'OOS')){
    for(hab in c('mixedwood', 'road')){
      # Predict
      # x1
      suppressWarnings(pred_x1 <- predict(get(paste(mod_type, 'mod', sep='_')), get(paste('x1', hab, sep='_'))))
      # x2
      suppressWarnings(pred_x2 <- predict(get(paste(mod_type, 'mod', sep='_')), x2))
      # Relative probability of selection
      log_rss <- as.numeric(pred_x1-pred_x2)
      # Assign
      assign(paste(mod_type, paste(hab, 'rss', sep='_'), sep='_'), log_rss)
    }
    
  }
  
  ### 5 - Calculate RMSE
  for(mod_type in c('gfr', 'ranef')){
    for(hab in c('mixedwood', 'road')){
      # Calculate RMSE
      indiv_rmse <- rmse(get(paste(mod_type, paste(hab, 'rss', sep='_'), sep='_')), get(paste('OOS', paste(hab, 'rss', sep='_'), sep='_')))
      # Assign
      assign(paste(mod_type, paste(hab, 'rmse', sep='_'), sep='_'), indiv_rmse)
    }
    
  }
  
  # Combine rss results into data table row
  rss_individual <- data.table(elkyear = pull_list[row,]$elkyear,
                                        iteration = pull_list[row,]$iteration,
                                        dates = pull_list[row,]$dates,
                                        numb_points = pull_list[row,]$numb_pts,
                                        mean_mixedwood, mean_road, mixedwood_avail, road_avail,
                                        gfr_mixedwood=gfr_mixedwood_rss, ranef_mixedwood=ranef_mixedwood_rss, OOS_mixedwood=OOS_mixedwood_rss,
                                        gfr_road=gfr_road_rss, ranef_road=ranef_road_rss, OOS_road=OOS_road_rss)
  # Bind with main table
  rss_data <- rbind(rss_data, rss_individual)
  
  # Combine rmse into data table row
  rmse_individual <- data.table(elkyear = pull_list[row,]$elkyear,
                                iteration = pull_list[row,]$iteration,
                                dates = pull_list[row,]$dates,
                                numb_points = pull_list[row,]$numb_pts,
                                gfr_mixedwood=gfr_mixedwood_rmse, ranef_mixedwood=ranef_mixedwood_rmse,
                                gfr_road=gfr_mixedwood_rmse, ranef_road=ranef_mixedwood_rmse)
  # Bind with main table
  rmse_data <- rbind(rmse_data, rmse_individual)
  
  # Save a temporary copy
  saveRDS(rss_data, 'results/prediction_outputs/temp_rss.rds')
  saveRDS(rmse_data, 'results/prediction_outputs/temp_rmse.rds')
  
  # Remove old objects to prevent lagging effects
  rm(gfr_mod, ranef_mod, OOS_mod, WS_mod, id_mcp, mixedwood_crop, road_crop, x1_mixedwood, x1_road, x2,
     ranef_mixedwood_rmse, ranef_mixedwood_rss, gfr_mixedwood_rmse, gfr_mixedwood_rss,
     ranef_road_rmse, ranef_road_rss, gfr_road_rmse, gfr_road_rss, OOS_road_rmse, OSS_road_rss)
}

### 6 - Save final copy of results
saveRDS(rss_data, 'results/prediction_outputs/rss_data.rds')
saveRDS(rmse_data, 'results/prediction_outputs/rmse_data.rds')


