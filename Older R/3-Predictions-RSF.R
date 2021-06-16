#####################################################
#####################################################
## 3 - COMPARE MODELS (GEOGRAPHIC RSS PREDICTIONS) ##
#####################################################
#####################################################
## Loops through all elk-years from OOSData with 
## standard errors less than 30

### Packages ----
libs <- c('data.table', 'rgdal', 'sp', 'adehabitatHR', 'raster', 'lme4', 'survival', 'glmmTMB', 'tidyverse')
lapply(libs, require, character.only = TRUE)

# Import raster data into directory
system("mkdir ~/Documents/R-Projects/individual_fr/rasters/")
system("cp ~/Documents/Spatial*Data/Manitoba*Data/distance*to/RMNP_dist_to_municipal_road.tif ~/Documents/R-Projects/individual_fr/rasters/")
system("cp ~/Documents/Spatial*Data/Manitoba*Data/landcover/mli*data*2004-2006/RMNP_mixedwood_ppn.tif ~/Documents/R-Projects/individual_fr/rasters/")
# Read landcover
mixedwood <- raster("rasters/RMNP_mixedwood_ppn.tif")
names(mixedwood) <- 'mixedwood'
# Read road data
distToRoad <- raster("rasters/RMNP_dist_to_municipal_road.tif")
names(distToRoad) <- 'road'

# Check which individual iterations have >= 1 week of data and summarize by year and date
pull_list <- covariates %>% filter(numb_pts >= 84) %>% group_by(elkyear, iteration, dates, numb_pts, type) %>% summarise()
pull_list <- pull_list[pull_list$type=='OOS',]

# Create data table to store correlations
# correlation_data <- data.table()

# Or load existing table to continue
correlation_data <- readRDS('results/prediction_outputs/temp_correlations.rds')

# Remove all rows from the list that have already been completed
pull_list <- anti_join(pull_list, correlation_data, by=c('elkyear', 'iteration', 'dates'))

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
  
  ### 3 - Create new data for predictions
  new_dat <- data.table(mixedwood=na.omit(mixedwood_crop@data@values), 
                        road=na.omit(road_crop@data@values),
                        mean_mixedwood,
                        mean_road,
                        weight=1,
                        elkyear=NA)
  
  ### 4 - Predict and calculate relative probability of selection for each pixel
  for(mod_type in c('gfr', 'ranef', 'OOS', 'WS')){
    # Predict
    suppressWarnings(pred_mod <- predict(get(paste(mod_type, 'mod', sep='_')), new_dat))
    # Relative probability of selection
    pred_mod <- pred_mod/sum(pred_mod)
    # Assign
    assign(paste(mod_type, 'pred', sep='_'), pred_mod)
  }
  
  ### 5 - Calculate correlation with OOS model
  for(mod_type in c('gfr', 'ranef', 'WS')){
    corr_r2 <- summary(lm(get(paste(mod_type, 'pred', sep='_')) ~ OOS_pred))$adj.r.squared
    assign(paste(mod_type, 'corr', sep='_'), corr_r2)
  }
  
  # Combine results into data table row
  correlations_individual <- data.table(elkyear = pull_list[row,]$elkyear,
                                        iteration = pull_list[row,]$iteration,
                                        dates = pull_list[row,]$dates,
                                        numb_points = pull_list[row,]$numb_pts,
                                        WS_corr, gfr_corr, ranef_corr)
  # Bind with main table
  correlation_data <- rbind(correlation_data, correlations_individual)
  
  # Save a temporary copy
  saveRDS(correlation_data, 'results/prediction_outputs/temp_correlations.rds')
  
  # Remove old objects to prevent lagging effects
  rm(gfr_mod, ranef_mod, OOS_mod, WS_mod, id_mcp, mixedwood_crop, road_crop, correlations_individual, new_dat)
}

### 6 - Save final copy of results
saveRDS(correlation_data, 'results/prediction_outputs/correlations.rds')



