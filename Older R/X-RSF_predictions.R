############################################################################################
############################################################################################
## 2 - RSF MODELS AND CORRELATIONS WITH INDIVIDUAL MODELS
############################################################################################
############################################################################################
## Returns:
## 1) Population level RSFs:
##    a) No functional response - random intercepts only
##    b) Random effects - random intercept and coefficients
##    c) Generalized functional response
## 1) Table of all correlations between selection predicted by each population
##    level model and selection predicted by the individual's excluded data

### Packages ----
libs <- c('data.table', 'rgdal', 'sp', 'adehabitatHR', 'raster', 'lme4', 'survival', 'glmmTMB')
lapply(libs, require, character.only = TRUE)

### Load data from RSF outputs folder ----

# List of iterations already completed
done_list <- list.files(path='results/prediction_outputs/')
# List of all folders
folder_list <- list.files(path="input/RSF_data/")
# Subtract completed folders from all folders
folder_list <- folder_list[!folder_list %in% done_list]

### Load data ----

# Spatial data
dat <- readRDS('input/dat_cleaned.rds')

# landcover
mixedwood <- raster("input/rasters/RMNP_mixedwood_ppn.tif")
names(mixedwood) <- 'mixedwood'

# distance to road
distToRoad <- raster("input/rasters/RMNP_dist_to_municipal_road.tif")
names(distToRoad) <- 'road'

############################### LOOP #######################################################


############################################################################################
############################################################################################
## PART 1: COMPILE THE DATA ##
############################################################################################
############################################################################################
## Pull data from folders based on folder list

# Specify existing data table to compile results (if left off at a previous iteration)
# 
no_FR_outputs <- readRDS('results/RSF_outputs/temp_no_FR.rds')
gfr_outputs <- readRDS('results/RSF_outputs/temp_gfr.rds')
ranef_outputs <- readRDS('results/RSF_outputs/temp_ranef.rds')

# Specify blank data table to compile results

# no_FR_outputs <- data.table()
# gfr_outputs <- data.table()
# ranef_outputs <- data.table()

# specify input folder for iteration 

for (input_folder in folder_list){
  
  # Data for population models and within-sample individual models
  WS_data <- readRDS(file = paste0("input/RSF_data/", paste(input_folder, "WS_data.rds", sep = "/")))
  
  # Data for out-of-sample individual models
  OOS_data <- readRDS(file = paste0("input/RSF_data/", paste(input_folder, "OOS_data.rds", sep = "/")))
  
  ############################################################################################
  ############################################################################################
  ## PART 2: RUN POPULATION LEVEL MODELS ##
  ############################################################################################
  ############################################################################################
  
  # Define the covariate sets
  base_covars=c('I(log(road))', 'mixedwood')
  ranef_covars=c('(1 | elkyear)', '(0 + I(log(road)) | elkyear)', '(0 + mixedwood | elkyear)')
  gfr_covars=c('I(log(road))*I(log(mean_road))', 'mixedwood*mean_mixedwood', 'I(log(road))*mean_mixedwood', 'mixedwood*I(log(mean_road))')
  
  # Use a weighted likelihood of 1000 for available points (Muff method)
  WS_data[, 'weight' := .(1000^(1-sample))]
  
  # Label the iteration before modelling
  print(WS_data$iteration[1])
  
  ###### No FR ---
  no_FR <- glmer(reformulate(c(base_covars, '( 1 | elkyear)'), response = 'sample'),
                 family = binomial,
                 data = WS_data)
  no_out <- broom.mixed::tidy(no_FR)
  # Remove extra covariates
  no_out <- no_out[c(2:3),]
  no_out <- as.data.table(no_out)
  no_out[, c('iteration', 'dates', 'type', 'numbelk') := .(rep(WS_data$iteration[1],nrow(no_out)), rep(input_folder, nrow(no_out)), rep('no_FR', nrow(no_out)), rep(length(unique(WS_data$elkyear))))]
  
  ###### Ranef model ---
  ranef <- glmmTMB(reformulate(c(base_covars, ranef_covars), response = 'sample'),
                   family = binomial(),
                   map=list(theta=factor(c(NA,1:2))),
                   data = WS_data, doFit=F, weights=weight)
  # Fix the sd of the first random term (1 | elkyear) to 10^6
  ranef$parameters$theta[1] = log(1e3)
  # ranef$mapArg = list(theta=factor(c(NA,1:2)))
  ranef_mod <- glmmTMB::fitTMB(ranef)
  # Tidy
  ranef_out <- broom.mixed::tidy(ranef_mod)
  # Remove extra covariates
  ranef_out <- ranef_out[c(2:3),]
  ranef_out <- as.data.table(ranef_out)
  ranef_out[, c('iteration', 'dates', 'type', 'numbelk') := .(rep(WS_data$iteration[1],nrow(ranef_out)), rep(input_folder, nrow(ranef_out)), rep('ranef', nrow(ranef_out)), rep(length(unique(WS_data$elkyear))))]
  
  ###### GFR model ---
  gfr <- glmmTMB(reformulate(c(base_covars, gfr_covars, ranef_covars), response = 'sample'),
                 family = binomial(),
                 data = WS_data, doFit=F, weights=weight)
  # Fix the sd of the first random term (1 | elkyear) to 10^6
  gfr$parameters$theta[1] = log(1e3)
  gfr$mapArg = list(theta=factor(c(NA,1:2)))
  gfr_mod <- glmmTMB::fitTMB(gfr)
  # Tidy
  gfr_out <- broom.mixed::tidy(gfr_mod)
  # Remove extra covariates
  gfr_out <- gfr_out[c(2:9),]
  gfr_out <- as.data.table(gfr_out)
  gfr_out[, c('iteration', 'dates', 'type', 'numbelk') := .(rep(WS_data$iteration[1],nrow(gfr_out)), rep(input_folder, nrow(gfr_out)), rep('gfr', nrow(gfr_out)), rep(length(unique(WS_data$elkyear))))]
  
  ############################################################################################
  ############################################################################################
  ## PART 3: COMPARE MODELS ##
  ############################################################################################
  ############################################################################################
  ## Loops through all elk-years from OOSData with standard errors less than 30
  
  # Initiate data frame to collect R-squared values
  correlation_data <- data.table()
  
  for (i in unique(OOS_data$elkyear)){
    
    sub_dat <- dat[elkyear==i]
    
    # Create spatial points data frame and mcp
    sp_sub <- SpatialPoints(coords=sub_dat[,c('X','Y')], proj4string =CRS("+init=epsg:26914"))
    mcp_sub <- mcp(sp_sub)
    
    # Extract the elk year from each predRSF containing covariates and average HR cover
    OOS_sub <- OOS_data[elkyear==i]
    WS_sub <- WS_data[elkyear==i]
    
    # Skip iteration if elk-year is not in the within-sample data
    if(nrow(WS_sub)==0){
      next
    }
    
    mixedwood_crop <- mask(mixedwood, mcp_sub)
    road_crop <- mask(distToRoad, mcp_sub)
    road_crop <- resample(road_crop, mixedwood_crop, method='bilinear')
    
    # # Log distance rasters
    # log_road_crop <- log(1+road_crop)
    # log_road_crop <- na.omit(log_road_crop)
    # 
    # # Create raster stack
    # predstack<-stack(mixedwood_crop, log_road_crop)
    # names(predstack)<-c("mixedwood", "road")
    
    # Extract raster means for FR
    mean_mixedwood <- mean(mixedwood_crop@data@values, na.rm=T)
    mean_road <- mean(road_crop@data@values, na.rm=T)
    
    # Create new data for predictions
    
    new_dat <- data.table(mixedwood=na.omit(mixedwood_crop@data@values), 
                          road=na.omit(road_crop@data@values),
                          mean_mixedwood,
                          mean_road,
                          weight=1,
                          elkyear=NA)
    ########################################################################################################
    ######### INDIVIDUAL RSFS ###############
    ##############################################
    
    ###### OOS RSF ---
    id_OOS_RSF <- glm(reformulate(base_covars, response = 'sample'),
                      family = binomial,
                      data = OOS_sub)
    id_OOS_out <- as.data.table(broom.mixed::tidy(id_OOS_RSF))
    # Stop if the model doesn't converge or not all covariates are modelled
    if(any(id_OOS_out$std.error>30, na.rm=T) | any(is.na(id_OOS_out$std.error))){
      next
    }
    # Remove intercept
    id_OOS_out <- id_OOS_out[!c(term=='(Intercept)')]
    
    ###### WS RSF ---
    id_WS_RSF <- glm(reformulate(base_covars, response = 'sample'),
                     family = binomial,
                     data = WS_sub)
    id_WS_out <- as.data.table(broom.mixed::tidy(id_WS_RSF))
    # Stop if the model doesn't converge or not all covariates are modelled
    if(any(id_WS_out$std.error>30, na.rm=T) | any(is.na(id_WS_out$std.error))){
      next
    }
    # Remove intercept
    id_WS_out <- id_WS_out[!c(term=='(Intercept)')]
    
    ###############################################################################
    #################
    ## PREDICTIONS ######
    
    # Individual level out-of-sample model
    pred_WS <- 
      id_WS_out[term=='road']$estimate*predstack$road +
      id_WS_out[term=='mixedwood']$estimate*predstack$mixedwood
    
    # Individual level within-sample model
    pred_OOS <- 
      id_OOS_out[term=='road']$estimate*predstack$road +
      id_OOS_out[term=='mixedwood']$estimate*predstack$mixedwood
    
    # No FR
    pred_no_FR <- 
      no_out[term=='road']$estimate*predstack$road +
      no_out[term=='mixedwood']$estimate*predstack$mixedwood
    
    # Ranef
    # pred_ranef <- 
    #   ranef_out[term=='road']$estimate*predstack$road +
    #   ranef_out[term=='mixedwood']$estimate*predstack$mixedwood
    
    pred_ranef <- predict(ranef_mod, new_dat)
    
    # GFR Model 1 population level generalized functional response model
    # FRij = raster*(Bi + Bi^2*hi + Bij*hj)
    pred_gfr <-
      predstack$road*as.numeric((gfr_out[term=='road']$estimate + 
                                   (gfr_out[term=='road:mean_road']$estimate*mean_road) +
                                   (gfr_out[term=='road:mean_mixedwood']$estimate*mean_mixedwood))) +
      predstack$mixedwood*as.numeric((gfr_out[term=='mixedwood']$estimate + 
                                        (gfr_out[term=='mixedwood:mean_road']$estimate*mean_road) +
                                        (gfr_out[term=='mixedwood:mean_mixedwood']$estimate*mean_mixedwood)))
    
    
    # Run LMs for each model in comparison to the out-of-sample individual model
    for(model in c('WS', 'no_FR', 'ranef', 'gfr')){
      model_sub <- get(paste('pred', model, sep='_'))
      model_lm <- lm(model_sub@data@values~pred_OOS@data@values)
      corr_lm <- as.numeric(summary(model_lm)$adj.r.squared)
      assign(paste(model, 'corr', sep='_'), corr_lm)
    }
    
    
    # Bind results
    corrs <- data.table(elkyear = i, year = year, npoints_OOS = OOS_sub[1]$npoints, npoints_WS = WS_sub[1]$npoints,
                        individual = WS_corr, no_FR = no_FR_corr, ranef = ranef_corr, gfr = gfr_corr)
    
    correlation_data <- rbind(correlation_data, corrs)
  }
  
  
  ############################################################################################
  ############################################################################################
  ## PART 4: SAVE ITERATION RESULTS ##
  ############################################################################################
  ############################################################################################
  ## Save each model output in the 'results/Prediction_outputs/' folder
  ## The file name should correspond to the name of input folder:
  ## Format should be: 'results/Prediction_outputs/MM-DD_MM-DD-predictions.csv'
  
  dir.create(path = paste0("results/prediction_outputs/", input_folder, sep='/'))
  
  # Set output folder from input folder
  
  saveRDS(correlation_data, file = paste0("results/prediction_outputs/", paste(input_folder, "correlations.rds", sep = "/")))
  
  no_FR_outputs <- rbind(no_FR_outputs, no_out)
  ranef_outputs <- rbind(ranef_outputs, ranef_out)
  gfr_outputs <- rbind(gfr_outputs, gfr_out)
  
  # Saves a temporary copy of the covariate outputs
  
  saveRDS(no_FR_outputs, 'results/RSF_outputs/temp_no_FR.rds')
  saveRDS(ranef_outputs, 'results/RSF_outputs/temp_ranef.rds')
  saveRDS(gfr_outputs, 'results/RSF_outputs/temp_gfr.rds')
  
}

# Save final copy of covariate outputs

saveRDS(no_FR_outputs, 'results/RSF_outputs/no_FR_covariates.rds')
saveRDS(ranef_outputs, 'results/RSF_outputs/ranef_covariates.rds')
saveRDS(gfr_outputs, 'results/RSF_outputs/gfr_covariates.rds')


############################### END LOOP ###################################################
