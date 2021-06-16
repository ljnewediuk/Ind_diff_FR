############################################################
############################################################
## 2 - RSF MODELS AND CORRELATIONS WITH INDIVIDUAL MODELS ##
############################################################
############################################################
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
done_list <- list.files(path='results/RSF_models/')
# List of all folders
folder_list <- list.files(path="input/RSF_data/")
# Subtract completed folders from all folders
folder_list <- folder_list[!folder_list %in% done_list]

############################################################################################
############################################################################################
## PART 1: COMPILE THE DATA ##
############################################################################################
############################################################################################
## Pull data from folders based on folder list

# Specify existing data table to compile results (if left off at a previous iteration)
gfr_outputs <- readRDS('results/RSF_outputs/temp_gfr.rds')
ranef_outputs <- readRDS('results/RSF_outputs/temp_ranef.rds')

# Or specify blank data table to compile results
# 
# gfr_outputs <- data.table()
# ranef_outputs <- data.table()

# specify input folder for iteration 

for (input_folder in folder_list){
  
  # Start input folders to collect results
  dir.create(path = paste("results/RSF_models_population/", input_folder, sep='/'))
  dir.create(path = paste("results/RSF_models_individuals/", input_folder, sep='/'))
  
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
  base_covars=c('I(log(road+1))', 'mixedwood')
  ranef_covars=c('(1 | elkyear)', '(0 + I(log(road+1)) + mixedwood | elkyear)')
  gfr_covars=c('I(log(road+1))*I(log(mean_road+1))', 'mixedwood*mean_mixedwood', 'I(log(road+1))*mean_mixedwood', 'mixedwood*I(log(mean_road+1))')
  
  # Use a weighted likelihood of 1000 for available points (Muff method)
  WS_data[, 'weight' := .(1000^(1-sample))]
  
  # Label the iteration before modelling
  print(input_folder)

  ###### Ranef model ---
  ranef <- suppressWarnings(glmmTMB(reformulate(c(base_covars, ranef_covars), response = 'sample'),
                   family = binomial(),
                   map=list(theta=factor(c(NA,1:3))),
                   data = WS_data, doFit=F, weights=weight))
  # Fix the sd of the first random term (1 | elkyear) to 10^6
  ranef$parameters$theta[1] = log(1e3)
  ranef_mod <- glmmTMB::fitTMB(ranef)
  # Tidy
  ranef_out <- broom.mixed::tidy(ranef_mod)
  # Remove extra covariates
  ranef_out <- ranef_out[c(2:3),]
  ranef_out <- as.data.table(ranef_out)
  ranef_out[, c('iteration', 'dates', 'type', 'numbelk') := .(rep(WS_data$iteration[1],nrow(ranef_out)), rep(input_folder, nrow(ranef_out)), rep('ranef', nrow(ranef_out)), rep(length(unique(WS_data$elkyear))))]
  
  ###### GFR model ---
  gfr <- suppressWarnings(glmmTMB(reformulate(c(base_covars, gfr_covars, ranef_covars), response = 'sample'),
                 family = binomial(),
                 map=list(theta=factor(c(NA,1:3))),
                 data = WS_data, doFit=F, weights=weight))
  # Fix the sd of the first random term (1 | elkyear) to 10^6
  gfr$parameters$theta[1] = log(1e3)
  gfr_mod <- glmmTMB::fitTMB(gfr)
  # Tidy
  gfr_out <- broom.mixed::tidy(gfr_mod)
  # Remove extra covariates
  gfr_out <- gfr_out[c(2:9),]
  gfr_out <- as.data.table(gfr_out)
  gfr_out[, c('iteration', 'dates', 'type', 'numbelk') := .(rep(WS_data$iteration[1],nrow(gfr_out)), rep(input_folder, nrow(gfr_out)), rep('gfr', nrow(gfr_out)), rep(length(unique(WS_data$elkyear))))]
  
  for (i in unique(OOS_data$elkyear)){
    
    # Extract the elk year from each predRSF containing covariates and average HR cover
    OOS_sub <- OOS_data[elkyear==i]
    WS_sub <- WS_data[elkyear==i]
    
    # Skip iteration if elk-year is not in the within-sample data
    if(nrow(WS_sub)==0){
      next
    }
    
  ###### OOS RSF ---
  id_OOS_RSF <- glm(reformulate(base_covars, response = 'sample'),
                    family = binomial,
                    data = OOS_sub)
  id_OOS_out <- as.data.table(broom.mixed::tidy(id_OOS_RSF))
  # Stop if the model doesn't converge or not all covariates are modelled
  if(any(id_OOS_out$std.error>30, na.rm=T) | any(is.na(id_OOS_out$std.error))){
    next
  } else {
    # Remove intercept
    id_OOS_out <- id_OOS_out[!c(term=='(Intercept)')]
    # Save model
    dir.create(path = paste("results/RSF_models_individuals/", paste(input_folder, paste( i, '/', sep=''), sep='/'), sep=''))
    saveRDS(id_OOS_RSF, paste("results/RSF_models_individuals/", paste(input_folder, paste( i, 'OOS_mod.rds', sep='/'), sep='/'), sep=''))
  }
  
  
  ###### WS RSF ---
  id_WS_RSF <- glm(reformulate(base_covars, response = 'sample'),
                   family = binomial,
                   data = WS_sub)
  id_WS_out <- as.data.table(broom.mixed::tidy(id_WS_RSF))
  # Stop if the model doesn't converge or not all covariates are modelled
  if(all(id_WS_out$std.error<30, na.rm=T) & all(!is.na(id_WS_out$std.error)) & dir.exists(paste("results/RSF_models_individuals/", paste(input_folder, paste( i, '/', sep=''), sep='/'), sep=''))){
    # Save model
    saveRDS(id_WS_RSF, paste("results/RSF_models_individuals/", paste(input_folder, paste( i, 'WS_mod.rds', sep='/'), sep='/'), sep=''))
  } else {
    next
  }
  # Remove intercept
  id_WS_out <- id_WS_out[!c(term=='(Intercept)')]
  
  }  
  ############################################################################################
  ############################################################################################
  ## PART 4: SAVE ITERATION RESULTS ##
  ############################################################################################
  ############################################################################################
  ## Save each model output in the 'results/Prediction_outputs/' folder
  ## The file name should correspond to the name of input folder:
  ## Format should be: 'results/Prediction_outputs/MM-DD_MM-DD-predictions.csv'
  
  saveRDS(ranef_mod, paste("results/RSF_models_population/", paste(input_folder, 'ranef_mod.rds', sep='/'), sep=''))
  saveRDS(gfr_mod, paste("results/RSF_models_population/", paste(input_folder, 'gfr_mod.rds', sep='/'), sep=''))
  
  ranef_outputs <- rbind(ranef_outputs, ranef_out)
  gfr_outputs <- rbind(gfr_outputs, gfr_out)
  
  # Saves a temporary copy of the covariate outputs
  
  saveRDS(ranef_outputs, 'results/RSF_outputs/temp_ranef.rds')
  saveRDS(gfr_outputs, 'results/RSF_outputs/temp_gfr.rds')
  
}

# Save final copy of covariate outputs

saveRDS(ranef_outputs, 'results/RSF_outputs/ranef_covariates.rds')
saveRDS(gfr_outputs, 'results/RSF_outputs/gfr_covariates.rds')


############################### END LOOP ###################################################
