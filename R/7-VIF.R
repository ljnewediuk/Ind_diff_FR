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
libs <- c('data.table', 'lme4', 'car')
lapply(libs, require, character.only = TRUE)

### Load data from RSF outputs folder ----

# List of all folders
folder_list <- list.files(path="input/RSF_data/")

### Load data ----

# Spatial data
dat <- readRDS('input/dat_cleaned.rds')


VIF_table <- data.table()

for (input_folder in folder_list){
  
  # Data for population models and within-sample individual models
  WS_data <- readRDS(file = paste0("input/RSF_data/", paste(input_folder, "WS_data.rds", sep = "/")))
  
  
  ############################################################################################
  ############################################################################################
  ## PART 2: RUN POPULATION LEVEL MODELS ##
  ############################################################################################
  ############################################################################################
  
  base_covars=c('road', 'mixedwood', 'coniferous')

  print(WS_data$iteration[1])
  
  ###### No FR ---
  mod <- glmer(reformulate(c(base_covars, '( 1 | elkyear)'), response = 'sample'),
                 family = binomial,
                 data = WS_data)
  
  # Remove extra covariates
  VIF_mod <- vif(mod)
  
  print(VIF_mod)
  
  VIF_covariates <- data.table(iteration=rep(WS_data$iteration[1],3), covariate=base_covars, VIF=as.numeric(VIF_mod))
  
  VIF_table <- rbind(VIF_table, VIF_covariates)

}  
  

############################### END LOOP ###################################################
