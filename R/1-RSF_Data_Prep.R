############################################################################################
############################################################################################
## 1 - INDIVIDUAL RSFS AND DATA PREP FOR POPULATION RSFS 
############################################################################################
############################################################################################
## Returns:
## 1) Covariate data for use in population level RSF models:
##    a) Within-sample
## 2) Individual model outputs with 3 different covariate sets:
##    a) All: NDVI difference, seasonal NDVI, coniferous forest, mixedwood forest, roads
##    b) Forage-related: NDVI difference, seasonal NDVI, coniferous forest, mixedwood forest
##    c) Habitat-related: coniferous forest, mixedwood forest, roads

### Packages ----
libs <- c('data.table', 'rgdal', 'sp', 'adehabitatHR', 'raster', 'lme4')
lapply(libs, require, character.only = TRUE)

### Load data ----

# A) Elk data
dat <- readRDS('input/elkdatbyEYs.rds')

# Add elkyear column
dat[, elkyear := .GRP,
    by = c('EarTag', 'intyear')]

# Screen out elkyears with fewer than 150 points
fewEY <- vector()
for(i in unique(dat$elkyear)){
  subdat <- dat[elkyear==i]
  Nsub <- subdat[, .N, by='elkyear']
  if(Nsub$N < 150){
    fewEY <- c(fewEY, i)
  }else{
    next
  }
}
# Remove
dat <- dat[!elkyear %in% fewEY]

# Exclude elk from Duck Mountain population in 2006/07
dat <- dat[!c(intyear==2006 | intyear==2007)]

# Save data frame for use in predictions
saveRDS(dat, 'input/dat_cleaned.rds')

# covariate rasters
# landcover
mixedwood <- raster("input/rasters/mixedwood_raster.tif")
coniferous <- raster("input/rasters/coniferous_raster.tif")

# NDVI seasonal
for(year in c('2004', '2005', '2008',
              '2009', '2011', '2012', '2015', '2016')){
  assign(paste('NDVIseas', year, sep='_'), raster(paste('input/rasters/NDVI-seasonal-', paste(year, '-reproj.tif', sep=''), sep='')))
}

# NDVI difference
for(year in c('2004', '2005', '2008',
              '2009', '2011', '2012', '2015', '2016')){
  assign(paste('NDVIdiff', year, sep='_'), raster(paste('input/rasters/NDVI-difference-', paste(year, '-reproj.tif', sep=''), sep='')))
}

# distance to road
distToRoad <- raster("input/rasters/distToRoad.tif")


############################################################################################
############################################################################################
## PART 1: SUBSET DATA INTO SAMPLE AND WITHHELD DATA USING SPECIFIED TEMPORAL RANGE ##
############################################################################################
############################################################################################
## Using a moving window, subset data into 30-day intervals for each elk beginning with Dec. 1 and
## ending March 1 (i.e. Dec. 1-30, Dec. 2-31, Dec. 3-Jan. 1,... March 1-30)
## This creates n=120x2 data.tables: the 30-day interval becomes the withheld data (out-of-sample),
## and the inverse (remaining 90days) becomes the within-sample data

### Subset dates for analysis ----

# Specify dates for the iteration
dates1 <- seq(as.Date('2020-11-01'), as.Date('2021-03-31'), by='day')
dates1 <- format(dates1, format='%m-%d')

dates2 <- c(seq(as.Date('2020-11-30'), as.Date('2021-03-31'), by='day'), seq(as.Date('2020-11-01'), as.Date('2020-11-29'), by='day'))
dates2 <- format(dates2, format='%m-%d')
# Create columns for month/day
dat[, 'MonthDay' := .(format(dat$DateTime, format="%m-%d"))]

# Initialize data table to collect RSF results

all_RSFs <- data.table()
forage_RSFs <- data.table()
hab_RSFs <- data.table()

# Subset observed data frame to new date range for:
# out-of-sample data (withheld)

for(day in 1:length(dates1)){
if(day <= 32 | day >= 62){
  # out of sample data
  datOOS <- dat[MonthDay >= dates1[day] & MonthDay <= dates2[day] ,]
  # within-sample data
  datWSD <- dat[MonthDay < dates1[day] | MonthDay > dates2[day] ,]
} else {
  # out of sample data
  datOOS <- dat[MonthDay >= dates1[day] | MonthDay <= dates2[day] ,]
  # within-sample data
  datWSD <- dat[MonthDay < dates1[day] & MonthDay > dates2[day] ,]
}

  
# Create output folder to collect data
outputfolder <- paste(dates1[day], dates2[day], sep="_")
  
############################################################################################
############################################################################################
## PART 2: CREATE MCP HOME RANGES FOR WITHIN-SAMPLE AND OUT-OF-SAMPLE DATA ##
############################################################################################
############################################################################################
## Creates a separate MCP for each elk-year based on dates provided
## Then sort them into lists by corresponding year
## The home ranges will be used to extract raster data on the fly

# Start loop
for(j in c('OOS', 'WSD')){
  # Initialize data table and list for mcps
  used <- data.table()
  mcp_list <- list()
  if(j=='OOS'){
    dat_split <- datOOS
  }else{
    dat_split=datWSD
  }
  eys <- unique(dat_split$elkyear)
# Cycle through elk years
for (i in eys) {
  sub_dat <- dat_split[elkyear==i]
  year_sub <- sub_dat$intyear[1]
  # Exclude elk-years with fewer than 5 points (mcp function won't work)
  if(nrow(sub_dat) < 5){
    next
  }
  # Get NDVI rasters corresponding to the elkyear
  NDVI_seas_sub <- get(paste('NDVIseas_', year_sub, sep=''))
  NDVI_diff_sub <- get(paste('NDVIdiff_', year_sub, sep=''))
  # Create spatial points data frame and mcp
  sp_sub <- SpatialPoints(coords=sub_dat[,c('X','Y')], proj4string =CRS("+init=epsg:26914"))
  mcp_sub <- mcp(sp_sub)
  # Rename the mcp id to match elkyear, add year attribute
  mcp_sub@data$id <- i
  mcp_sub@data$year <- year_sub
  mcp_sub@data$npoints <- nrow(sub_dat)
  # Extract raster values
  NDVI_seas_pts <- extract(NDVI_seas_sub, sp_sub)
  NDVI_diff_pts <- extract(NDVI_diff_sub, sp_sub)
  coniferous_pts <- extract(coniferous, sp_sub)
  mixedwood_pts <- extract(mixedwood, sp_sub)
  road_pts <- extract(distToRoad, sp_sub)
  # Compile data table
  sub_used <- data.table(elkyear = rep(sub_dat$elkyear[1], length(mixedwood_pts)),
                         year = rep(year_sub, length(mixedwood_pts)), NDVI_seas = NDVI_seas_pts, NDVI_diff = NDVI_diff_pts,
                         mixedwood = mixedwood_pts, coniferous = coniferous_pts, road = road_pts, npoints = nrow(sub_dat), sample = rep(1, length(mixedwood_pts)))
  used <- rbind(used, sub_used)
  mcp_list <- c(mcp_list, mcp_sub)
}
  assign(paste('used', j, sep=''), used)
  assign(paste('mcp_list', j, sep=''), mcp_list)
}


############################################################################################
############################################################################################
## PART 3: EXTRACT AVAILABLE DATA ##
############################################################################################
############################################################################################
## Creates a separate MCP for each elk-year based on dates provided
## Then sort them into lists by corresponding year
## The home ranges will be used to extract raster data on the fly

skips <- vector()

for(j in c('OOS', 'WSD')){
    
  # Initialize data tables
  avail <- data.table()
  HRmeans <- data.table()
  if(j=='OOS'){
    mcp_list <- mcp_listOOS
  }else{
    mcp_list <- mcp_listWSD
  }
for(i in mcp_list){
  
  tryCatch({
    print(i@data$id)
    
  # Sample random points
  sp_sub<-spsample(i,n=i@data$npoints*10,"random")
  year_sub <- i@data$year
  # Get NDVI rasters corresponding to the elkyear
  NDVI_seas_sub <- get(paste('NDVIseas_', year_sub, sep=''))
  NDVI_diff_sub <- get(paste('NDVIdiff_', year_sub, sep=''))
  # Extract raster values at points
  NDVI_seas_pts <- extract(NDVI_seas_sub, sp_sub)
  NDVI_diff_pts <- extract(NDVI_diff_sub, sp_sub)
  coniferous_pts <- extract(coniferous, sp_sub)
  mixedwood_pts <- extract(mixedwood, sp_sub)
  road_pts <- extract(distToRoad, sp_sub)
  # Extract raster value means
  NDVI_seas_mean <- extract(NDVI_seas_sub, i, method='simple', fun=mean, na.rm=TRUE)
  NDVI_diff_mean <- extract(NDVI_diff_sub, i, method='simple', fun=mean, na.rm=TRUE)
  coniferous_mean <- extract(coniferous, i, method='simple', fun=mean, na.rm=TRUE)
  mixedwood_mean <- extract(mixedwood, i, method='simple', fun=mean, na.rm=TRUE)
  road_mean <- extract(distToRoad, i, method='simple', fun=mean, na.rm=TRUE)
  
  
  # Skip any elk-years where all NDVI=NA and add to a list
  if(any(is.nan(c(NDVI_seas_mean, NDVI_diff_mean, coniferous_mean, mixedwood_mean, road_mean,
                  NDVI_seas_pts, NDVI_diff_pts, coniferous_pts, mixedwood_pts, road_pts)))==TRUE){
    skips <- c(skips, i@data$id) 
    next
  }
  
  # Compile data table of means
  sub_means <- data.table(elkyear = i@data$id, year = i@data$year,
                          mean_coniferous = coniferous_mean, mean_mixedwood = mixedwood_mean, mean_road = road_mean,
                          mean_NDVI_seas = NDVI_seas_mean, mean_NDVI_diff = NDVI_diff_mean)
  HRmeans <- rbind(HRmeans, sub_means)
  
  # Compile data table of available points
  sub_avail <- data.table(elkyear = rep(i@data$id, i@data$npoints*10), year = rep(i@data$year, i@data$npoints*10), NDVI_seas = NDVI_seas_pts, NDVI_diff = NDVI_diff_pts,
                          mixedwood = mixedwood_pts, coniferous = coniferous_pts, road = road_pts, npoints = i@data$npoints, sample = rep(0, i@data$npoints*10))
  avail <- rbind(avail, sub_avail)
  
  }, warning=function(w){cat("WARNING:", conditionMessage(w), "\n")})}
  
  assign(paste('avail', j, sep=''), avail)
  assign(paste('HRmeans', j, sep=''), HRmeans)
  # assign(paste('skips', j, sep=''), skips)
  
}

# Rename columns
colnames(HRmeansOOS) <- c('elkyear', 'year', 'mean_coniferous', 'mean_mixedwood', 'mean_road', 'mean_NDVI_seas', 'mean_NDVI_diff')
colnames(HRmeansWSD) <- c('elkyear', 'year', 'mean_coniferous', 'mean_mixedwood', 'mean_road', 'mean_NDVI_seas', 'mean_NDVI_diff')

# Remove all elkyears from the used data if NDVI data was all NA in home range
if(length(skips) > 0){
  usedOOS <- usedOOS[!elkyear %in% skips]
  usedWSD <- usedWSD[!elkyear %in% skips]
  }else{
    usedOOS <- usedOOS
    usedWSD <- usedWSD
}

# Combine used and available data
RSF_datOOS <- rbind(usedOOS, availOOS)
RSF_datWSD <- rbind(usedWSD, availWSD)

# Add means data to used available data
RSF_datOOS <- merge(RSF_datOOS, HRmeansOOS, by=c('year', 'elkyear'))
RSF_datWSD <- merge(RSF_datWSD, HRmeansWSD, by=c('year', 'elkyear'))

# Log transform distances, add iteration for RSFs
RSF_datOOS[, c('mean_road', 'road', 'iteration') := .(log(RSF_datOOS$mean_road+1), log(RSF_datOOS$road+1), rep(day, nrow(RSF_datOOS)))]
RSF_datWSD[, c('mean_road', 'road', 'iteration') := .(log(RSF_datWSD$mean_road+1), log(RSF_datWSD$road+1), rep(day, nrow(RSF_datWSD)))]

############################################################################################
############################################################################################
## PART 5: RUN RESOURCE SELECTION FUNCTIONS ##
############################################################################################
############################################################################################

## --- Using within-sample data (i.e. predictive models):
## METHOD 1: Population RSF using ID as random intercept (no functional response)
## METHOD 2: Population RSF using ID as random intercept and random slopes for
##           functional response
## METHOD 3: Population RSFs using ID as random intercept and interaction with
##           mean home range proportion as functional response (GFR)
## METHOD 4: Individual RSF

## --- Using out-of-sample data (i.e. to be predicted):
## METHOD 4: Individual RSF

all_covars <- data.table(model = rep('all',5), covars=c('NDVI_diff', 'NDVI_seas', 'road', 'mixedwood', 'coniferous'))
hab_covars <- data.table(model = rep('hab',3), covars=c('road', 'mixedwood', 'coniferous'))
forage_covars <- data.table(model = rep('forage',4), covars=c('NDVI_diff', 'NDVI_seas', 'mixedwood', 'coniferous'))
covars_list <- rbind(all_covars, hab_covars, forage_covars)

for(k in unique(covars_list$model)){

##################################################################################
##### METHOD 3: WITHIN-SAMPLE Individual RSF using available points from within HR

covars_sub <- covars_list[model==k]
covars <- covars_sub$covars
names_covars <- covars_sub$model[1]
id_WSD_out <- data.table()

for(i in unique(RSF_datWSD$elkyear)){
  sub_RSF_dat <- RSF_datWSD[elkyear==i]
  id_RSF <- glm(reformulate(covars, response = 'sample'),
             family = binomial,
             data = sub_RSF_dat)
  id_out <- as.data.table(broom::tidy(id_RSF))
  # Stop if the model doesn't converge
  if(any(id_out$std.error>30)){
    next
  }
  # Remove intercept and add elkyear
  id_out <- id_out[!c(term=='(Intercept)')]
  id_out[, c('elkyear', 'numb_pts') := .(rep(i, nrow(id_out)), rep(sub_RSF_dat[1]$npoints, nrow(id_out)))]
  # Bind
  id_WSD_out <- rbind(id_WSD_out, id_out)
}

id_WSD_out <- merge(id_WSD_out, HRmeansWSD, by='elkyear')
id_WSD_out[, c('iteration', 'dates', 'type') := .(rep(day, nrow(id_WSD_out)), rep(outputfolder, nrow(id_WSD_out)), rep('WSD', nrow(id_WSD_out)))]

###############################################################################
## METHOD 4: OUT-OF-SAMPLE Individual RSF using available points from within HR

id_OOS_out <- data.table()

for(i in unique(RSF_datOOS$elkyear)){
  sub_RSF_dat <- RSF_datOOS[elkyear==i]
  id_RSF <- glm(reformulate(covars, response = 'sample'),
                family = binomial,
                data = sub_RSF_dat)
  id_out <- as.data.table(broom::tidy(id_RSF))
  # Stop if the model doesn't converge
  if(any(id_out$std.error>30)){
    next
  }
  # Remove intercept and add elkyear
  id_out <- id_out[!c(term=='(Intercept)')]
  id_out[, c('elkyear', 'numb_pts') := .(rep(i, nrow(id_out)), rep(sub_RSF_dat[1]$npoints, nrow(id_out)))]
  # Bind
  id_OOS_out <- rbind(id_OOS_out, id_out)
}

id_OOS_out <- merge(id_OOS_out, HRmeansOOS, by='elkyear')
id_OOS_out[, c('iteration', 'dates', 'type') := .(rep(day, nrow(id_OOS_out)), rep(outputfolder, nrow(id_OOS_out)), rep('OOS', nrow(id_OOS_out)))]

## Combine all models into a data.table
outputs <- rbind(id_WSD_out, id_OOS_out)

assign(paste(names_covars, 'output', sep='_'), outputs)

}

############################################################################################
############################################################################################
## PART 7: SAVE ITERATION RESULTS ##
############################################################################################
############################################################################################

all_RSFs <- rbind(all_RSFs, all_output)
forage_RSFs <- rbind(forage_RSFs, forage_output)
hab_RSFs <- rbind(hab_RSFs, hab_output)

## Save each model output in a separate folder corresponding to the date range
## Date range format should be: 'results/RSF_outputs/MM-DD_MM-DD'

# Set output folder for iteration dates
dir.create(path = paste0("input/RSF_data/", outputfolder, sep='/'))

####################
# Save model outputs
saveRDS(RSF_datWSD, file = paste0("input/RSF_data/", paste(outputfolder, "WS_data.rds", sep = "/")))
saveRDS(RSF_datOOS, file = paste0("input/RSF_data/", paste(outputfolder, "OOS_data.rds", sep = "/")))

###########

}

saveRDS(all_RSFs, 'results/RSF_outputs/id_all_covariates.rds')
saveRDS(forage_RSFs, 'results/RSF_outputs/id_forage_covariates.rds')
saveRDS(hab_RSFs, 'results/RSF_outputs/id_hab_covariates.rds')

