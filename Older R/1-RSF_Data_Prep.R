#############################################################
#############################################################
## 1 - INDIVIDUAL RSFS AND DATA PREP FOR POPULATION RSFS   ##
#############################################################
#############################################################
## Returns:
## 1) Data for use in RSFs from each date range:
##      i) Out of sample (OOS) data
##     ii) Within sample (WS) data
## 2) Individual model outputs (mw_rd_RSFs) for summary
##
## WARNING: This script takes several hours to run!

### Packages ----

libs <- c('data.table', 'rgdal', 'sp', 'adehabitatHR', 'raster', 'lme4', 
          'lubridate')
lapply(libs, require, character.only = TRUE)

### Load data: Elk data ----

dat <- readRDS('input/elkdatbyEYs.rds')
# Add elkyear column
dat[, elkyear := .GRP, by = c('EarTag', 'intyear')]
# Screen out elkyears with fewer than 150 points
fewEY <- vector()
for(i in unique(dat$elkyear)) {
  subdat <- dat[elkyear==i]
  Nsub <- subdat[, .N, by='elkyear']
  if(Nsub$N < 150) {
    fewEY <- c(fewEY, i)
  } else {
    next
  }
}
# Remove the elk
dat <- dat[!elkyear %in% fewEY]
# Exclude elk from Duck Mountain population collared in 2006/07
dat <- dat[!c(intyear==2006 | intyear==2007)]
# Save data frame for use in predictions script
saveRDS(dat, 'input/dat_cleaned.rds')

### Load data: Spatial data ----

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
# Remove raster data
system("rm -rf ~/Documents/R-Projects/individual_fr/rasters/")

### Create list of dates for analysis ----

# Remove dates outside of December and January
dat <- dat[Month %in% c(1,12)]
# Add Julian day column
dat[, 'Jday' := .(yday(DateTime))]
# Specify dates for the iteration
dates1 <- c(seq(as.Date('2020-12-01'), as.Date('2020-12-31'), by='day'), 
            as.Date('2021-01-01'), as.Date('2021-01-02'))
dates1 <- format(dates1, format='%m-%d')
dates2 <- c(seq(as.Date('2020-12-30'), as.Date('2021-01-31'), by='day'), 
            seq(as.Date('2020-12-01'), as.Date('2020-12-31'), by='day'), 
            as.Date('2021-01-30'), as.Date('2021-01-31'))
dates2 <- format(dates2, format='%m-%d')

# Specify existing data table to compile results (if left off at some iteration)
mw_rd_RSFs <- readRDS('results/RSF_outputs/temp_mw_rd_RSFs.rds')
# OR initialize data table to collect RSF results
# mw_rd_RSFs <- data.table()

#########################################################################
##                            LOOP                                     ##
#########################################################################

### Start looping over dates to extract data ----

### 1: Set up folders ----
for(day in 1:length(dates1)) {
  if(day < 32 & day > 2) {
    # Specify correct order to place dates before/after Dec. 31
    dat_OOS <- dat[Jday %in% c(seq(yday(as.Date(dates1[day], format='%m-%d')), 
                                   365, 1), seq(1, yday(as.Date(dates2[day], 
                                                                format='%m-%d')), 1))]
    dat_WSD <- dat[!Jday %in% c(seq(yday(as.Date(dates1[day], format='%m-%d')), 
                                    365, 1), seq(1, yday(as.Date(dates2[day], 
                                                                 format='%m-%d')), 1))]
  } else {
    dat_OOS <- dat[Jday %in% c(seq(yday(as.Date(dates1[day], format='%m-%d')), 
                                   yday(as.Date(dates2[day], format='%m-%d')), 1))]
    dat_WSD <- dat[!Jday %in% c(seq(yday(as.Date(dates1[day], format='%m-%d')), 
                                    yday(as.Date(dates2[day], format='%m-%d')), 1))]
  }
  # Create output folder to collect data
  outputfolder <- paste(dates1[day], dates2[day], sep='_')
  # Create folder to collect MCPs
  dir.create(path = paste("input/MCPs/", paste(outputfolder, '/', sep=''), sep=''))
  # Print current output folder
  print(outputfolder)
  # Check whether iteration has already been completed
  done_list <- list.files(path='input/RSF_data/')
  # If complete, move on to next iteration
  if(outputfolder %in% done_list) {
    print('Already completed')
    next
  }
  
  ### 2: Create MCPs for extracting the data ----
  
  print('Creating home ranges...')
  
  # Start loop over both OOS data and WS data
  for(j in c('OOS', 'WSD')) {
    # Initialize data table and list for mcps
    used <- data.table()
    mcp_list <- list()
    # Get the correct data type (OOS or WS)
    dat_split <- get(paste('dat', j, sep='_'))
    # Cycle through elk years
    for (i in unique(dat_split$elkyear)) {
      sub_dat <- dat_split[elkyear==i]
      year_sub <- sub_dat$intyear[1]
      # Exclude elk-years with fewer than 5 points ('mcp' function won't work)
      if(nrow(sub_dat) < 5) {
        next
      }
      # Create spatial points data frame and mcp
      sp_sub <- SpatialPoints(coords=sub_dat[,c('X','Y')], 
                              proj4string =CRS("+init=epsg:26914"))
      mcp_sub <- mcp(sp_sub)
      # Rename the mcp id to match elkyear, add year attribute
      mcp_sub@data$id <- i
      mcp_sub@data$year <- year_sub
      mcp_sub@data$npoints <- nrow(sub_dat)
      # Extract raster values
      mixedwood_pts <- extract(mixedwood, sp_sub)
      road_pts <- extract(distToRoad, sp_sub)
      # Compile data table
      sub_used <- data.table(elkyear = sub_dat$elkyear[1], year = year_sub, 
                             mixedwood = mixedwood_pts, road = road_pts, 
                             npoints = nrow(sub_dat), sample = 1)
      used <- rbind(used, sub_used)
      mcp_list <- c(mcp_list, mcp_sub)
      # Save MCPs
      dir.create(path = paste("input/MCPs/", paste(outputfolder, 
                                                   paste(i, '/', sep=''), sep='/'), sep=''))
      saveRDS(mcp_sub, paste("input/MCPs/", paste(outputfolder, 
                                                  paste(i, paste(j, 'mcp.rds', sep='_'), sep='/'), 
                                                  sep='/'), sep=''))
    }
    assign(paste('used', j, sep=''), used)
    assign(paste('mcp_list', j, sep='_'), mcp_list)
  }
  
  ### 3: Extract data ----
  
  print('Extracting data...')
  
  # Create an object to store and handle missing data
  skips <- vector()
  # Start loop over both OOS data and WS data
  for(j in c('OOS', 'WSD')) {
    # Initialize data tables
    avail <- data.table()
    HRmeans <- data.table()
    # Get the right list of mcps
    mcp_list <- get(paste('mcp_list', j, sep='_'))
    # Loop over individuals
    for(i in mcp_list) {
      # tryCatch({
      print(i@data$id)
      # Sample random points
      sp_sub<-spsample(i,n=i@data$npoints*10,"random")
      year_sub <- i@data$year
      # Extract raster values at points
      mixedwood_pts <- extract(mixedwood, sp_sub)
      road_pts <- extract(distToRoad, sp_sub)
      # Extract raster value means
      mixedwood_mean <- extract(mixedwood, i, method='simple', fun=mean, na.rm=TRUE)
      road_mean <- extract(distToRoad, i, method='simple', fun=mean, na.rm=TRUE)
      # Skip any elk-years where all data=NA and add to a list
      if(any(is.nan(c(mixedwood_mean, road_mean,
                      mixedwood_pts, road_pts)))==TRUE) {
        skips <- c(skips, i@data$id) 
        next
      }
      # Compile data table of means
      sub_means <- data.table(elkyear = i@data$id, year = i@data$year, 
                              mean_mixedwood = mixedwood_mean, mean_road = road_mean)
      HRmeans <- rbind(HRmeans, sub_means)
      # Compile data table of available points
      sub_avail <- data.table(elkyear = rep(i@data$id, i@data$npoints*10), 
                              year = rep(i@data$year, i@data$npoints*10), 
                              mixedwood = mixedwood_pts, road = road_pts, 
                              npoints = i@data$npoints, sample = rep(0, i@data$npoints*10))
      avail <- rbind(avail, sub_avail)
      # }, warning=function(w){cat("WARNING:", conditionMessage(w), "\n")})
    }
    # Output data frames
    assign(paste('avail', j, sep=''), avail)
    assign(paste('HRmeans', j, sep=''), HRmeans)
    assign(paste('skips', j, sep=''), skips)
  }
  # Rename columns
  colnames(HRmeansOOS) <- c('elkyear', 'year', 'mean_mixedwood', 'mean_road')
  colnames(HRmeansWSD) <- c('elkyear', 'year', 'mean_mixedwood', 'mean_road')
  # Remove all elkyears from the used data if  data was all NA in home range
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
  # Add column for iteration
  RSF_datOOS[, 'iteration' := .(day)]
  RSF_datWSD[, 'iteration' := .(day)]
  
  ### 4: Run individual models ----
  
  print('Fitting individual models...')
  
  # Specify covariates to use
  covars <- c('I(log(road+1))', 'mixedwood')
  
  ### Within-sample individual models ----
  
  id_WSD_out <- data.table()
  # Run model
  for(i in unique(RSF_datWSD$elkyear))  {
    sub_RSF_dat <- RSF_datWSD[elkyear==i]
    id_RSF <- glm(reformulate(covars, response = 'sample'),
                  family = binomial,
                  data = sub_RSF_dat)
    id_out <- as.data.table(broom::tidy(id_RSF))
    # Stop if the model doesn't converge
    if(any(id_out$std.error>30, na.rm=T)){
      next
    }
    # Remove intercept and add elkyear
    id_out <- id_out[!c(term=='(Intercept)')]
    id_out[, c('elkyear', 'numb_pts') := .(i, sub_RSF_dat[1]$npoints)]
    # Bind
    id_WSD_out <- rbind(id_WSD_out, id_out)
  }
  # Add individual habitat means
  id_WSD_out <- merge(id_WSD_out, HRmeansWSD, by='elkyear')
  # Add columns for iteration, date range, and  data type
  id_WSD_out[, c('iteration', 'dates', 'type') := .(day, outputfolder, 'WSD')]
  
  ### Out-of-sample individual models ----
  
  id_OOS_out <- data.table()
  #  Run model
  for(i in unique(RSF_datOOS$elkyear)){
    sub_RSF_dat <- RSF_datOOS[elkyear==i]
    id_RSF <- glm(reformulate(covars, response = 'sample'),
                  family = binomial,
                  data = sub_RSF_dat)
    id_out <- as.data.table(broom::tidy(id_RSF))
    # Stop if the model doesn't converge
    if(any(id_out$std.error>30, na.rm=T)){
      next
    }
    # Remove intercept and add elkyear
    id_out <- id_out[!c(term=='(Intercept)')]
    id_out[, c('elkyear', 'numb_pts') := .(i, sub_RSF_dat[1]$npoints)]
    # Bind
    id_OOS_out <- rbind(id_OOS_out, id_out)
  }
  # Add individual habitat means
  id_OOS_out <- merge(id_OOS_out, HRmeansOOS, by='elkyear')
  # Add columns for iteration, date range, and  data type
  id_OOS_out[, c('iteration', 'dates', 'type') := .(day, outputfolder, 'OOS')]
  
  # Combine all results into a data.table
  mw_rd_out <- rbind(id_WSD_out, id_OOS_out)
  mw_rd_RSFs <- rbind(mw_rd_RSFs, mw_rd_out)
  
  ### 5: Save model output in folder corresponding to date range ----

  # Set output folder for iteration dates
  dir.create(path = paste0("input/RSF_data/", outputfolder, sep='/'))
  # Save data
  saveRDS(RSF_datWSD, file = paste0("input/RSF_data/", paste(outputfolder, 
                                                             "WS_data.rds", sep = "/")))
  saveRDS(RSF_datOOS, file = paste0("input/RSF_data/", paste(outputfolder, 
                                                             "OOS_data.rds", sep = "/")))
  # Save temporary copy of the individual model outputs
  saveRDS(mw_rd_RSFs, 'results/RSF_outputs/temp_mw_rd_RSFs.rds')
  
  print('Done')
  
  #########################################################################
  ##                            END LOOP                                 ##
  #########################################################################
}

# Save final individual model output
saveRDS(mw_rd_RSFs, 'results/RSF_outputs/mw_rd_RSFs.rds')


