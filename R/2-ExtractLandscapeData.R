
library(tidyverse)
library(sf)
library(raster)

# Load prepped location data
dat <- readRDS('input/prepped_data.rds')

# Import raster data into directory
system('mkdir ~/Documents/R-Projects/individual_fr/rasters/')
system(paste('cp ~/Documents/Spatial*Data/Manitoba*Data/distance*to/',
             'RMNP_dist_to_municipal_road.tif ', 
             '~/Documents/R-Projects/individual_fr/rasters/', sep = ''))
system(paste('cp ~/Documents/Spatial*Data/Manitoba*Data/landcover/', 
             'RMNP_ACI/aci_2009.tif ',
             '~/Documents/R-Projects/individual_fr/rasters/', sep = ''))

# Read landcover
hab_rast <- raster("rasters/aci_2009.tif")
# Make all values of mixed forest = 1, and otherwise zero
hab_rast[hab_rast %in% c(220, 230)] <- 1
hab_rast[hab_rast > 1] <- 0
# Name raster
names(hab_rast) <- 'mixedwood'

# Read road data
road <- raster("rasters/RMNP_dist_to_municipal_road.tif")
# Name raster
names(road) <- 'road'

# Loop through elk blocks to build rsf data

rsf_dat <- data.frame()

# Start loop
for(i in unique(dat$elkblock)) {
  # Subset out elk block
  sub_dat <- dat %>%
    filter(elkblock == i) %>%
    # Add column for case = T
    mutate(case = T)
  
  # Convert to Spatial Points and get 100% MCP
  sub_sp <- as(sub_dat, 'Spatial')
  sub_mcp <- suppressWarnings(adehabitatHR::mcp(sub_sp, percent = 100))
  # Convert mcp back to sf
  sub_mcp_sf <- as(sub_mcp, 'sf')
  
  # Sample available points (10 x used) and get coords
  sub_avail <- sub_mcp_sf %>%
    st_sample(size = nrow(sub_dat)*10, type = 'random') %>%
    st_coordinates()
  
  # Build df with columns corresponding to used data
  full_dat <- data.frame(X = sub_avail[,1],
                        Y = sub_avail[,2]) %>%
    # Add corresponding columns
    mutate(uid = unique(sub_dat$uid),
           elkyear = unique(sub_dat$elkyear),
           dat_time = NA,
           year = unique(sub_dat$year),
           julian_day = NA,
           block = unique(sub_dat$block),
           points_in_block = unique(sub_dat$points_in_block),
           elkblock = unique(sub_dat$elkblock),
           case = F) %>%
    # Convert to sf
    st_as_sf(coords = c('X', 'Y'), crs = st_crs(sub_dat)) %>%
    # Bind to observed data
    rbind(sub_dat)
  
    # Extract and add landscape data at points
    rsf_row <- full_dat %>% mutate(mixedwood = extract(hab_rast, full_dat),
           roaddist = extract(road, full_dat),
           # Extract and add landscape data across homerange
           mixedwood_hr = as.numeric(extract(hab_rast, sub_mcp_sf, 
                                  method = 'simple', fun = mean, na.rm = T)),
           roaddist_hr = as.numeric(extract(road, sub_mcp_sf, 
                                 method = 'simple', fun = mean, na.rm = T))) %>%
    # Drop geometry
    st_drop_geometry()
  
  # Bind with all data
  rsf_dat <- rbind(rsf_dat, rsf_row)
}

# Remove raster data
system("rm -rf ~/Documents/R-Projects/individual_fr/rasters/")
