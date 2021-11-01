
library(tidyverse)
library(sf)

# Load data
dat <- readRDS('input/prepped_data.rds')

# Initiate df
homerange_sizes <- data.frame()

# Calculate whether an elk can traverse its 2-week home range within 1 hr
for(i in unique(dat$elkblock)) {
  # Subset elk block
  elk_dat <- dat %>%
    filter(elkblock == i)
  
  # 1 - Calculate maximum distance travelled by the elk within 1 hour
  # Initiate vectors
  pt_dists <- c()
  pt_times <- c()
  # Loop  through elk blocks
  for(j in 1:(nrow(elk_dat)-1)) {
    # Calculate distance between consecutive points
    dist_btwn <- as.numeric(st_distance(elk_dat[j ,], elk_dat[j + 1 ,]))
    time_btwn <- as.numeric(difftime(elk_dat[j + 1 ,]$dat_time, 
                                     elk_dat[j ,]$dat_time))
    # Calculate time between consecutive points
    pt_dists <- c(pt_dists, dist_btwn)
    pt_times <- c(pt_times, time_btwn)
  }
  # Get the index for the maximum distance between consecutive points
  max_dist_index <- which.max(pt_dists)
  # Divide by time between those two points to get distance/hour
  max_dist <- pt_dists[[max_dist_index]]/pt_times[[max_dist_index]]
  
  # 2 - Make MCP home range and calculate diameter
  # Make MCP home range
  elk_dat_sp <- as(elk_dat, 'Spatial')
  elk_mcp_sp <- suppressWarnings(adehabitatHR::mcp(elk_dat_sp))
  elk_mcp <- as(elk_mcp_sp, 'sf')
  # Convert polygon vertices to points
  elk_mcp_pts <- st_cast(elk_mcp, 'POINT')
  # Initiate vector
  pt_dists <- c()
  # Loop through vertices
  for(j in 1:(nrow(elk_mcp_pts)-1)) {
    # Calculate distances between vertices
    dist_btwn <- as.numeric(st_distance(elk_mcp_pts[j ,], elk_mcp_pts[j + 1 ,]))
    # Calculate time between consecutive points
    pt_dists <- c(pt_dists, dist_btwn)
  }
  # Get maximum distance between vertices
  max_mcp_diam <- max(pt_dists)
  
  # Build data frame
  homerange_row <- data.frame(elkblock = i, max_dist, max_mcp_diam)
  # Bind together
  homerange_sizes <- rbind(homerange_sizes, homerange_row)
  
}

# Calculate median ± 95% CI home range diameter
median(homerange_sizes$max_mcp_diam)
quantile(homerange_sizes$max_mcp_diam, probs = 0.025)
quantile(homerange_sizes$max_mcp_diam, probs = 0.95)

# Calculate max distance travelled within 1 hr
max(homerange_sizes$max_dist)


