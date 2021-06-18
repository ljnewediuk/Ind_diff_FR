
library(tidyverse)
library(sf)

# Load data
dat <- readRDS('input/rmnp_data.rds') %>%
  # Filter dates between Dec. 1 and Jan. 29
  filter(julian_day %in% c(1: 29, 335: 365)) %>%
  # Assign a block to equal 15-d periods
  mutate(block = case_when(julian_day %in% 335:349 ~ 1,
                           julian_day %in% 350:364 ~ 2,
                           julian_day %in% c(365, 1:14) ~ 3,
                           julian_day %in% 15:29 ~ 4))

# Create separate df to screen out blocks without enough data
screen_dat <- dat %>%
  # Drop geometry
  st_drop_geometry() %>%
  # Group by elkyear and block
  group_by(elkyear, block) %>%
  # Get number of location points in each block
  summarize(points_in_block = n()) %>%
  # Filter blocks with < 60% expected data within 15-d period (12 loc*15 d*0.6)
  filter(points_in_block >= 108)

# Remove individual-blocks with less than threshold amount of data
prepped_dat <- dat %>%
  # Join to screening df
  right_join(screen_dat) %>%
  # Add elk-block column
  mutate(elkblock = paste(elkyear, block, sep = '_')) %>%
  # Ungroup 
  ungroup() %>%
  dplyr::select(- c(uid, ID)) %>%
  #  Remove elk from 2006 and 2007
  filter(! year %in% c(2006, 2007))

# Save prepped data
saveRDS(prepped_dat, 'input/prepped_data.rds')
