
###########################
######## 1 - Load libraries
libs <- c('tidyverse', 'lubridate', 'rgdal', 'raster', 'sf', 'adehabitatHR', 'rnaturalearth', 'rnaturalearthdata', 'cowplot', 'ggspatial')
lapply(libs, require, character.only=T)

######################
######## 2 - Load data
dat <- readRDS('input/dat_cleaned.rds')
mw <- raster('input/shapes/mixedwood/mixedwood.tif')
# Convert to sf object and subset indiviuals to those highlighted in RSS plots
dat_coords <- reproj::reproj(x=cbind(dat$X, dat$Y), source="+proj=utm +zone=14 +datum=NAD83 +units=m +no_defs", target="+proj=longlat +datum=WGS84 +no_defs")
dat$X <- dat_coords[,1]
dat$Y <- dat_coords[,2]
dat_sf <- st_as_sf(dat, coords=c('X','Y'), crs=projection("+proj=longlat +datum=WGS84 +no_defs"))

hlt_sf <- dat_sf[dat_sf$elkyear %in% c(17, 35, 28, 69),]
# Convert to spatial objects
dat_sp <- as(dat_sf, 'Spatial')
# Create MCPs
dat_mcp <- mcp(dat_sp, percent=95)
for(ey in unique(hlt_sf$elkyear)){
  sub_ey <- as(hlt_sf[hlt_sf$elkyear==ey,], 'Spatial')
  assign(paste('mcp', ey, sep='_'), mcp(sub_ey, percent=95))
}
mcp_17_sf <- as(mcp_17, 'sf')
mcp_28_sf <- as(mcp_28, 'sf')
mcp_35_sf <- as(mcp_35, 'sf')
mcp_69_sf <- as(mcp_69, 'sf')
dat_mcp_sf <- as(dat_mcp, 'sf')
# Load park boundary and provincial boundary
provinces <- getData(country="Canada", level=1)
provinces_sf <- as(provinces, 'sf')
provinces_sf <- provinces_sf[provinces_sf$NAME_1 %in% c('Manitoba', 'Saskatchewan', 'Ontario', 'Alberta'),]
mb_bound <- st_read('input/shapes/bdy_province_py')
rmnp_bound <- st_read('input/shapes/RMNP_park_boundary/')
rmnp_roads <- st_read('input/shapes/RMNP_roads/')
# Load Canada boundary
canada <- ne_countries(country='canada', returnclass='sf')
usa <- ne_countries(country='United States of America', returnclass='sf')
# Create bounding box for population
pop_bb = st_as_sfc(st_bbox(dat_mcp_sf))
rmnp_bb = st_as_sfc(st_bbox(rmnp_bound))



sa_inset <- ggplot() + geom_sf(data=usa) + geom_sf(data=canada) +
  geom_sf(data=rmnp_bb, fill='white', colour='black', size=0.3) +
  coord_sf(ylim=c(30,80), xlim=c(-150,-70), expand=F) + 
  theme(axis.text = element_blank(), axis.ticks = element_blank(),
        plot.background = element_rect(colour='black', fill='white', size=2), 
        panel.background = element_rect(fill='white'))

sa_main <- ggplot() + geom_sf(data=provinces_sf, fill='white') + geom_sf(data=rmnp_bound) + 
  geom_sf(data=dat_sf, alpha=0.3, fill='grey', colour='grey') +
  geom_sf(data=mcp_17_sf, fill='#9820c375', colour='#000000', size=1) + geom_sf(data=mcp_28_sf, fill='#9820c375', colour='#000000', size=1) + geom_sf(data=mcp_35_sf, fill='#9820c375', colour='#000000', size=1) + geom_sf(data=mcp_69_sf, fill='#9820c375', colour='#000000', size=1) + 
  geom_sf(data=rmnp_roads, col='black', alpha=0.4) + 
  annotation_north_arrow(style=north_arrow_orienteering(fill=c('black', 'black'), text_size=20), width=unit(2, 'cm'), height=unit(3, 'cm'), location='tl') +
  annotation_scale(pad_x = unit(3, 'cm'), pad_y = unit(1, 'cm'), location='tl', height = unit(1, 'cm'), style='ticks', text_cex = 2, line_width = 2) +
  coord_sf(ylim=c(50.68912,51.06854), xlim=c(-101.1261,-100.2884)) +
  theme(axis.text = element_blank(), axis.ticks = element_blank(),
        plot.background = element_rect(colour='black', fill='white', size=2), 
        panel.background = element_rect(fill='white'), legend.position = 'right')

# tiff('figures/study_area_map.tiff', width = 12, height = 8, units = 'in', res = 300)
gg_inset_map1 <- ggdraw() +
  draw_plot(sa_main) +
  draw_plot(sa_inset, x = 0.1, y = 0.1, width = 0.3, height = 0.3)

# rmnp_bb <- extent(-105, -98, 50, 52)
# rmnp_bb <- extent(352933.9, 5595097, 461859.3, 5653647) # bbox:           xmin: 349337 ymin: 5616150 xmax: 410460 ymax: 5658821
mw_crop <- crop(mw, rmnp_bb_edge)
# First, to a SpatialPointsDataFrame
mw_pts <- rasterToPoints(mw, spatial = TRUE)
# Then to a 'conventional' dataframe
mw_df  <- data.frame(mw_pts)

###### NEED TO MAKE RMNP BBOX GO FURTHER WEST AND NORTH

