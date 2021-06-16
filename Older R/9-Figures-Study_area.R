#############################
#############################
## 9 - STUDY AREA FIGURES ##
############################
############################

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
dat_sf <- st_as_sf(dat, coords=c('X','Y'), crs=projection("+proj=longlat +datum=WGS84 +no_defs")) %>% mutate(elkyear=factor(elkyear))

hlt_sf <- dat_sf[dat_sf$elkyear %in% c(17, 35, 28, 69),]
# Convert to spatial objects
dat_sp <- as(dat_sf, 'Spatial')
# Create MCPs
dat_mcp <- mcp(dat_sp, percent=95)
# Add additional elkyears
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
states <- getData(country='USA', level=1)
states_sf <- as(states, 'sf')
provinces_sf <- as(provinces, 'sf')
states_sf <- states_sf[provinces_sf$NAME_1 %in% c('Minnesota', 'North Dakota')]
provinces_sf <- provinces_sf[provinces_sf$NAME_1 %in% c('Manitoba', 'Saskatchewan', 'Ontario', 'Alberta'),]
mb_bound <- st_read('input/shapes/bdy_province_py')
rmnp_bound <- st_read('input/shapes/RMNP_park_boundary/')
rmnp_roads <- st_read('input/shapes/RMNP_roads/')
# Load Canada boundary
canada <- ne_countries(country='canada', returnclass='sf')
usa <- ne_countries(country='United States of America', returnclass='sf')
# Create bounding box for population
pop_bb = st_as_sfc(st_bbox(dat_mcp_sf))
rmnp_bb = st_as_sfc(st_bbox(c(xmin = -101.1, xmax = -100.3, ymax = 51.1, ymin = 50.7), crs = st_crs(4326)))


sa_inset <- ggplot() + geom_sf(data=states_sf) + geom_sf(data=provinces_sf) +
  geom_sf(data=rmnp_bound) + geom_sf(data=rmnp_bb, fill=NA, colour='black', size=1) +
  coord_sf(ylim=c(46,54), xlim=c(-104,-92), expand=F) + 
  theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(),
        plot.background = element_rect(colour='black', fill='white', size=2), 
        panel.background = element_rect(fill='white'))

sa_inset <- sa_inset + annotate('text', x=-95, y=47, label='MN') +
           annotate('text', x=-102, y=47, label='ND') +
           annotate('text', x=-100, y=50, label='MB')

sa_main <- ggplot() + geom_sf(data=provinces_sf, fill='white') + geom_sf(data=rmnp_bound) + 
  geom_sf(data=dat_sf, alpha=0.3, fill='grey', colour='grey', size=0.5) +
  # Add coloured points for individuals that don't follow the mw-road functional response
  geom_sf(data=dat_sf[dat_sf$elkyear %in% c(2,8,10,21,22,34,35,47,55) ,], aes(fill=elkyear, colour=elkyear), size=0.5) +
  scale_colour_viridis_d() + scale_fill_viridis_d() +
# geom_sf(data=mcp_17_sf, fill='#9820c375', colour='#000000', size=1) + geom_sf(data=mcp_28_sf, fill='#9820c375', colour='#000000', size=1) + geom_sf(data=mcp_35_sf, fill='#9820c375', colour='#000000', size=1) + geom_sf(data=mcp_69_sf, fill='#9820c375', colour='#000000', size=1) + 
  geom_sf(data=rmnp_roads, col='black', alpha=0.4) + 
  annotation_north_arrow(style=north_arrow_orienteering(fill=c('black', 'black'), text_size=20), width=unit(2, 'cm'), height=unit(3, 'cm'), location='br', pad_x = unit(11, 'cm')) +
  annotation_scale(pad_x = unit(14, 'cm'), pad_y = unit(0.5, 'cm'), location='br', height = unit(1, 'cm'), style='ticks', text_cex = 2, line_width = 2) +
  coord_sf(ylim=c(50.68912,51.06854), xlim=c(-101.1261,-100.2884)) +
  theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(),
        plot.background = element_rect(colour='black', fill='white', size=2), 
        panel.background = element_rect(fill='white'), legend.position = 'none')

# tiff('figures/study_area_map.tiff', width = 12, height = 8, units = 'in', res = 300)
gg_inset_map1 <- ggdraw() +
  draw_plot(sa_main) +
  draw_plot(sa_inset, x = 0.006, y = 0.01, width = 0.3, height = 0.3)

# rmnp_bb <- extent(-105, -98, 50, 52)
# rmnp_bb <- extent(352933.9, 5595097, 461859.3, 5653647) # bbox:           xmin: 349337 ymin: 5616150 xmax: 410460 ymax: 5658821
mw_crop <- crop(mw, rmnp_bb_edge)
# First, to a SpatialPointsDataFrame
mw_pts <- rasterToPoints(mw, spatial = TRUE)
# Then to a dataframe
mw_df  <- data.frame(mw_pts)


