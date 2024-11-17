setwd('/home/sagesteppe/Documents/assoRted/geomorphology_soils_SOS/scripts')

p2spat <- '../data/spatial'

library(terra)
library(tidyverse)
library(rayshader)
library(osmdata)
library(sf)
# load raster

mv <- rast(file.path(p2spat, 'MonumentValley.tif'))
cb <- rast(file.path(p2spat, 'UG_dem_3m_v1.tif'))

# crop to reasonable extent for generation

domain_mv <- rast(nrows = 1, ncols = 1) # create a big empty raster, you can go in through sf too. 
ext(domain_mv) <- c( -110.09, -110.04, 36.93, 36.965) # set the extent # old 36.965
crs(domain_mv) <- "EPSG:4326" # define the projection
mv <- crop(mv, domain_mv)
plot(mv)

rm(domain_mv)

domain_cb <- rast(nrows = 1, ncols = 1) # create a big empty raster, you can go in through sf too. 
ext(domain_cb) <- c( -106.97, -106.90, 38.86, 38.92) # set the extent
crs(domain_cb) <- "EPSG:4326" # define the projection
domain_cb <- project(domain_cb, crs(cb))
cb <- crop(cb, domain_cb)

# create products in rayshader formats

mv_mat <- raster_to_matrix(mv)
cb_mat <- raster_to_matrix(cb)

## generate figures 

cb_ext <- ext(project(domain_cb, "EPSG:4326"))
cb_bb <- sf::st_bbox(project(domain_cb, "EPSG:4326"))

osm_bbox <- c(cb_ext[1], cb_ext[3], cb_ext[2], cb_ext[4])

streams <- opq(osm_bbox) %>% 
  add_osm_feature("waterway") %>% 
  osmdata_sf() 

streams <- sf::st_transform(streams$osm_lines, crs=crs(cb))  %>% 
  filter(name != 'Verzuh Young Bafand Ditch')

stream_layer <- generate_line_overlay(streams, extent = ext(cb),
                                     linewidth = 12, color="skyblue2", 
                                     heightmap = cb_mat)

positions <- data.frame(
  positions = c('Summit', 'Shoulder', 'Ridge', 'Backslope', 'Alluvial Fan'), 
  latitude = c(38.88329, 38.88961, 38.88299,  38.87830, 38.88301), 
  longitude = c(-106.94332, -106.94073, -106.94406, -106.92891, -106.89769) 
) %>% 
  st_as_sf(coords = c('longitude', 'latitude'), crs = 4326) %>% 
  st_transform(st_crs(cb)) 

positions <-  generate_label_overlay(positions, extent = ext(cb), heightmap = cb_mat, 
                                     data_label_column = "positions", 
                                     text_size = 8, point_size = 5, color = "white", 
                                     halo_color = "black", halo_expand = 5, 
                                     halo_blur = 10, halo_alpha = 0.8,
)


bm <- cb_mat %>%
  sphere_shade(texture = "imhof1") %>%
  add_shadow(ray_shade(cb_mat, zscale = 3), 0.5) %>%
  add_shadow(ambient_shade(cb_mat), 0) %>% 
  add_overlay(stream_layer)

bm %>% 
  add_overlay(positions) %>% 
  plot_3d(cb_mat, zscale = 10, fov = 0, theta = 180, zoom = 0.75, phi = 25, 
          windowsize = c(1200, 1200))  
Sys.sleep(0.2) 
render_snapshot('crested_butte-theta180-phi25.png') 

mv_mat %>% 
  sphere_shade(texture = "desert") %>% 
  add_shadow(ray_shade(mv_mat, zscale = 3), 0.5) %>% 
  add_shadow(ambient_shade(mv_mat), 0) %>% 
  plot_3d(mv_mat, zscale = 10, fov = 0, theta = 180, zoom = 0.75, phi = 45, 
          windowsize = c(1500, 1200))  
Sys.sleep(0.2)
render_snapshot('monument.valley')

#render_movie('demo_stage_buffer.mp4')