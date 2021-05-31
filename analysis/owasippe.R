library(rayshader)
library(rayvista)
library(tidyverse)
library(osmdata)
library(sf)
library(raster)
library(doFuture)

.long <- -86.2130000000
.lat <- 43.4400000000
.zscale <- 0.6
.phi <- 30

owasippe_3d <-
  plot_3d_vista(
    lat = .lat,
    long = .long,
    zscale = .zscale,
    phi = .phi,
    radius = 3000,
    overlay_detail = 15,
    elevation_detail = 14,
    show_vista = FALSE, 
    verbose = TRUE
  )

temp_img <- tempfile(fileext = '.png')
png::writePNG(owasippe_3d$texture, temp_img)
mag_img <- magick::image_read(temp_img)

edited_texture <- magick::image_modulate(mag_img,
                                         brightness = 90,
                                         saturation = 40,
                                         hue = 280) %>%
  magick::image_enhance() %>%
  magick::image_equalize() %>%
  magick::image_contrast(sharpen = 1)

magick::image_write(edited_texture, temp_img)
edit_tex <- png::readPNG(temp_img)

lat_range <- c(attr(owasippe_3d$dem_matrix, 'extent')[3],
               attr(owasippe_3d$dem_matrix, 'extent')[4])

long_range <- c(attr(owasippe_3d$dem_matrix, 'extent')[1],
               attr(owasippe_3d$dem_matrix, 'extent')[2])

osm_bbox = c(long_range[1], 
             lat_range[1], 
             long_range[2], 
             lat_range[2])

convert_coords = function(lat,long, from = CRS("+init=epsg:4326"), to) {
  data = data.frame(long = long, lat = lat)
  coordinates(data) <- ~ long+lat
  proj4string(data) = from
  #Convert to coordinate system specified by EPSG code
  xy = data.frame(sp::spTransform(data, to))
  colnames(xy) = c("x","y")
  return(unlist(xy))
}

utm_bbox = convert_coords(lat = lat_range,
                          long = long_range,
                          to = raster::crs(attr(owasippe_3d$dem_matrix, "crs")))

extent_zoomed = extent(utm_bbox[1], 
                       utm_bbox[2], 
                       utm_bbox[3], 
                       utm_bbox[4])

owasippe_highway <- opq(osm_bbox) %>% 
  add_osm_feature("highway") %>% 
  osmdata_sf()

owasippe_lines <- sf::st_transform(owasippe_highway$osm_lines, 
                                   crs = raster::crs(attr(owasippe_3d$dem_matrix, "crs"))) %>%
  mutate(linetype = case_when(
    highway == "cycleway" ~ "dashed",
    highway == "footway" ~ "dashed",
    highway == "path" ~ "dotdash",
#    highway == "track" ~ "dotted",
    highway == "service" ~ "solid",
    highway == "tertiary" ~ "solid",
    highway == "residential" ~ "solid",
    TRUE ~ NA_character_),
    color = case_when(
      highway == "cycleway" ~ "red",
      highway == "footway" ~ "red",
      highway == "path" ~ "red",
      highway == "track" ~ "red",
      highway == "service" ~ "gray80",
      highway == "tertiary" ~ "black",
      highway == "residential" ~ "gray90",     
      TRUE ~ NA_character_)) %>%
  filter(!is.na(linetype)) 

owasippe_lines %>% 
ggplot() +
  geom_sf(aes(color = color, linetype = linetype)) +
  scale_linetype_identity() +
  scale_color_identity() +
  theme_void()

trail_lines <- owasippe_lines %>% 
  filter(color == "red")

trail_layer <-   generate_line_overlay(
    trail_lines,
    extent = extent_zoomed,
    linewidth = 8,
    color = "red",
    heightmap = owasippe_3d$dem_matrix) 

trail_layer2 <-    generate_line_overlay(
    trail_lines,
    extent = extent_zoomed,
    linewidth = 4,
    color = "white",
    heightmap = owasippe_3d$dem_matrix)

road_layer <- owasippe_lines %>% 
  filter(color != "red") %>% 
    generate_line_overlay(
    extent = extent_zoomed,
    linewidth = 8,
    color = "white",
    heightmap = owasippe_3d$dem_matrix
  ) 

owasippe_water <- opq(osm_bbox) %>% 
  add_osm_feature(c("waterway")) %>% 
  osmdata_sf()

owasippe_streams <-
  sf::st_transform(owasippe_water$osm_lines, 
                   crs = raster::crs(attr(owasippe_3d$dem_matrix, "crs")))

stream_layer <-
  generate_line_overlay(
    owasippe_streams,
    extent = extent_zoomed,
    linewidth = 4,
    color = "skyblue2",
    heightmap = owasippe_3d$dem_matrix
  )

owasippe_lake <- opq(osm_bbox) %>% 
  add_osm_feature(c("water")) %>% 
  osmdata_sf()

owasippe_lake <- sf::st_transform(owasippe_lake$osm_polygons, 
                   crs = raster::crs(attr(owasippe_3d$dem_matrix, "crs")))

lake_layer <- generate_polygon_overlay(
    owasippe_lake,
    extent = extent_zoomed,
    linewidth = 4,
    linecolor = "black",
    palette = "skyblue2",
    heightmap = owasippe_3d$dem_matrix)

edit_tex %>%
  add_overlay(trail_layer) %>%
  add_overlay(trail_layer2) %>% 
  add_overlay(road_layer) %>%
  add_overlay(stream_layer) %>% 
  add_overlay(lake_layer) %>% 
  plot_3d(
    owasippe_3d$dem_matrix,
    zscale = .zscale,
    windowsize = 1200,
    zoom = 0.7,
    phi = .phi
  )

render_label(
  heightmap = owasippe_3d$dem_matrix,
  text = 'Admin Center',
  lat = 43.434342,
  long = -86.235229,
  extent = attr(owasippe_3d$dem_matrix, 'extent'),
  altitude = 250,
  clear_previous = T,
  zscale = .zscale
)

render_label(
  heightmap = owasippe_3d$dem_matrix,
  text = 'Camp Blackhawk',
  lat = 43.450472,
  long = -86.210719,
  extent = attr(owasippe_3d$dem_matrix, 'extent'),
  altitude = 250,
  clear_previous = F,
  zscale = .zscale
)

render_compass()

if (file.exists("owasippe.mp4")) file.remove("owasippe.mp4")

all_cores <- parallelly::availableCores(omit = 1)
future::plan("multisession", workers = all_cores) # on Windows

foreach(i = 1:360) %dopar% {
  render_camera(theta = -30 + i)
  render_snapshot(filename = sprintf("owasippe%i.png", i), 
                  title_text = "Owasippe Scout Reservation",
                  title_bar_color = "#1f5214", title_color = "white", title_bar_alpha = 1)
}

future::plan(strategy = "sequential")

rgl::rgl.close()

system("ffmpeg -framerate 20 -i owasippe%d.png -pix_fmt yuv420p owasippe.mp4")

# cleanup
file.remove(list.files(pattern = "png$", full.names = TRUE))
