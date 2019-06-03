#install.packages("rayshader")
library(rayshader)
library(dplyr)
setwd("C:/Users/karin/Documents/Rstudio/Datos de Miércoles/DDM_Sem7")
terremotos <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-05-29/terremotos.csv")
max_magnitud <- terremotos[max(terremotos$magnitud),]

library(raster)
elevation.raster <- raster("rasters_gmrt/output_gmrt.tif")
elevation.matrix <-
  matrix(
    extract(elevation.raster, extent(elevation.raster), buffer = 100),
    nrow = ncol(elevation.raster),
    ncol = nrow(elevation.raster)
  )
my.z <- 30

elevation.matrix  %>% 
  sphere_shade(sunangle = 35, texture = "imhof1", zscale = my.z) %>%
  plot_map()

elevation.amb.shade <- ambient_shade(elevation.matrix, zscale = my.z)
elevation.ray.shade <- ray_shade(elevation.matrix,  sunangle = 35, zscale = my.z)

library(png)
elevation.texture.map <- readPNG("rasters_gmrt/viz.gmrt.crhs.white.png")





bbox <- list(
  p1 = list(long = 94, lat = 2),
  p2 = list(long = 97, lat = 5)
)


define_image_size <- function(bbox, major_dim = 400) {
  # calculate aspect ration (width/height) from lat/long bounding box
  aspect_ratio <- abs((bbox$p1$long - bbox$p2$long) / (bbox$p1$lat - bbox$p2$lat))
  # define dimensions
  img_width <- ifelse(aspect_ratio > 1, major_dim, major_dim*aspect_ratio) %>% round()
  img_height <- ifelse(aspect_ratio < 1, major_dim, major_dim/aspect_ratio) %>% round()
  size_str <- paste(img_width, img_height, sep = ",")
  list(height = img_height, width = img_width, size = size_str)
}
image_size <- define_image_size(bbox, major_dim = 600)


find_image_coordinates <- function(long, lat, bbox, image_width, image_height) {
  x_img <- round(image_width * (long - min(bbox$p1$long, bbox$p2$long)) / abs(bbox$p1$long - bbox$p2$long))
  y_img <- round(image_height * (lat - min(bbox$p1$lat, bbox$p2$lat)) / abs(bbox$p1$lat - bbox$p2$lat))
  list(x = x_img, y = y_img)
  
}
label <- list(text = "Islandia Terremoto")
label$pos <- find_image_coordinates(
  long = 95.9820, lat = 3.2950, bbox = bbox,
  image_width = image_size$width, image_height = image_size$height)






elevationn <- elevation.matrix  %>%
  sphere_shade(sunangle = 35,
               texture = "desert",
               zscale = my.z) %>%
  add_shadow(elevation.amb.shade) %>%
  add_shadow(elevation.ray.shade, 0.7) %>%
  add_overlay(
    elevation.texture.map,
    alphacolor = NULL,
    alphalayer = 0.9,
    gamma_correction = TRUE
  ) %>%
  plot_3d(
    heightmap = elevation.matrix,
    zscale = my.z,
    fov = 90,
    lineantialias = TRUE,
    theta = 45,
    phi = 15,
    zoom = 0.7, 
    water=TRUE) 
render_label(
  elevation.matrix,
  y = label$pos$y,
  x = label$pos$x,
  z = 2000,
  zscale = my.z,
  text = "Terremoto 9.1",
  relativez = FALSE,
  textsize = 1,
  linewidth = 2,
  freetype = FALSE
)


render_snapshot()



save_3d_gif(elevation.matrix, file = "elevation.gif", duration = 6)
