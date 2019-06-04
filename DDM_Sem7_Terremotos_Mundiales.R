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


#Mapa Chile
terremotos_al <- terremotos %>% filter (latitud>(-39) &
                              latitud<(-33) &
                              longitud>(-76) & 
                              longitud<(-70) &
                                fecha=="2010-02-27" & 
                                magnitud>6)


library(raster)
elevation.raster <- raster("rasters_gmrt_2/output_gmrt.tif")
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
elevation.texture.map <- readPNG("rasters_gmrt_2/viz.gmrt.crhs.white.png")





bbox <- list(
  p1 = list(long = -76, lat = -39),
  p2 = list(long = -70, lat = -33)
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

label_8.8 <- list(text = "8.8")
label_8.8$pos <- find_image_coordinates(
  long = -72.898, lat = -36.122, bbox = bbox,
  image_width = image_size$width, image_height = image_size$height)

label_7.4 <- list(text = "7.4")
label_7.4$pos <- find_image_coordinates(
  long = -75.048, lat = -37.773, bbox = bbox,
  image_width = image_size$width, image_height = image_size$height)

label_6.2 <- list(text = "6.2")
label_6.2$pos <- find_image_coordinates(
  long = -72.614, lat = -34.867, bbox = bbox,
  image_width = image_size$width, image_height = image_size$height)

label_6.2_2 <- list(text = "6.2")
label_6.2_2$pos <- find_image_coordinates(
  long = -71.828, lat = -33.422, bbox = bbox,
  image_width = image_size$width, image_height = image_size$height)

label_6.1 <- list(text = "6.1")
label_6.1$pos <- find_image_coordinates(
  long = -72.427, lat = -34.749, bbox = bbox,
  image_width = image_size$width, image_height = image_size$height)

label_6.1_2 <- list(text = "6.1")
label_6.1_2$pos <- find_image_coordinates(
  long = -73.208, lat = -36.354, bbox = bbox,
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

render_label(elevation.matrix, y = label_8.8$pos$y, x = label_8.8$pos$x, z = 6000, zscale = my.z,
             text = label_8.8$text, relativez = FALSE,  textsize = 1,linewidth = 1,freetype = FALSE)
render_label(elevation.matrix, y = label_7.4$pos$y, x = label_7.4$pos$x, z = 5000, zscale = my.z,
             text = label_7.4$text, relativez = FALSE,  textsize = 1,linewidth = 1,freetype = FALSE)
render_label(elevation.matrix, y = label_6.2$pos$y, x = label_6.2$pos$x, z = 4000, zscale = my.z,
             text = label_6.2$text, relativez = FALSE,  textsize = 1,linewidth = 1,freetype = FALSE)
render_label(elevation.matrix, y = label_6.2_2$pos$y, x = label_6.2_2$pos$x, z = 4000, zscale = my.z,
             text = label_6.2_2$text, relativez = FALSE,  textsize = 1,linewidth = 1,freetype = FALSE)
render_label(elevation.matrix, y = label_6.1$pos$y, x = label_6.1$pos$x, z = 3000, zscale = my.z,
             text = label_6.1$text, relativez = FALSE,  textsize = 1,linewidth = 1,freetype = FALSE)
render_label(elevation.matrix, y = label_6.1_2$pos$y, x = label_6.1_2$pos$x, z = 3000, zscale = my.z,
             text = label_6.1_2$text, relativez = FALSE,  textsize = 1,linewidth = 1,freetype = FALSE)



render_snapshot()

library(rgl)
library(magick)


movie3d(spin3d(axis = c(0, 1, 0), rpm = 4), duration = 15, dir = getwd(), movie = "render_3")

