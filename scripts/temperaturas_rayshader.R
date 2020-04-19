library(readr)
library(dplyr)
library(ggplot2)
library(emojifont)
library(raster)
library(rayshader)
library(png)
library(sp)
library(rgdal)
library(geoviz)
library(rgl)
library(magick)

#Datos

estaciones <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-10-09/estaciones.csv",locale = readr::locale(encoding = "latin1"))
meteo <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-10-09/meteo.csv", na = "-99.9")

#extremos de temperatura y precipitación

min_temps <- meteo %>% 
  dplyr::select (id_estacion,fecha,t=t_min) %>% 
  group_by(id_estacion) %>% 
  summarise(t = min(t, na.rm = TRUE))

max_temps <- meteo %>% 
  dplyr::select (id_estacion,fecha,t=t_max) %>% 
  group_by(id_estacion) %>% 
  summarise(t = max(t, na.rm = TRUE))

prec <- meteo %>% 
  dplyr::select (id_estacion,fecha,t=precipitacion) %>% 
  group_by(id_estacion) %>% 
  summarise(t = sum(t, na.rm = TRUE))

min_temps <- merge (min_temps,estaciones,by="id_estacion", all.x=TRUE, all.y=FALSE)
max_temps <- merge (max_temps,estaciones, by="id_estacion", all.x=TRUE, all.y=FALSE)
prec <- merge(prec,estaciones, by="id_estacion", all.x=TRUE, all.y=FALSE)

min_temps$emoji <- emoji('snowflake')
min_temps$temp <- "min"
max_temps$emoji <- emoji('sunny')
max_temps$temp <- "max"
prec$emoji <- emoji('droplet')
prec$temp <- "prec"

maxmin_temps <- rbind(min_temps,max_temps)

maxmin_temps %>% filter(elevacion<=1500) %>%
ggplot(aes(x=elevacion,y=t, label=emoji, color=emoji)) +
  geom_smooth()+
  scale_color_manual(values = c("darkgoldenrod2", "steelblue", "green"))+
  geom_text(family="EmojiOne", size=4, show.legend = FALSE) +
  labs(title="Temperatura máxima o mínima registrada y elevación", 
       subtitle = "Estaciones meteorológicas de la Cuenca del Río de la Plata", 
       caption="#Datosdemiercoles - @karbartolome", 
       x="Altura en metros sobre el nivel del mar", 
       y="Temperatura registrada (°C)")+
  theme_get()+
  facet_grid(rows=vars(emoji), scales="free_y")
  
prec %>% filter(elevacion<=3000) %>%
  ggplot(aes(x=elevacion,y=t, label=emoji, color=emoji)) +
  geom_smooth(color="green")+
  scale_color_manual(values = c("steelblue"))+
  geom_text(family="EmojiOne", size=5, show.legend = F) +
  labs(title="Precipitaciones y elevación", 
       subtitle = "Estaciones meteorológicas de la Cuenca del Río de la Plata", 
       caption="#Datosdemiercoles - @karbartolome", 
       x="Altura en metros sobre el nivel del mar", 
       y="Milimetros acumulados")+
  theme_get()
  
min_temp <- min_temps %>% slice(which.min(t)) 
max_temp <- max_temps %>% slice(which.max(t)) 
max_prec <- prec %>% slice(which.max(t)) 
min_prec <- prec %>% slice(which.min(t))

# Rayshader

#el siguiente archivo tif lo obtuve de opentopo.sdsc.edu/ (se puede seleccionar en un mapa mundial la superficie que se quiere convertir en raster)
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

elevation.texture.map <- readPNG("raster_image.png")


elevationn <- elevation.matrix  %>%
  sphere_shade(sunangle = 35,
               texture = "desert",
               zscale = my.z) %>%
  #add_shadow(elevation.amb.shade) %>%
  #add_shadow(elevation.ray.shade, 0.7) %>%
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


crs(elevation.raster) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 


#los puntos extremos tienen coordenadas en latlong que hay que convertirlo al formato del raster
extremos<- rbind(max_prec,min_prec,max_temp,min_temp)
extremos_latlon <- data.frame(extremos %>% dplyr::select(lat,lon))
coordinates(extremos_latlon) <- c("lat", "lon")
proj4string(extremos_latlon) = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
extremos_latlon = spTransform(extremos_latlon,CRS(proj4string(extremos_latlon)))
extremos_raster<-latlong_to_rayshader_coords(elevation.raster,extremos_latlon$lat,extremos_latlon$lon)

#con el render todavía abierto correr lo siguiente para los labels de temperatura y precipitaciones extremas
render_label(
  elevation.matrix,
  text = "97326mm",
  y = extremos_raster$y[1],
  x = extremos_raster$x[1],
  z = 6000,
  zscale = my.z,
  relativez = FALSE,
  textsize = 1,
  linewidth = 2,
  freetype = FALSE,
  textcolor = "blue" 
)

render_label(
  elevation.matrix,
  text = paste(extremos[3,]$t,"C,", "4158mm"),
  y = extremos_raster$y[3],
  x = extremos_raster$x[3],
  z = 8000,
  zscale = my.z,
  relativez = FALSE,
  textsize = 1,
  linewidth = 2,
  freetype = FALSE, 
  textcolor = "green"
)
render_label(
  elevation.matrix,
  text = paste(extremos[4,]$t, " C"),
  y = extremos_raster$y[4],
  x = extremos_raster$x[4],
  z = 6000,
  zscale = my.z,
  relativez = FALSE,
  textsize = 1,
  linewidth = 2,
  freetype = FALSE, 
  textcolor = "red"
)

#guardar como animación
movie3d(spin3d(axis = c(0, 1, 0), rpm = 4), duration = 15, dir = getwd(), movie = "rayshader")
