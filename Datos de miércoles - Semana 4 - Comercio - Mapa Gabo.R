# Exportaciones Argentinas al mundo - Segmentado por países hispanoaméricanos a los que exporta y qué porcentaje de sus exportaciones totales van a cada país. 
#______________________________________________________________________________________________
# Extensión de: https://gitlab.com/gavg712/datos_de_miercoles_aportes/blob/master/R/20190501.R 
# De @gavg712 
#______________________________________________________________________________________________
# % exportado por país y países hispanoaméricanos

# porcentaje exportado
a <- aggregate(exportado ~ codigo_iso_origen+anio, 
  data=comercio_centroid,
  FUN = "sum"
)
comercio_centroid <-  merge(comercio_centroid, a, by = c("codigo_iso_origen", "anio"))

comercio_centroid$porcentaje_expo <-(comercio_centroid$exportado.x / comercio_centroid$exportado.y)*100

#Países hispanoaméricanos
comercio_centroid <- comercio_centroid %>% mutate(
  hispanos = if_else(codigo_iso_destino %in% codigo_iso_origen, "Hispanos", "Otros")
)

#Subset ARG
comercio_centroid_arg<-subset(comercio_centroid, comercio_centroid$codigo_iso_origen=="ARG")

aggregate(comercio_centroid_arg$porcentaje_expo ~ anio,
          data=comercio_centroid_arg, 
          FUN = "sum")


comercio_centroid_arg <-
  comercio_centroid_arg %>% mutate(porcentaje_expo2 = if_else(
    porcentaje_expo <= 1,
    "<1%",
    if_else(
      porcentaje_expo > 1 &
        porcentaje_expo <= 5,
      "1-5%",
      if_else(
        porcentaje_expo > 5 & porcentaje_expo <= 10,
        "5-10%",
        if_else(porcentaje_expo > 10, ">10%", "no")
      )
    )
  ))

#animación
anim<-ggplot() +
    geom_sf(data = paises, fill = "black", color = "grey", size = .15) +
  geom_line(data = comercio_centroid_arg, aes(orig_lon, orig_lat),
            size = 0.5) +
  geom_point(data = comercio_centroid_arg, aes(dest_lon, dest_lat, 
                                               color=porcentaje_expo2, 
                                               size=2))+
  geom_curve(data = comercio_centroid_arg, 
             aes(x = orig_lon, y = orig_lat, 
                 xend = dest_lon, yend = dest_lat,
                 color=hispanos, size=porcentaje_expo
                 ), alpha=0.25, show.legend = TRUE)+
  coord_sf(crs = st_crs(paises)) +
  scale_fill_distiller(palette = "RdYlGn")+
  scale_colour_manual(name="",  
                      values = c("<1%"="grey", "1-5%"="seagreen1", "5-10%"="slateblue2",
                                 ">10%"="red", "Hispanos"="magenta2", "Otros" ="steelblue3"))+
  guides(color = guide_legend(nrow = 2), size = FALSE) +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        legend.position = "bottom",
        legend.background = element_rect(fill = "white"),
        legend.text = element_text(color = "black"),
        legend.key = element_rect(fill = NA, color = NA),
        panel.background = element_rect(fill = "white")) +
  labs(title = 'Exportaciones de Argentina al mundo',
       subtitle = 'Exportaciones en el Año: {current_frame}',
       caption = "#DatosdeMiercoles", 
       color="Porcentaje de expo totales")+
    transition_manual(anio) +
    enter_fade()+
    exit_fade()

anim_save("relaciones_comerciales_exportaciones.gif", animation = anim, fps = 20, 
          width = 800, height = 550)



#Para exportaciones por tipo de exportación:
#Modifico el código de @gavg712
#________________________________________________________________________________________________________________
## Datos -----

comercio <- read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-05-01/comercio_hispanoamerica_mundo_agregado.csv")
data("countriesHigh", package = "rworldxtra")
paises <- countriesHigh %>% 
  spTransform("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs") %>% 
  st_as_sf() %>%
  filter(REGION != "Antarctica")

## tweet 1: Relaciones comerciales Hispanoamérica ----

paises_centroid <- paises %>%
  select(NAME, NAME_FORMA, ISO_A3, REGION, LON, LAT) %>%
  st_centroid() %>%
  mutate(LON = st_coordinates(.)[,1],
         LAT = st_coordinates(.)[,2]) %>%
  mutate_if(.predicate = is.factor, list(~as.character))

comercio_centroid <- comercio %>% 
  filter(codigo_iso_origen != codigo_iso_destino) %>%
  group_by(anio, codigo_iso_origen, codigo_iso_destino, nombre_comunidad_producto) %>%
  summarise(exportado = sum(valor_exportado_dolares), 
            importado = sum(valor_importado_dolares)) %>%
  ungroup() %>%
  mutate(balance = (exportado - importado)/1e6,
         rel = scales::rescale(exportado, to = c(0.1,5)),
         codigo_iso_destino = str_to_upper(codigo_iso_destino),
         codigo_iso_origen = str_to_upper(codigo_iso_origen)) %>%
  left_join(paises_centroid %>% select(ISO_A3, REGION, LON, LAT), 
            by = c("codigo_iso_origen" = "ISO_A3")) %>%
  rename(orig_lon = LON, orig_lat = LAT) %>%
  select(-geometry) %>%
  left_join(paises_centroid  %>% select(-REGION), 
            by = c("codigo_iso_destino" = "ISO_A3")) %>%
  rename(dest_lon = LON, dest_lat = LAT) %>%
  filter(!is.na(dest_lat))%>%
  mutate(grp = paste0(anio, " a ", NAME)) %>%
  st_as_sf()
#__________________________________________________________________________________________________________
#y después:
# porcentaje exportado
a <- aggregate(exportado ~ codigo_iso_origen+nombre_comunidad_producto, 
  data=comercio_centroid,
  FUN = "sum"
)
comercio_centroid <-  merge(comercio_centroid, a, by = c("codigo_iso_origen", "nombre_comunidad_producto"))

comercio_centroid$porcentaje_expo <-
  (comercio_centroid$exportado.x / comercio_centroid$exportado.y) * 100

#Países hispanoaméricanos
comercio_centroid <- comercio_centroid %>% mutate(
  hispanos = if_else(codigo_iso_destino %in% codigo_iso_origen, "Hispanos", "Otros")
)

#Subset ARG
comercio_centroid_arg<-subset(comercio_centroid, comercio_centroid$codigo_iso_origen=="ARG")

aggregate(comercio_centroid_arg$porcentaje_expo ~ nombre_comunidad_producto,
          data=comercio_centroid_arg, 
          FUN = "sum")


comercio_centroid_arg <-
  comercio_centroid_arg %>% mutate(porcentaje_expo2 = if_else(
    porcentaje_expo <= 1,
    "<1%",
    if_else(
      porcentaje_expo > 1 &
        porcentaje_expo <= 5,
      "1-5%",
      if_else(
        porcentaje_expo > 5 & porcentaje_expo <= 10,
        "5-10%",
        if_else(porcentaje_expo > 10, "10% o más", "no")
      )
    )
  ))

#animación
anim<-ggplot() +
    geom_sf(data = paises, fill = "black", color = "grey", size = .10) +
  geom_line(data = comercio_centroid_arg, aes(orig_lon, orig_lat),
            size = 0.5) +
  geom_point(data = comercio_centroid_arg, aes(dest_lon, dest_lat, 
                                               color=porcentaje_expo2, 
                                               size=5))+
  geom_curve(data = comercio_centroid_arg, 
             aes(x = orig_lon, y = orig_lat, 
                 xend = dest_lon, yend = dest_lat,
                 color=hispanos
                 ), alpha=0.25, show.legend = TRUE)+
  coord_sf(crs = st_crs(paises)) +
  scale_fill_distiller(palette = "RdYlGn")+
  scale_colour_manual(name="",  
                      values = c("<1%"="grey", "1-5%"="blue", "5-10%"="green",
                                 "10% o más"="red", "Hispanos"="magenta2", "Otros" ="steelblue3"))+
  guides(color = guide_legend(nrow = 2), size = FALSE) +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        legend.position = "bottom",
        legend.background = element_rect(fill = "white"),
        legend.text = element_text(color = "black"),
        legend.key = element_rect(fill = NA, color = NA),
        panel.background = element_rect(fill = "white")) +
  labs(title = 'Exportaciones de Argentina - Tipos de exportaciones y relación con Hispanoamérica',
       subtitle = 'Tipo de exportación: {current_frame}',
       caption = "#DatosdeMiercoles", 
       color="Porcentaje de expo totales")+
    transition_manual(nombre_comunidad_producto) +
    enter_fade()+
    exit_fade()

anim_save("relaciones_comerciales_exportaciones2.gif", animation = anim, fps = 3, 
          width = 800, height = 550)
