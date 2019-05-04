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
  hispanos = if_else(codigo_iso_destino %in% codigo_iso_origen, "Hispanoamérica", "Otros")
)

#Subset ARG
comercio_centroid_arg<-subset(comercio_centroid, comercio_centroid$codigo_iso_origen=="ARG")

summary(comercio_centroid_arg$porcentaje_expo)

comercio_centroid_arg <-
  comercio_centroid_arg %>% mutate(porcentaje_expo2 = if_else(
    porcentaje_expo <= 0.01,
    "< 1%",
    if_else(
      porcentaje_expo > 0.01 &
        porcentaje_expo <= 0.1,
      "1 - 10 %",
      if_else(
        porcentaje_expo > 0.1 & porcentaje_expo <= 0.25,
        "10 - 25%",
        if_else(porcentaje_expo > 0.2, ">25%", "no")
      )
    )
  ))

#animación
anim<-ggplot() +
    geom_sf(data = paises, fill = "black", color = "grey50", size = .15) +
  geom_line(data = comercio_centroid_arg, aes(orig_lon, orig_lat),
            size = 0.5) +
  geom_point(data = comercio_centroid_arg, aes(dest_lon, dest_lat, color=porcentaje_expo2, size=2))+
  geom_curve(data = comercio_centroid_arg, 
             aes(x = orig_lon, y = orig_lat, 
                 xend = dest_lon, yend = dest_lat,
                 color=hispanos, size=porcentaje_expo
                 ), alpha=0.25, show.legend = TRUE)+
  coord_sf(crs = st_crs(paises)) +
  scale_fill_distiller(palette = "RdYlGn")+
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
       caption = "#DatosdeMiercoles")+
    transition_manual(anio) +
    enter_fade()+
    exit_fade()

anim_save("relaciones_comerciales_exportaciones.gif", animation = anim, fps = 20, 
          width = 800, height = 550)

