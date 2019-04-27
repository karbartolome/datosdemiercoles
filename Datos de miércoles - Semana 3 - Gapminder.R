#Librerías
library(esquisse)
library(tidyverse)
library(gganimate)

#Datos de miércoles - Gapminder
gapminder <-
  readr::read_csv(
    "https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-04-24/gapminder_es.csv"
  )
gapminder$poblacion_en_millones <- gapminder$poblacion / 1000000
gapminder$pbi_percap <- round(gapminder$pbi_per_capita, digits = 1)

#Datos de miércoles - Mundiales
mundiales <-
  readr::read_delim(
    "https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-04-10/partidos.txt",
    delim = "\t"
  )

##cantidad de goles por equipo por año
datos1 <- mundiales %>%
  group_by(equipo_1, anio) %>%
  summarise(goles = sum(equipo_1_final))
datos1$eq_anio <- paste(datos1$anio, datos1$equipo_1)
datos2 <- mundiales %>%
  group_by(equipo_2, anio) %>%
  summarise(goles = sum(equipo_2_final))
datos2$eq_anio <- paste(datos2$anio, datos2$equipo_2)

goles <- merge(datos1, datos2, by = "eq_anio")
goles$total <- goles$goles.x + goles$goles.y
goles <- goles %>% select(anio = anio.x, pais = equipo_1, total)

#Países que participaron de mundiales
participantes <-
  data.frame(pais = unique(goles$pais), participo = "participó")

#Añado la información de participación de mundiales a Gapminder
gapminder_participantes<-merge(gapminder,participantes,by="pais",all.x=TRUE)
gapminder_participantes <- gapminder_participantes %>% mutate(
  Mundiales = if_else(
    is.na(gapminder_participantes$participo),
    "No participó", 
    "Participó"))

options(scipen=999)

##Expectativa de vida y PBI per cápita - Evolución
#Población y participación en mundiales
anim <- ggplot(
  data = gapminder_participantes,
  aes(
    x = pib_per_capita,
    y = esperanza_de_vida,
    size = poblacion_en_millones,
    color = Mundiales
    )
  ) +
  geom_point() +
  scale_x_log10() +
  scale_colour_manual(values = c("No participó" = "firebrick4", "Participó" =
                                   "deepskyblue4")) +
  labs(
    title = "PBI per cápita y expectativa de vida a lo largo del tiempo",
    subtitle = "{closest_state}",
    x = "PBI per cápita (Log)",
    y = "Expectativa de vida",
    caption = "Datos de Miércoles, RstatsES",
    color = "Mundiales",
    size = "Población"
  ) +
  theme_gray() +
  theme(
    legend.title = element_text(hjust = 1),
    legend.text = element_text(hjust = 0.5),
    legend.position = "bottom",
    legend.box = "vertical",
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      vjust = 1
    )
  ) +
  facet_wrap(vars(continente), nrow = 1)+
  transition_states(anio)
#Genero la animación
animate(anim, duration = 4)

#Guardo la animación
anim_save("Gapminder.gif",animation = last_animation())
