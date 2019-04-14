#Librerías
library(readr)
library(ggplot2)
library(gganimate)
library(tidyverse)

#Datos
conjunto_datos <- readr::read_delim("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-04-10/partidos.txt",delim = "\t")

#Cantidad de goles por equipo por año
datos<- conjunto_datos %>% 
  group_by(equipo_1, anio) %>% 
  summarise(goles=sum(equipo_1_final))
datos$eq_anio <- paste(datos$anio, datos$equipo_1)
datos2<- conjunto_datos %>% 
  group_by(equipo_2, anio) %>% 
  summarise(goles=sum(equipo_2_final))
datos2$eq_anio <- paste(datos2$anio, datos2$equipo_2)

goles<-merge(datos, datos2, by="eq_anio")
goles$total <- goles$goles.x+goles$goles.y

goles <- goles %>% select(anio=anio.x, equipo=equipo_1, total)

#Máximos goleadores por año
maxgoleadores <- goles %>%
      group_by(anio) %>%
      arrange(desc(total)) %>%
      slice(1:1) %>%    #si se modifica el 1:1 por 1:5 serían los 5 con máx cantidad de goles
      select(equipo, total, anio)

#Ggplot animado
anim <- ggplot(maxgoleadores, aes(x=total, y=anio, color=equipo)) +
  geom_point(size=4)+
  geom_text(aes(label=paste(equipo,total), hjust=-0.2))+
  scale_x_continuous(limits = c(min(0),max(maxgoleadores$total)+6))+
  labs(title="Países con mayor cantidad de goles por año", 
       x="Cantidad de goles", 
       y="Año",
       caption = "Datos de Miércoles")+
  transition_states(anio, transition_length=0.5, state_length = 0.5)+
  shadow_mark()

#Ver y guardar gif
animate(anim)
anim_save("anim.gif", animation = anim)


