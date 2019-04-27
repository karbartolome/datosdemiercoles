#DATOS
gapminder <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-04-24/gapminder_es.csv")
gapminder$poblacion_en_millones <- gapminder$poblacion / 1000000
gapminder$pbi_percap<- round(gapminder$pbi_per_capita, digits=1)
#Librerías
library(esquisse)
library(tidyverse)
library(gganimate)
library(emojifont)

#Animación creada en base a un scritp de ggplot creado con el paquete Esquisse
anim <- ggplot(data = gapminder) +
  aes(x = pib_per_capita, y = esperanza_de_vida , color = anio_exp, size = poblacion_en_millones) +
  geom_point() +
  labs(subtitle = "{closest_state}") +
  scale_colour_viridis_c(option  = "magma") +
  labs(title = "PBI per cápita y expectativa de vida a lo largo del tiempo",
       x = "PBI per cápita",
       y = "Esperanza de vida al nacer",
       caption = "Datos de Miércoles - RstatsES", 
       color="Expectativa",
       size="Población")+
  theme_minimal() +
  theme(legend.position = 'bottom') +
  facet_wrap(vars(continente), ncol = 5)+
  transition_states(anio)+
  scale_x_log10()


animate(anim, duration = 3)
anim_save("anim3.gif",animation = last_animation())





#----------------------------------Añado dataset cruzado Fútbol
#datos
mundiales <- readr::read_delim("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-04-10/partidos.txt",delim = "\t")
winners<- readxl::read_xlsx("C:/Users/karin/Documents/Rstudio/Datos de Miércoles/fútbol_info_adicional.xlsx")

#cantidad de goles por equipo por año
datos1<- mundiales %>% 
  group_by(equipo_1, anio) %>% 
  summarise(goles=sum(equipo_1_final))
datos1$eq_anio <- paste(datos1$anio, datos1$equipo_1)
datos2<- mundiales %>% 
  group_by(equipo_2, anio) %>% 
  summarise(goles=sum(equipo_2_final))
datos2$eq_anio <- paste(datos2$anio, datos2$equipo_2)

goles<-merge(datos1, datos2, by="eq_anio")
goles$total <- goles$goles.x+goles$goles.y
goles <- goles %>% select(anio=anio.x, pais=equipo_1, total)

winners<-rename(winners, "anio" = "eq_anio", "pais" = "winner")
winners$ganador<-"Ganador"

goles<-merge(goles, winners, by=c("anio","pais"), all.x=TRUE)


participantes<-data.frame(pais=unique(goles$pais),participo="participó")


#dataframe de méximos goleadores por año
maxgoleadores <- goles %>%
  group_by(anio) %>%
  arrange(desc(total)) %>%
  slice(1:1) %>%    
  select(pais, total, anio, ganador)
maxgoleadores$igual <- maxgoleadores$pais==maxgoleadores$ganador
maxgoleadores$Resultado <- ifelse(maxgoleadores$igual==TRUE, "Ganador del mundial", "No ganó" )

 

#Unir los datos

gapminder_mundiales <- merge(gapminder,goles, by=c("pais", "anio"), all.x=TRUE, all.y=FALSE)


gapminder_mundiales<- gapminder_mundiales %>% mutate(
                          Goles = if_else(
                            is.na(gapminder_mundiales$goles),
                            "No participó", 
                            if_else(
                              gapminder_mundiales$goles==0,
                              "0 goles",
                              if_else(
                                gapminder_mundiales$goles %in% c(1:5),
                                "1 a 5 goles",
                                if_else(
                                  gapminder_mundiales$goles %in% c(6:10),
                                  "6 a 10 goles",
                                  "más de 11 goles"
                                )))))
gapminder_mundiales$Goles<-as.factor(Goles)
                 
                     


anim <- ggplot(data = gapminder_mundiales) +
  aes(x = pib_per_capita, y = esperanza_de_vida , color = Goles, size = poblacion_en_millones) +
  geom_point() +
  scale_color_manual()
  labs(subtitle = "{closest_state}") +
  labs(title = "PBI per cápita y expectativa de vida a lo largo del tiempo",
       subtitle = "@karbartolome",
       x = "PBI per cápita",
       y = "Esperanza de vida al nacer",
       caption = "Datos de Miércoles - RstatsES", 
       color="Goles en mundial",
       size="Población")+
  theme_minimal() +
  theme(legend.position = 'bottom') +
  facet_wrap(vars(continente), nrow = 1)+
  transition_states(anio)+
  scale_x_log10()

animate(anim, duration = 5)
anim_save("anim3.gif",animation = last_animation())

##____ EMOJI


gapminder_participantes<-merge(gapminder,participantes,by="pais",all.x=TRUE)
gapminder_participantes <- gapminder_participantes %>% mutate(
    Mundiales = if_else(
      is.na(gapminder_participantes$participo),
      "No participó", 
      "Participó"))

gapminder_participantes$emoji<-emoji("soccer")


anim <- ggplot(data = gapminder_participantes) +
  aes(x = pib_per_capita, y = esperanza_de_vida , label=emoji,color = Mundiales) +
  geom_point() +
  geom_text(family="EmojiOne", size=6,show.legend = F)+
  scale_x_log10()+
  scale_colour_manual(values = c("No participó"="firebrick4","Participó"="deepskyblue4"))+
  labs(subtitle = "{closest_state}") +
  labs(title = "PBI per cápita y expectativa de vida a lo largo del tiempo",
       subtitle = "Datos de Miércoles - RstatsES",
       x = "Log(PBI per cápita)",
       y = "Esperanza de vida",
       caption = "@karbartolome", 
       color="Mundiales",
       size="Población")+
  theme_minimal() +
  facet_wrap(vars(continente), nrow = 1)+
  transition_states(anio)

animate(anim, duration = 4, height = 500, width= 1100)
anim_save("anim3.gif",animation = last_animation())


