# Datos de miércoles - 
library(readtext)
library(tm)
library(stringr)
library(wordcloud)
library(dplyr)
library(ggwordcloud)

# -------DATOS -------

arg_fernandez_2019_original <- paste(
  readLines("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-09-04/argentina_frente-de-todos_fernandez_2019.txt", encoding="UTF-8"), 
  collapse = " ")
arg_macri_2019_original <- paste(
  readLines("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-09-04/argentina_juntos-por-el-cambio_macri_2019.txt", encoding="UTF-8"), 
  collapse = " ")

# ------ LIMPIEZA ------------
arg_fernandez_2019 <- gsub(pattern="\\W", replace = " ", arg_fernandez_2019_original)
arg_fernandez_2019 <- gsub(pattern="\\d", replace = " ", arg_fernandez_2019)
arg_fernandez_2019 <- tolower(arg_fernandez_2019)
arg_fernandez_2019 <- removeWords(arg_fernandez_2019, stopwords(kind="spanish"))
arg_fernandez_2019 <- gsub(pattern="\\b[a-z]\\b{1}", replace = " ", arg_fernandez_2019)
arg_fernandez_2019 <- stripWhitespace(arg_fernandez_2019)
arg_fernandez_2019 <- unlist(str_split(arg_fernandez_2019, pattern="\\s+"))
wordcloud(arg_fernandez_2019, min.freq = 10)

arg_fernandez_2019 <- data.frame(word=arg_fernandez_2019)
arg_fernandez_2019_grouped <- arg_fernandez_2019 %>% count(word, sort=TRUE) %>% select(word, cantidad_fernandez=n)
ggplot(arg_fernandez_2019_grouped[1:100,],aes(label=word, size=cantidad_fernandez))+
  geom_text_wordcloud()

arg_macri_2019 <- gsub(pattern="\\W", replace = " ", arg_macri_2019_original)
arg_macri_2019 <- gsub(pattern="\\d", replace = " ", arg_macri_2019)
arg_macri_2019 <- tolower(arg_macri_2019)
arg_macri_2019 <- removeWords(arg_macri_2019, stopwords(kind="spanish"))
arg_macri_2019 <- gsub(pattern="\\b[a-z]\\b{1}", replace = " ", arg_macri_2019)
arg_macri_2019 <- stripWhitespace(arg_macri_2019)
arg_macri_2019 <- unlist(str_split(arg_macri_2019, pattern="\\s+"))

wordcloud(arg_macri_2019)

arg_macri_2019 <- data.frame(word=arg_macri_2019)
arg_macri_2019_grouped <- arg_macri_2019 %>% count(word, sort=TRUE) %>% select(word,cantidad_macri=n)

arg_macri_2019_grouped$macri <-
  arg_macri_2019_grouped$cantidad_macri / sum(arg_macri_2019_grouped$cantidad_macri) * 100
sum(arg_macri_2019_grouped$macri)

arg_fernandez_2019_grouped$fernandez <-
  arg_fernandez_2019_grouped$cantidad_fernandez / sum(arg_fernandez_2019_grouped$cantidad_fernandez) * 100
sum(arg_fernandez_2019_grouped$fernandez)

ggplot(arg_macri_2019_grouped[1:150,],aes(label=word, size=cantidad_macri))+
  geom_text_wordcloud()

grouped <- merge(arg_macri_2019_grouped[1:80,], arg_fernandez_2019_grouped[1:80,], by="word",all.x=TRUE, all.y=TRUE)


grouped <- grouped %>% mutate(tipo=case_when(
  !is.na(grouped$cantidad_macri) & !is.na(grouped$cantidad_fernandez) ~ "Ambos",
  !is.na(grouped$cantidad_macri) & is.na(grouped$cantidad_fernandez) ~ "Macri",
  is.na(grouped$cantidad_macri) & !is.na(grouped$cantidad_fernandez) ~ "Fernandez"))

grouped$macri <- ifelse(is.na(grouped$macri),0,grouped$macri)
grouped$fernandez <- ifelse(is.na(grouped$fernandez),0,grouped$fernandez)

grouped <- grouped %>% mutate(Repeticion=case_when(
  macri < fernandez ~ fernandez,
  macri > fernandez  ~ macri, 
  fernandez == macri ~ macri
))

plot<-ggplot(grouped,aes(label=word, size=Repeticion, color=tipo))+
  geom_text_wordcloud(show.legend = TRUE, 
                      area_corr = TRUE 
                      )+
  theme_minimal()+
  labs(title="Programas de los principales candidatos argentinos", 
       subtitle = "80 palabras más utilizadas en cada programa", 
       caption="#Datosdemiercoles - @karbartolome", 
       color="¿En qué programa?", 
       size="Repetición (%)")+
  scale_color_viridis_d()+
  # scale_color_brewer(palette='Set1')+
  theme(legend.position = "bottom", 
        legend.direction = "vertical")+
  scale_size_area(max_size = 8)+
  guides(size=guide_legend(ncol=4))

ggsave("plot.jpg", plot=last_plot())

       
