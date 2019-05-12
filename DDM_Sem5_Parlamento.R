#Participación de mujeres en el parlamento - Serie Histórica
#install.packages("rvest")
library(rvest)
library(XML)
library(ggplot2)
library(dplyr)
library(ggpol)
library(countrycode)
library(gganimate)


setwd("C:/Users/karin/Documents/Rstudio/Datos de Miércoles/DDM_Sem5/Base Parlamento")

#Web scrapping de datos históricos
urls<-c("http://archive.ipu.org/wmn-e/arc/classif010197.htm", 
        "http://archive.ipu.org/wmn-e/arc/classif250198.htm",
        "http://archive.ipu.org/wmn-e/arc/classif010199.htm",
        "http://archive.ipu.org/wmn-e/arc/classif250100.htm",
        "http://archive.ipu.org/wmn-e/arc/classif300101.htm",
        "http://archive.ipu.org/wmn-e/arc/classif040202.htm",
        "http://archive.ipu.org/wmn-e/arc/classif310103.htm",
        "http://archive.ipu.org/wmn-e/arc/classif300104.htm",
        "http://archive.ipu.org/wmn-e/arc/classif310105.htm",
        "http://archive.ipu.org/wmn-e/arc/classif310106.htm",
        "http://archive.ipu.org/wmn-e/arc/classif310107.htm",
        "http://archive.ipu.org/wmn-e/arc/classif310108.htm",
        "http://archive.ipu.org/wmn-e/arc/classif310109.htm",
        "http://archive.ipu.org/wmn-e/arc/classif310110.htm",
        "http://archive.ipu.org/wmn-e/arc/classif310111.htm",
        "http://archive.ipu.org/wmn-e/arc/classif310112.htm",
        "http://archive.ipu.org/wmn-e/arc/classif010113.htm",
        "http://archive.ipu.org/wmn-e/arc/classif010114.htm",
        "http://archive.ipu.org/wmn-e/arc/classif010115.htm",
        "http://archive.ipu.org/wmn-e/arc/classif010116.htm",
        "http://archive.ipu.org/wmn-e/arc/classif010117.htm",
        "http://archive.ipu.org/wmn-e/arc/classif010118.htm",
        "http://archive.ipu.org/wmn-e/arc/classif010119.htm"
)
anios<-c(seq(1997,2019,by=1))
datos<-data.frame(anios,urls)
datos$urls <- as.character(datos$urls)


#Armado de bases por año
i=1
for (i in 1:nrow(datos)) {
  parlamento<- datos$urls[i]
  parlamento.tabla<-readHTMLTable(parlamento, stringsAsFactors = F)
  parlamento.tabla <- data.frame(parlamento.tabla[[3]])
  parlamento.tabla <- parlamento.tabla[2:nrow(parlamento.tabla),] 
  parlamento.tabla_2 <- parlamento.tabla %>%
    mutate(anio=datos$anios[i]) %>%
    rename(
      "Pais" = "V2",
      "Elecciones_camara_baja_o_unica" =
        "V3",
      "Asientos_camara_baja_o_unica" =
        "V4",
      "Mujeres_camara_baja" =
        "V5",
      "Porcentaje_mujeres_camara_baja" =
        "V6",
      "Elecciones_camara_alta_o_senado" =
        "V7",
      "Asientos_camara_alta_o_senado" =
        "V8",
      "Mujeres_camara_alta_o_senado" =
        "V9",
      "Porcentaje_mujeres_camara_alta_o_senado" =
        "V10") 
  write.csv(parlamento.tabla_2, file = paste0(datos$anios[i], "parlamento.csv"), row.names = FALSE)  
}

bases<-data.frame(nombre=paste("p",anios, sep=""))
archivos <- data.frame(archivo=paste(anios, "parlamento.csv", sep=""))


#Union de bases
p1997 <- read.csv("1997parlamento.csv", stringsAsFactors = FALSE)
p1998 <- read.csv("1998parlamento.csv", stringsAsFactors = FALSE)
p1999 <- read.csv("1999parlamento.csv", stringsAsFactors = FALSE)
p2000 <- read.csv("2000parlamento.csv", stringsAsFactors = FALSE)
p2001 <- read.csv("2001parlamento.csv", stringsAsFactors = FALSE)
p2002 <- read.csv("2002parlamento.csv", stringsAsFactors = FALSE)
p2003 <- read.csv("2003parlamento.csv", stringsAsFactors = FALSE)
p2004 <- read.csv("2004parlamento.csv", stringsAsFactors = FALSE)
p2005 <- read.csv("2005parlamento.csv", stringsAsFactors = FALSE)
p2006 <- read.csv("2006parlamento.csv", stringsAsFactors = FALSE)
p2007 <- read.csv("2007parlamento.csv", stringsAsFactors = FALSE)
p2008 <- read.csv("2008parlamento.csv", stringsAsFactors = FALSE)
p2009 <- read.csv("2009parlamento.csv", stringsAsFactors = FALSE)
p2010 <- read.csv("2010parlamento.csv", stringsAsFactors = FALSE)
p2011 <- read.csv("2011parlamento.csv", stringsAsFactors = FALSE)
p2012 <- read.csv("2012parlamento.csv", stringsAsFactors = FALSE)
p2013 <- read.csv("2013parlamento.csv", stringsAsFactors = FALSE)
p2014 <- read.csv("2014parlamento.csv", stringsAsFactors = FALSE)
p2015 <- read.csv("2015parlamento.csv", stringsAsFactors = FALSE)
p2016 <- read.csv("2016parlamento.csv", stringsAsFactors = FALSE)
p2017 <- read.csv("2017parlamento.csv", stringsAsFactors = FALSE)
p2018 <- read.csv("2018parlamento.csv", stringsAsFactors = FALSE)
p2019 <- read.csv("2019parlamento.csv", stringsAsFactors = FALSE)

p2019 <- p2019[2:nrow(p2019),]
p2018 <- p2018[2:nrow(p2018),]
p2017 <- p2017[2:nrow(p2017),]
p2016 <- p2016[2:nrow(p2016),]
p2015 <- p2015[2:nrow(p2015),]
p2014 <- p2014[2:nrow(p2014),]
p2013 <- p2013[2:nrow(p2013),]
p2012 <- p2012[2:nrow(p2012),]
p2011 <- p2011[2:nrow(p2011),]
p2010 <- p2010[2:nrow(p2010),]

parlamento_serie<-rbind(p1997,
                        p1998,
                        p1999,
                        p2000,
                        p2001,
                        p2002,
                        p2003,
                        p2004,
                        p2005,
                        p2006,
                        p2007,
                        p2008,
                        p2009,
                        p2010,
                        p2011,
                        p2012,
                        p2013,
                        p2014,
                        p2015,
                        p2016,
                        p2017,
                        p2018,
                        p2019)





#Analisis de los datos
#Porcentaje de partipación femenina en el parlamento en países latinoaméricanos
camara_baja <- parlamento_serie %>%
  select(-V1) %>%
  select(anio,
         pais=Pais,
         asientos = Asientos_camara_baja_o_unica,
         mujeres=Mujeres_camara_baja,
         Porcentaje_mujeres_camara_baja) 


camara_baja$asientos <- as.numeric(camara_baja$asientos)
camara_baja$mujeres <- as.numeric(camara_baja$mujeres)
camara_baja$hombres <- camara_baja$asientos-camara_baja$mujeres
camara_baja$porcentaje <- round((camara_baja$mujeres/camara_baja$asientos), 2)

paises<-data.frame(unique(camara_baja$pais))

#Para continentes
codes<-codelist
str(codelist)

codes<-codelist %>% select(pais=country.name.en, region, continent)

camara_baja_regiones <- merge(camara_baja, codes, by="pais", all.x=TRUE, all.y=FALSE)



#Boxplot
particip_continentes<- camara_baja_regiones %>%
  filter(!is.na(continent)) %>%
  ggplot(aes(x = continent, y = porcentaje, color=continent)) +
  geom_boxplot()+
  labs(
    x = "Continente",
    y = "Porcentaje de mujeres en cámara baja",
    title = "Participación de mujeres en el parlamento (cámara baja o única)",
    subtitle = "Año: {closest_state}"
  ) +
  theme(
    legend.position = "none")+
  transition_states(anio)

anim_save("Participación camara baja.gif",animation = particip_continentes, duration=12)



