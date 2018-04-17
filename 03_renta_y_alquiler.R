library(readr)
library(ggthemes)
library(tidyverse)
library(scales)


setwd(paste0(getwd(), "/data/"))  ##  Añadir el path donde esté la carpeta cons los csv y los scripts


## Importo las medias de renta de hogar (2014) y formateo los datos
renta <- read_delim("./renta_barrios.csv", 
                            ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                        grouping_mark = "."), trim_ws = TRUE)

q <- str_split(renta$barrio, pattern = "\\.", simplify = TRUE)

q <- as.data.frame(q)

renta <- bind_cols(q, renta)

colnames(renta)[1] <- "cod"
colnames(renta)[2] <- "barrio_"

renta$barrio_ <- as.character(renta$barrio_)

renta[1,1] <- NA
renta[1,2] <- "Madrid"

renta <- renta[-152,]
renta <- renta[-151,]

renta$tipo[nchar(as.character(renta$cod)) == 2] <- "Distrito"
renta$tipo[nchar(as.character(renta$cod)) == 3] <- "Barrio"
renta$tipo[is.na(renta$cod)] <- "Ciudad"

renta$barrio_ <- str_trim(renta$barrio_, "left")


## Importo los datos de Idealisto y formateo los datos

MAD <- read.csv("./alquiler_2018-03-20_Madrid.csv")
MAD <- unique(MAD)

point <- format_format(suffix = "€", preffix = "€", big.mark = ".", decimal.mark = ",", scientific = FALSE)



## theme
texto <- element_text(family = "Roboto Condensed", size = 12)
titulo <- element_text(face = "bold", size = 18)
margenY <- element_text(margin = margin(0,20,0,0))
margenX <- element_text(margin = margin(20,0,0,0))


MAD$Barrio <- str_remove(MAD$Barrio, "Barrio")
MAD$Barrio <- str_trim(MAD$Barrio, "both")
MAD$Barrio <- str_remove(MAD$Barrio, "Urb\\.")
MAD$Barrio <- str_remove(MAD$Barrio, "^  de ")
MAD$Barrio <- str_remove(MAD$Barrio, "^ ")

MAD$Barrio <- str_replace(MAD$Barrio, "Chueca-Justicia", "Justicia")
MAD$Barrio <- str_replace(MAD$Barrio, "Malasaña-Universidad", "Universidad")
MAD$Barrio <- str_replace(MAD$Barrio, "Huertas-Cortes", "Cortes")
MAD$Barrio <- str_replace(MAD$Barrio, "Lavapiés-Embajadores", "Embajadores")
MAD$Barrio <- str_trim(MAD$Barrio, "both")
MAD$Barrio <- str_replace(MAD$Barrio, "12 de Octubre-Orcasur", "Orcasur")
MAD$Barrio <- str_replace(MAD$Barrio, "Arroyo del Fresno", "Mirasierra")
MAD$Barrio <- str_replace(MAD$Barrio, "Ventilla-Almenara", "Almenara")
MAD$Barrio <- str_replace(MAD$Barrio, "Ensanche de Vallecas - La Gavia", "Casco Histórico de Vallecas")
MAD$Barrio <- str_replace(MAD$Barrio, "Las Letras", "Cortes")
MAD$Barrio <- str_replace(MAD$Barrio, "Las Tablas", "Valverde")
MAD$Barrio <- str_replace(MAD$Barrio, "Tres Olivos - Valverde", "Valverde")
MAD$Barrio <- str_replace(MAD$Barrio, "Montecarmelo", "El Goloso")
MAD$Barrio <- str_replace(MAD$Barrio, "Águilas", "Las Águilas")
MAD$Barrio <- str_replace(MAD$Barrio, "Bernabéu-Hispanoamérica", "Hispanoamérica")
MAD$Barrio <- str_replace(MAD$Barrio, "Buena Vista", "Buenavista")
MAD$Barrio <- str_replace(MAD$Barrio, "Campo de las Naciones-Corralejos", "Corralejos")
MAD$Barrio <- str_replace(MAD$Barrio, "Los Cármenes", "Cármenes")
MAD$Barrio <- str_replace(MAD$Barrio, "Cuzco-Castillejos", "Castillejos")
MAD$Barrio <- str_replace(MAD$Barrio, "Sanchinarro", "Valdefuentes")
MAD$Barrio <- str_replace(MAD$Barrio, "Nuevos Ministerios-Ríos Rosas", "Ríos Rosas")
MAD$Barrio <- str_replace(MAD$Barrio, "Puerta del Ángel", "Puerta del Angel")
MAD$Barrio <- str_replace(MAD$Barrio, "Palomeras sureste", "Palomeras Sureste")
MAD$Barrio <- str_replace(MAD$Barrio, "Valdebebas - Valdefuentes", "Valdefuentes")
MAD$Barrio <- str_replace(MAD$Barrio, "Virgen del Cortijo - Manoteras", "Valdefuentes")
MAD$Barrio <- str_replace(MAD$Barrio, "Conde Orgaz-Piovera", "Piovera")

#Las Letras <- Cortes

#Las Tablas <- Valverde
#Tres Olivos - Valverde
#Montecarmelo <- El Goloso

#Águilas - Las Águilas

#Bernabéu-Hispanoamérica
#Buena Vista <- Buenavista

#Campo de las Naciones-Corralejos
#Los Cármenes <- Cármenes

#Cuzco-Castillejos
#Sanchinarro <- Valdefuentes

#Nuevos Ministerios-Ríos Rosas
#Puerta del Ángel <- Puerta del Angel

#Palomeras sureste <- Palomeras Sureste
#Valdebebas - Valdefuentes
#Virgen del Cortijo - Manoteras <- Valdefuentes
#Conde Orgaz-Piovera



## Ejecuto la correción estadística para eliminar anuncios muy por encima de los precios medios (aquellos una SD por encima de la media).

medias <- MAD %>% group_by(Distrito) %>% summarise(media = mean(Precio_m2, na.rm = TRUE), sd = sd(Precio_m2, na.rm = TRUE))
medias_barris_mediana <- MAD %>% group_by(Barrio, Distrito) %>% summarise(N = n(), media = median(Precio, na.rm = TRUE), media_m2 = median(Precio_m2, na.rm = TRUE), m2 = median(Superficie, na.rm = TRUE))
medias_barris_media <- MAD %>% group_by(Barrio, Distrito) %>% summarise(N = n(), media = mean(Precio, na.rm = TRUE), media_m2 = mean(Precio_m2, na.rm = TRUE), m2 = mean(Superficie, na.rm = TRUE))

barris <- medias_barris$Barrio
distris <- medias$Distrito

for (p in barris) {
  mean <- mean(MAD$Precio_m2[MAD$Barrio == p], na.rm = TRUE)
  sd <- sd(MAD$Precio_m2[MAD$Barrio == p], na.rm = TRUE)
  print(mean)
  
  MAD$fuera[MAD$Barrio == p & MAD$Precio_m2 > mean+sd] <- TRUE
  MAD$fuera[MAD$Barrio == p & MAD$Precio_m2 <= mean+sd] <- FALSE
}

MAD_fuera_barris <- subset(MAD, fuera == TRUE)
MAD_dentro_barris <- subset(MAD, fuera == FALSE)

medias_fuera <- MAD_fuera_barris %>% group_by(Barrio) %>% summarise(N = n(), media = median(Precio, na.rm = TRUE), media_m2 = median(Precio_m2, na.rm = TRUE), m2 = median(Superficie, na.rm = TRUE))
medias_dentro <- MAD_dentro_barris %>% group_by(Barrio) %>% summarise(N = n(), media = median(Precio, na.rm = TRUE), media_m2 = median(Precio_m2, na.rm = TRUE), m2 = median(Superficie, na.rm = TRUE))
###



## Fusiono y hago los calculos del alquiler anual

i <- merge(renta, medias_barris_mediana, by.x = "barrio_", by.y ="Barrio")
i <- subset(i, tipo != "Distrito")
i$media12 <- i$media * 12
i$pct_vivienda <- i$media12/i$renta
i <- subset(i, N >= 80)
i$centro[i$Distrito == "Centro"] <- "Centro"
i$centro[i$Distrito != "Centro"] <- "Otros distritos"
i$centro[i$Distrito == "Tetuán"] <- "Tetuán"
i$centro[i$Distrito == "Chamberí"] <- "Chamberí"



## Visualizo

p <- ggplot(i, aes(x = reorder(barrio_, pct_vivienda), y = pct_vivienda))
p + geom_col(aes(fill = centro)) + 
  coord_flip() +
  scale_y_continuous(labels = percent) +
  labs(title = "Porcentaje que supone el alquiler mediano anual de una vivienda \nsobre la renta media de los hogares en 2014",
       subtitle = "Los precios medianos del alquiler corresponden a las medianas de la oferta total en cada barrio de \nuna extracción de 10.296 anuncios de Idealista. Solo se han considerado los barrios con más de 80 anuncios.",
       x = "",
       y = "% que supone el alquiler mediano anual en 2018 \nsobre la renta media de los hogares en 2014", 
       fill = NULL,
       caption = "Fuente: Idealista y Ayuntamiento de Madrid     //     @hmeleiros") +
  theme_minimal(base_family = "Roboto Condensed") + 
  theme(plot.title = element_text(face = "bold"),
        axis.title.x = element_text(margin = margin(20,0,0,0), size = 9),
        plot.caption = element_text(margin = margin(20,0,0,0))) +
  scale_fill_wsj(palette = "rgby") +
  geom_text(aes(label = percent(pct_vivienda)), family = "Roboto Condensed", size = 3, nudge_y = 0.03)

