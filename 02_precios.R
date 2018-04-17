library(readr)
library(ggthemes)
library(tidyverse)
library(scales)


setwd(paste0(getwd(), "/data/"))  ##  Añadir el path donde esté la carpeta cons los csv y los scripts


## Importo los datos de Idealisto y formateo los datos
MAD <- read.csv("./alquiler_2018-03-20_Madrid.csv")
MAD <- unique(MAD)


euro_french_format <- function(x, ...) currency_format(symbol_currency = "€/m²", symbol_position = "after", symbol_spacing = "single", separator_thousand = ".", separator_decimal = ",")
euro_french <- euro_french_format()
euro_french_code_format <- function(x, ...) currency_format(symbol_currency = "EUR", symbol_position = "after", symbol_spacing = "single", separator_thousand = ".", separator_decimal = ",")
euro_french_code <- euro_french_code_format()


## theme
texto <- element_text(family = "Roboto Condensed", size = 12)
titulo <- element_text(face = "bold", size = 18)
margenY <- element_text(margin = margin(0,20,0,0))
margenX <- element_text(margin = margin(20,0,0,0))


## Ejecuto la correción estadística para eliminar anuncios muy por encima de los precios medios (aquellos una SD por encima de la media).

medias <- MAD %>% group_by(Distrito) %>% summarise(media = mean(Precio_m2), sd = sd(Precio_m2))

distris <- medias$Distrito


for (p in distris) {
  mean <- mean(MAD$Precio_m2[MAD$Distrito == p])
  sd <- sd(MAD$Precio_m2[MAD$Distrito == p])
  print(mean)
  
  MAD$fuera[MAD$Distrito == p & MAD$Precio_m2 > mean+sd] <- TRUE
  MAD$fuera[MAD$Distrito == p & MAD$Precio_m2 <= mean+sd] <- FALSE
}

MAD_fuera <- subset(MAD, fuera == TRUE)
MAD_dentro <- subset(MAD, fuera == FALSE)

#medias_fuera <- MAD_fuera %>% group_by(Distrito) %>% summarise(N = n(), media = mean(Precio), media_m2 = mean(Precio_m2), m2 = mean(Superficie))
medias_dentro <- MAD_dentro %>% group_by(Distrito) %>% summarise(N = n(), media = mean(Precio), media_m2 = mean(Precio_m2), m2 = mean(Superficie))

####



## Visualizo el resultado



p <- ggplot(medias_dentro, aes(x = reorder(Distrito, media_m2), y = media_m2))
p + geom_col(fill = "#d5695d") + 
  coord_flip() +
  theme_minimal(base_family = "Roboto Condensed") + 
  theme(plot.title = element_text(face = "bold"),
        axis.title.x = element_text(margin = margin(20,0,0,0), size = 9),
        plot.caption = element_text(margin = margin(20,0,0,0))) +
  geom_text(aes(label = euro_french(round(media_m2, digits = 2))), family = "Roboto Condensed", color = "white", size = 3, nudge_y = -2) +
  scale_y_continuous(labels = euro_french) +
  labs(x = NULL,
       y = "Precio/superficie",
       title = "Precio del alquiler por distritos en la ciudad de Madrid",
       subtitle = "Elaborado en base a una extracción de 9.282 anuncios de Idealista a los que se le ha aplicado una correción \nestadística eliminando de la muestra los anuncios una desviación típica por encima de la media.")


