library(tidyverse)
library(scales)

setwd(paste0(getwd(), "/data/"))  ##  Añadir el path donde esté la carpeta cons los csv y los scripts

MAD <- read.csv("./vigencia_idealisto_df_MAD.csv")
point <- format_format(suffix = "€", preffix = "€", big.mark = ".", decimal.mark = ",", scientific = FALSE)

medias <- MAD %>% group_by(Distrito) %>% summarise(media = mean(Precio....m2.), sd = sd(Precio....m2.))


## theme
texto <- element_text(family = "Roboto Condensed", size = 12)
titulo <- element_text(face = "bold", size = 18)
margenY <- element_text(margin = margin(0,20,0,0))
margenX <- element_text(margin = margin(20,0,0,0))


distris <- medias$Distrito


for (p in distris) {
  mean <- mean(MAD$Precio....m2.[MAD$Distrito == p])
  sd <- sd(MAD$Precio....m2.[MAD$Distrito == p])
  print(mean)
  
  MAD$fuera[MAD$Distrito == p & MAD$Precio....m2. > mean+sd] <- TRUE
  MAD$fuera[MAD$Distrito == p & MAD$Precio....m2. <= mean+sd] <- FALSE
}

MAD_fuera <- subset(MAD, fuera == TRUE)
MAD_dentro <- subset(MAD, fuera == FALSE)

#medias_fuera <- MAD_fuera %>% group_by(Distrito) %>% summarise(N = n(), media = mean(Precio), media_m2 = mean(Precio....m2.), m2 = mean(Superficie))
medias_dentro <- MAD_dentro %>% group_by(Distrito) %>% summarise(N = n(), media = mean(Precio), media_m2 = mean(Precio....m2.), m2 = mean(Superficie))


MAD$precio_agrup <- NA
MAD$precio_agrup[MAD$Precio < 400] <- "<400€"
MAD$precio_agrup[MAD$Precio >= 400 & MAD$Precio < 600] <- "400-599€"
MAD$precio_agrup[MAD$Precio >= 600 & MAD$Precio < 800] <- "600-799€"
MAD$precio_agrup[MAD$Precio >= 800 & MAD$Precio < 1000] <- "800-999€"
MAD$precio_agrup[MAD$Precio >= 1000 & MAD$Precio < 1300] <- "1.000-1.299€"
MAD$precio_agrup[MAD$Precio >= 1300 & MAD$Precio < 1600] <- "1.300-1.599€"
MAD$precio_agrup[MAD$Precio >= 1600 & MAD$Precio < 2000] <- "1.600-1.999€"
MAD$precio_agrup[MAD$Precio >= 2000] <- ">2000€"



MAD$precio_agrup <- factor(MAD$precio_agrup, levels = c("<400€", "400-599€", "600-799€", "800-999€", "1.000-1.299€", "1.300-1.599€", "1.600-1.999€", ">2000€"), ordered = TRUE)

#MAD <- df
submuestra <- subset(MAD, vigencia <= 30)
submuestra$ofer_deman <- "Demanda"
MAD$ofer_deman <- "Oferta"
MAD <- rbind(submuestra, MAD)
MAD %>% group_by(ofer_deman) %>% summarise(N = n(), mediana = median(Precio))


MAD_dentro$Habitaciones[MAD_dentro$Habitaciones == 0] <- NA
medias_distritos <- MAD_dentro %>% group_by(Distrito) %>% summarise(N = n(), precio_mediano = median(Precio), precio_medio = mean(Precio), precio_medio_m2 = mean(Precio....m2.), precio_habitacion = mean(precio_habita, na.rm = TRUE), superficie_media = mean(Superficie), habitaciones_media = mean(Habitaciones, na.rm = TRUE))



#Gráfico columnas

## El 71% de la demanda de vivienda en Madrid se sitúa por debajo de los 1600€ al mes pero solo el 57% de la oferta está en ese rango de precios.
## El 32% de la oferta está por encima de los 2.000€ pero solo el 19% de la oferta se acomoda a ese nivel de precios.
precio_agrup <- MAD %>% group_by(ofer_deman, precio_agrup) %>% summarise(N = n()) %>% mutate(pct = N/sum(N))

p <- ggplot(precio_agrup, aes(x = precio_agrup, y = pct, group = ofer_deman))
p + geom_col(aes(fill = ofer_deman), 
             alpha = 0.5, width = 1, position = position_identity()) +
  scale_y_continuous(labels = percent) + 
  labs( title = "Oferta vs. demanda en el mercado del alquiler en Madrid",
        x = "Precio",
        y = "% sobre el total", caption = "Fuente: Idealista     //     @hmeleiros",
        fill = NULL, subtitle = "Llamo demanda a los anuncios que se dan de baja del portal inmobiliario Idealista en 30 días o menos y \noferta a la totalidad de los anuncios. Extracción de 9.282 anuncios de la web de Idealista el día 30 de enero de 2018.") +
  theme(text = texto,
        plot.title = titulo,
        axis.title.x = margenX,
        axis.title.y = margenY,
        plot.caption = margenX)

