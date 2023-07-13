library(sf)
library(spdep)
library(tmap)
library(tidyverse)

# Carga de estructura de trabajo registrado
df <- read_delim("datos/registro_trabajo_xproduoservicios.txt", delim = "\t") %>% 
  filter(depto != "Paraná")
dfvar <- read_delim("datos/variacion_total_2014-2022.txt", delim = "\t")


df$prorel <- df$produccion / sum(df$produccion)


# Carga de mapas
shape <- rgdal::readOGR(dsn = "mapa/pxdptodatosok.shp", verbose = FALSE)
shapeData <- sp::spTransform(shape, sp::CRS("+proj=longlat +datum=WGS84 +no_defs"))
shapeData <- sf::st_as_sf(shapeData, wkt = 'polygons', crs = st_crs(4326)) # make sf
shapeData <- rmapshaper::ms_simplify(shapeData, keep = 0.01, keep_shapes = TRUE) %>% 
  filter(provincia == "Entre Ríos")

mapdata <- sp::merge(x = shapeData, y = df, by.x = "departamen", by.y = "depto") %>% 
  filter(!is.na(prorel))

# Análisis univariado
boxplot(mapdata$prorel)

# Mapa
tm_shape(mapdata) + tm_fill(col="prorel",
                                style="quantile",
                                n=4,
                                palette="Greens") +
  tm_legend(outside=TRUE)

# Definición de vecinos
nb <- poly2nb(mapdata, queen=TRUE)

# Asignar peso a los vecinos
lw <- nb2listw(nb, style="W", zero.policy=TRUE)

# Analisis con pesos rezagados
per.lag <- lag.listw(lw, mapdata$prorel)
per.lag

plot(per.lag ~ mapdata$prorel, pch=16, asp=1)
text(df$prorel, (per.lag-0.02), labels=dfvar$depto)
M1 <- lm(per.lag ~ df$prorel)
abline(M1, col="blue")
coef(M1)[2]

# Inferencia
moran.test(mapdata$prorel,lw, alternative="greater")
