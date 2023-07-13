library(tidyverse)

# ====================================
# PRIMER CLUSTER DIFERENCIA MANHATTAN, WARD, TODOS LOS SECTORES Y PROVINCIAS. 
# ====================================

# CARGA Y TRANSFORMACIÓN DEL DATAFRAME
dft <- read_delim("datos/sectorxdepto.txt", delim = "\t")
depto <- dft$depto

# Porcentaje
dftp <- round(dft[2:20] / rowSums(dft[2:20]), 3) 
dftp <- data.frame(dftp)

# ESCALAMIENTO DE LAS VARIABLES
rob_scale = function(x) (x - median(x)) / IQR(x)
rownames(dftp) <- dft$depto

dftp = dftp %>%
  select_if(is.numeric) %>% 
  # scale(center=T, scale=T) %>% # estandarizacion media-desvio
  mutate_all(rob_scale) %>% # normalizacion
  as.data.frame()

# EVALUACIÓN DE TENDENCIA AL AL AGRUPAMIENTO
hopkins = factoextra::get_clust_tendency(dftp, n = 14, seed = 321)
cat("Hopkins =", hopkins$hopkins_stat)

# CANTIDAD DE CLUSTERS RECOMENDADOS PARA METODO WARD Y MANHATTAN
# gap stat
factoextra::fviz_nbclust(dftp,
                         FUNcluster = factoextra::hcut, 
                         method = "gap_stat",
                         k.max = 10,
                         nboot = 50,
                         diss = dist(dftp, method = "manhattan"),
                         hc_method = "ward.D")

# silhouette
factoextra::fviz_nbclust(dftp,
                         FUNcluster = factoextra::hcut, 
                         method = "silhouette",
                         k.max = 10,
                         nboot = 50,
                         diss = dist(dftp, method = "manhattan"),
                         hc_method = "ward.D")

# wss (suma de cuadrados totales hacia adentro)
factoextra::fviz_nbclust(dftp,
                         FUNcluster = factoextra::hcut, 
                         method = "wss",
                         k.max = 10,
                         nboot = 50,
                         diss = dist(dftp, method = "manhattan"),
                         hc_method = "ward.D")


# Con la distancia de manhattan y el criterio de ward, se seleccionan 3 clusters 

# DENDOGRAMA (CLUSTER JERÁRQUICO)
rownames(dftp) <- dft$depto
hc = amap::hcluster(dftp, method = "manhattan", link = "ward", doubleprecision = TRUE)
factoextra::fviz_dend(hc, horiz = T, k = 3, type = "phylogenic")

# CLUSTER EN DATA FRAME 
hc = factoextra::hcut(dftp, k = 3, hc_method = "ward.D",
                      hc_metric = "manhattan", stand = F)

# Cargar a df
dftp_cluster = dftp %>% 
  mutate(cluster = factor(hc$cluster))
dftp_cluster$depto <- depto

# DESCRIPCIÓN DE LOS CLUSTER - GRAFICO DE DENSIDAD Y BOXPLOT POR VARIABLE
gdat = dftp_cluster %>% 
  pivot_longer(
    -all_of(c("depto", "cluster")), names_to="variable", values_to="value")

# densidades por variable
library(ggridges)
plt_density = 
  ggplot(gdat, aes(x=value, y=variable, color=cluster, point_color=cluster
                   , fill=cluster)) +
  ggridges::geom_density_ridges(
    alpha=0.5, scale=1
    ,jittered_points=T, position=position_points_jitter(height=0)
    ,point_shape="|", point_size=2
  ) +
  theme_minimal() +
  NULL
plt_density

plt_boxplot =
  ggplot(gdat, aes(x=variable, y=value, color=cluster)) +
  facet_wrap(~cluster, ncol=2, scales="free_y") +
  geom_boxplot() +
  theme_minimal() +
  theme(axis.text.x=element_text(angle=-90)) +
  NULL

print(plt_boxplot)

# MAPA DE CLUSTERS
# Carga de mapa
shape <- rgdal::readOGR(dsn = "mapa/pxdptodatosok.shp", verbose = FALSE)
shapeData <- sp::spTransform(shape, sp::CRS("+proj=longlat +datum=WGS84 +no_defs"))
shapeData <- sf::st_as_sf(shapeData, wkt = 'polygons', crs = st_crs(4326)) # make sf
shapeData <- rmapshaper::ms_simplify(shapeData, keep = 0.01, keep_shapes = TRUE) %>% 
  filter(provincia == "Entre Ríos")

shapeData <- sp::merge(shapeData, dftp_cluster, by.x = "departamen", by.y = "depto")

# Colores
dispal <- colorFactor("Spectral", domain = shapeData$cluster, na.color = "black")

# Texto
textos <- paste0("<b>", shapeData$departamen, ":</b>
                 <br> Cluster ",
                 round(as.numeric(shapeData$cluster)))

# Mapa
library(leaflet)
leaflet(shapeData) %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  setView(-59.5, -32, 7.2) %>% 
  addPolygons(weight = 2,
              color = "black",
              dashArray = 1,
              fillOpacity = 0.8,
              fillColor = ~dispal(shapeData$cluster),
              label = ~lapply(as.list(textos), htmltools::HTML)) %>% 
  addLegend(pal = dispal, values = seq(1, 3, 1), opacity = 0.7,
            title = "Cluster",
            position = "bottomright")





# ====================================
# SEGUNDO CLUSTER DIFERENCIA EUCLIDEANA, ENCADENAMIENTO SIMPLE Y BASE COMPLETA
# ====================================

# CARGA Y TRANSFORMACIÓN DEL DATAFRAME
dft <- read_delim("datos/sectorxdepto.txt", delim = "\t")
depto <- dft$depto

# Porcentaje
dftp <- round(dft[2:20] / rowSums(dft[2:20]), 3) 
dftp <- data.frame(dftp)

# ESCALAMIENTO DE LAS VARIABLES
rob_scale = function(x) (x - median(x)) / IQR(x)
rownames(dftp) <- dft$depto

dftp = dftp %>%
  select_if(is.numeric) %>% 
  # scale(center=T, scale=T) %>% # estandarizacion media-desvio
  mutate_all(rob_scale) %>% # normalizacion
  as.data.frame()

# EVALUACIÓN DE TENDENCIA AL AL AGRUPAMIENTO
hopkins = factoextra::get_clust_tendency(dftp, n = 14, seed = 321)
cat("Hopkins =", hopkins$hopkins_stat)

# CANTIDAD DE CLUSTERS RECOMENDADOS PARA METODO COMPLETO Y EUCLIDEO
# gap stat
factoextra::fviz_nbclust(dftp,
                         FUNcluster = factoextra::hcut, 
                         method = "gap_stat",
                         k.max = 10,
                         nboot = 50,
                         diss = dist(dftp, method = "euclidean"),
                         hc_method = "complete")

# silhouette
factoextra::fviz_nbclust(dftp,
                         FUNcluster = factoextra::hcut, 
                         method = "silhouette",
                         k.max = 10,
                         nboot = 50,
                         diss = dist(dftp, method = "euclidean"),
                         hc_method = "complete")

# wss (suma de cuadrados totales hacia adentro)
factoextra::fviz_nbclust(dftp,
                         FUNcluster = factoextra::hcut, 
                         method = "wss",
                         k.max = 10,
                         nboot = 50,
                         diss = dist(dftp, method = "euclidean"),
                         hc_method = "complete")

# Se seleccionan 3 clusters 

# DENDOGRAMA (CLUSTER JERÁRQUICO)
rownames(dftp) <- dft$depto
hc = amap::hcluster(dftp, method = "manhattan", link = "ward.D2", doubleprecision = TRUE)
factoextra::fviz_dend(hc, horiz = T, k = 3, type = "phylogenic")

# CLUSTER EN DATA FRAME 
hc = factoextra::hcut(dftp, k = 3, hc_method = "ward.D",
                      hc_metric = "manhattan", stand = F)

# Cargar a df
dftp_cluster = dftp %>% 
  mutate(cluster = factor(hc$cluster))
dftp_cluster$depto <- depto

# DESCRIPCIÓN DE LOS CLUSTER - GRAFICO DE DENSIDAD Y BOXPLOT POR VARIABLE
gdat = dftp_cluster %>% 
  pivot_longer(
    -all_of(c("depto", "cluster")), names_to="variable", values_to="value")

# densidades por variable
library(ggridges)
plt_density = 
  ggplot(gdat, aes(x=value, y=variable, color=cluster, point_color=cluster
                   , fill=cluster)) +
  ggridges::geom_density_ridges(
    alpha=0.5, scale=1
    ,jittered_points=T, position=position_points_jitter(height=0)
    ,point_shape="|", point_size=2
  ) +
  theme_minimal() +
  NULL
plt_density

plt_boxplot =
  ggplot(gdat, aes(x=variable, y=value, color=cluster)) +
  facet_wrap(~cluster, ncol=2, scales="free_y") +
  geom_boxplot() +
  theme_minimal() +
  theme(axis.text.x=element_text(angle=-90)) +
  NULL

print(plt_boxplot)
