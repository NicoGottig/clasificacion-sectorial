library(tidyverse)

puestos <- read.csv("datos/puestos_depto_tot_emp_por_clae2.csv") %>% 
  filter(id_provincia_indec == 30) %>% 
  select(-id_provincia_indec)

clae <- read.csv("datos/diccionario_clae2.csv")
prov <- read.csv("datos/diccionario_cod_depto.csv")

df <- left_join(puestos, prov, by = "codigo_departamento_indec")
df <- left_join(df, clae, by = "clae2")
df <- df %>% 
  mutate(anio = lubridate::year(fecha), mes = lubridate::month(fecha)) %>% 
  select(clae2, anio, mes, depto = nombre_departamento_indec, letra = letra, desc_clae = clae2_desc, puestos) %>% 
  filter(anio %in% c(2022))

df$letra <- if_else(nchar(df$letra) == 0, "OTR", df$letra)
df$puestos <- ifelse(df$puestos < 0, 0, df$puestos)

clae_dict <- clae %>% 
  select(letra, letra_desc) %>% 
  unique()

rm(clae, prov, puestos)

# df_perent <- dfclus %>% 
#   group_by(depto) %>% 
#   summarise(puestorel = (puestos / sum(puestos))*100)

df <- df %>% 
  select(-c(clae2, anio, desc_clae)) %>% 
  group_by(depto, letra) %>% 
  summarise(puesto = round(mean(puestos),0))


# Cantidades sembradas
siembra <- read_delim("datos/Estimaciones_agro.csv")


# Clustering
dft <- df %>%
  pivot_wider(names_from = letra, values_from = puesto) 

dftp <- round(dft[2:20] / rowSums(dft[2:20]), 2)
colnames(dftp) <- dft$depto

# Correlaciones
plt = GGally::ggcorr(dftp, label=T, method=c("all.obs","pearson"))
print(plt)

# Correlaciones por casos
dfc <- df %>% 
  pivot_wider(names_from = depto, values_from = puesto)

dfc <- round(dfc[2:18] / rowSums(dfc[2:18]), 2)

plt = GGally::ggcorr(dfc, label=T, method=c("all.obs","pearson"))
print(plt)

write_delim(dft, "datos/sectorxdepto.txt", delim = "\t")

# Analisis de casos atípicos
mean <- colMeans(dfc)
Sx <- cov(dfc)
Dm <- mahalanobis(dfc, mean, Sx, inverted = FALSE)

print(Dm)
pchisq(Dm, df = 17, lower.tail = FALSE)
qchisq(.99, df = 17)

plt = ggplot(gdat, aes(x=variable, y=value, group=depto)) +
  geom_line(alpha=0.3) +
  geom_line(data=filter(gdat, depto == "Paraná")
            , color="red", alpha=0.3) +
  theme_minimal()

# Analisis de perfiles
sector <- colnames(dft[2:20])
gdat <- bind_cols(sector = sector, dfc)

gdat <- gdat %>% 
  pivot_longer(
  -all_of("sector"), names_to="variable", values_to="value"
)

plt = ggplot(gdat, aes(x=variable, y=value, group=sector)) +
  geom_line(alpha=0.3) +
  geom_line(data=filter(gdat, sector == "C")
            , color="red", alpha=0.3) +
  theme_minimal()
print(plt)

### Observamos las distancias entre los departamentos
dist = dist(dfc, method = "manhattan")
dist_m = as.matrix(dist)

dimnames(dist_m) = list(sector1 = sector, sector2 = sector)
dist_df = as.data.frame(as.table(dist_m)) %>% rename(dist = Freq)

# Deptos más cercanos y lejanos de Paraná
tmp = dist_df %>% 
  filter(sector1 != sector2) %>% 
  filter(sector1 == "C") %>% 
  arrange(dist)

head(tmp)
tail(tmp)

### Test de Hopkins para tendencia al agrupamiento
hopkins = factoextra::get_clust_tendency(dftp, n = 14, seed = 321)
cat("Hopkins =", hopkins$hopkins_stat)

### ALGORITMO DE CLUSTER JERÁRQUICO
dftt <- dftp%>% 
  data.frame()

rownames(dftt) <- dft$depto
hc = amap::hcluster(dftt, method = "manhattan", link = "complete")

### Dendograma
factoextra::fviz_dend(hc, horiz = T, k = 4)

### Estudiamos si existen departamentos atípicos
dist = dist(dftp, method = "manhattan")
dist_m = as.matrix(dist)

dimnames(dist_m) = list(depto1 = dft$depto, depto2 = dft$depto)
dist_df = as.data.frame(as.table(dist_m)) %>% rename(dist = Freq)

gdat <- dftp %>% 
  mutate(depto = dft$depto) %>% 
  data.frame() %>% 
  pivot_longer(
    -all_of("depto"), names_to="variable", values_to="value"
  )

gdat = dist_df %>% 
  group_by(depto1) %>% 
  summarise(median_dist = median(dist))

plt = 
  ggplot(gdat, aes(x=reorder(depto1, median_dist), y = median_dist)) +
  geom_point() +
  theme_bw() +
  theme(axis.text.x = element_blank())

plotly::ggplotly(plt, width = 450, height = 300)

# Mapa de calor
mapacalor = function(long_df) {
  ggplot(long_df, aes(x=depto1, y=depto2, z=dist)) +
    geom_tile(aes(fill=dist)) +
    theme(
      axis.title.x=element_blank()
      ,axis.text.x=element_blank()
      ,axis.title.y=element_blank()
      ,axis.text.y=element_blank()
    ) +
    scale_fill_viridis_c()
} 

gdat = dist_df %>% 
  mutate(
    depto1 = factor(depto1, levels=dft$depto[hc$order]),
    depto2 = factor(depto2, levels=dft$depto[hc$order])
  )

print(mapacalor(gdat))

# Cantidad optima de clusters
factoextra::fviz_nbclust(dftp,
                         FUNcluster = factoextra::hcut, 
                         method = "gap_stat",
                         k.max = 10,
                         nboot = 50,
                         diss = dist(dftp, method = "manhattan"),
                         hc_method = "average")


# Clusters
dftp <- data.frame(dftp)
colnames(dftp) <- data.frame(dft$depto)
hc = factoextra::hcut(dftp, k = 4, hc_method = "average",
                      hc_metric = "manhattan", stand = F)

plt = factoextra::fviz_silhouette(hc, label = T,
                                  print.summary = F) 
plt +
  theme(axis.text.x = element_text(angle=-90, size=4))

print(plt)

# Dendograma por cluster
factoextra::fviz_dend(hc, horiz = T, k = 2, repel = T)
factoextra::fviz_dend(hc, type = "phylogenic", horiz = T, k = 4, repel = T)

# Agrupamiento de Cluster
dat_hc = dft %>% 
  mutate(cluster = factor(hc$cluster))

# Comparacion visual de sectores
gdat = dat_hc %>% 
  pivot_longer(
    -all_of(c(id_cols, "cluster")), names_to="variable", values_to="value")

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

print(plt_density)

plt_boxplot =
  ggplot(gdat, aes(x=variable, y=value, color=cluster)) +
  facet_wrap(~cluster, ncol=2, scales="free_y") +
  geom_boxplot() +
  theme_minimal() +
  theme(axis.text.x=element_text(angle=-90)) +
  NULL

print(plt_boxplot)

# Mapa
library(leaflet)

# Carga de mapa
shape <- rgdal::readOGR(dsn = "../ind-prov/pxdptodatosok.shp", verbose = FALSE)
shapeData <- sp::spTransform(shape, sp::CRS("+proj=longlat +datum=WGS84 +no_defs"))
shapeData <- sf::st_as_sf(shapeData, wkt = 'polygons', crs = st_crs(4326)) # make sf
shapeData <- rmapshaper::ms_simplify(shapeData, keep = 0.01, keep_shapes = TRUE) %>% 
  filter(provincia == "Entre Ríos")

