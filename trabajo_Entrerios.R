library(tidyverse)

# Carga de archivos ####
puestos <- read.csv("datos/puestos_depto_tot_emp_por_clae2.csv") %>% 
  filter(id_provincia_indec == 30) %>% 
  select(-id_provincia_indec)
clae <- read.csv("datos/diccionario_clae2.csv")
prov <- read.csv("datos/diccionario_cod_depto.csv")
salar <- read.csv("datos/w_median_privado_mensual_por_clae2.csv")
salar <- salar %>% 
  mutate(anio = lubridate::year(fecha), mes = lubridate::month(fecha)) %>% 
  filter(anio == 2022 & mes == 12)

df <- left_join(puestos, prov, by = "codigo_departamento_indec")
df <- left_join(df, clae, by = "clae2")
df <- df %>% 
  mutate(anio = lubridate::year(fecha), mes = lubridate::month(fecha)) %>% 
  select(clae2, anio, mes, depto = nombre_departamento_indec, letra_desc = letra, desc_clae = clae2_desc, puestos) %>% 
  filter(anio %in% c(2022)) %>% 
  filter(letra_desc == "C")

df$letra_desc <- if_else(nchar(df$letra_desc) == 0, "OTR", df$letra_desc)
df$puestos <- ifelse(df$puestos < 0, 0, df$puestos)

rm(clae, prov, puestos)

df <- df %>% 
  group_by(anio, depto, clae2, desc_clae) %>% 
  summarise(puestos = max(puestos)) %>% 
  data.frame()

dfs <- merge(df, salar, by.x = "clae2", by.y = "clae2")

dfs <- dfs %>% 
  select(clae2, depto, desc_clae, puestos, w_median) %>% 
  mutate(va_anual = puestos*w_median)

dft <- dfs %>%
  select(-c(clae2, puestos, w_median)) %>% 
  pivot_wider(names_from = desc_clae, values_from = va_anual) 

dft[is.na(dft)] <- 0

# Se elimina tabaco xq solo parana tiene
dft <- dft %>% 
  select(-`Elaboración de productos de tabaco`) %>% 
  data.frame()

# Se saca paraná, concordia y gualeguaychú por atípicos (y por lo tanto refinación de petroleo)
dft <- dft %>% 
  filter(!(depto %in% c("Paraná", "Concordia", "Gualeguaychú"))) %>% 
  select(-Fabricación.de.productos.de.refinación.de.petróleo)

# Análisis de componentes
rownames(dft) <- dft$depto
pca <- stats::prcomp(dft[,2:23], scale = TRUE)
pca$eig

dims <- pca[2] %>% data.frame()
dims$var <- rownames(dims)
write_delim(dims, "datos/corrpca.txt", delim = "\t")

factoextra::fviz_pca_ind(pca, geom.ind = "text", repel = TRUE)

pcdim <- as.data.frame(-pca$x[,1:10])

# Evaluacion de clustering
factoextra::fviz_nbclust(pcdim, kmeans)

kmeans_iris = kmeans(pcdim, centers = 3, nstart = 50)
factoextra::fviz_cluster(kmeans_iris, data = pcdim)






# Diccionario
dfs$sector <- case_when(
  dfs$desc_clae== "Elaboracion de productos farmacéuticos" ~ "Farm", 
  dfs$desc_clae== "Elaboración de bebidas" ~ "Bebi",
  dfs$desc_clae== "Elaboración de prendas de vestir" ~ "Vest",
  dfs$desc_clae== "Elaboración de productos alimenticios" ~ "Alim",
  dfs$desc_clae== "Elaboración de productos de cuero y calzado" ~ "Cuercalz",
  dfs$desc_clae== "Elaboración de productos de madera" ~ "Made",
  dfs$desc_clae== "Elaboración de productos de papel" ~ "Pape",
  dfs$desc_clae== "Elaboración de productos textiles" ~ "Text",
  dfs$desc_clae== "Fabricación de equipo eléctrico" ~ "EquiElec",
  dfs$desc_clae== "Fabricación de maquinarias" ~ "Maqu",
  dfs$desc_clae== "Fabricación de metales comunes" ~ "MetCom",
  dfs$desc_clae== "Fabricación de muebles" ~ "Mueb",
  dfs$desc_clae== "Fabricación de otros equipos de transporte" ~ "EquiTran",
  dfs$desc_clae== "Fabricación de productos de caucho y vidrio" ~ "CauVid",
  dfs$desc_clae== "Fabricación de productos de informática, de electrónica y de óptica" ~ "InfoElecOpt",
  dfs$desc_clae== "Fabricación de productos de vidrio y otros minerales no metálicos" ~ "VidOtrMin",
  dfs$desc_clae== "Fabricación de productos elaborados del metal, excepto maquinaria y equipo" ~ "MetalExcMaqEq",
  dfs$desc_clae== "Fabricación de sustancias químicas" ~ "SustQuim",
  dfs$desc_clae== "Fabricación de vehículos automotores, remolques y semirremolques" ~ "AutoRemSemi",
  dfs$desc_clae== "Imprentas y editoriales" ~ "ImpEdit",
  dfs$desc_clae== "Otras industrias manufactureras" ~ "Otr",
  dfs$desc_clae== "Reparación e instalación de maquinaria y equipo" ~ "RepInstalMaq",
  dfs$desc_clae== "Elaboración de productos de tabaco" ~ "ProdTab",        
  dfs$desc_clae== "Fabricación de productos de refinación de petróleo" ~ "RefPet"  
)

dft <- dfs %>%
  select(-c(clae2, desc_clae, puestos, w_median)) %>% 
  pivot_wider(names_from = sector, values_from = va_anual) 

dft[is.na(dft)] <- 0


# Participación relativa por total de trabajo por departamento
df_perent <- dfclus %>% 
  group_by(depto) %>% 
  summarise(puestorel = (puestos / sum(puestos))*100)

dfclus <- dfclus %>% 
  select(depto, desc_clae) %>% 
  mutate(puestorel = df_perent$puestorel)

write_delim(dfclus, "datos/trabajoIndustria2022.txt", delim = "\t")

# Análisis exploratorio 1 


# Estandarización robusta
minmax = function(x) (x - min(x)) / (max(x) - min(x))
rob_scale = function(x) (x - median(x)) / IQR(x)
deptos <- dft$depto

dft = dft %>%
  select_if(is.numeric) %>% 
  # scale(center=T, scale=T) %>% # estandarizacion media-desvio
  mutate_all(rob_scale) %>% # normalizacion
  as.data.frame()

dft <- bind_cols(depto = deptos, dft)

# Análisis exploratorio 2 
id_cols <- c("depto")

# formato long con datos normalizados
gdat = dft %>%
  pivot_longer(
    -all_of(id_cols), names_to="variable", values_to="value"
  )

# plot de coordenadas paralelas
plt = ggplot(gdat, aes(x=variable, y=value, group=depto)) +
  geom_line(alpha=0.3) +
  geom_line(data=filter(gdat, depto == "Paraná")
            , color="red", alpha=0.3) +
  theme_minimal()

# plot interactivo
library(plotly)
ggplotly(plt, width=1500, height=500)

### Observamos las distancias entre los departamentos
dist = dist(dft[2:25], method = "manhattan")
dist_m = as.matrix(dist)

dimnames(dist_m) = list(depto1 = dft$depto, depto2 = dft$depto)
dist_df = as.data.frame(as.table(dist_m)) %>% rename(dist = Freq)

# Deptos más cercanos y lejanos de Paraná
tmp = dist_df %>% 
  filter(depto1 != depto2) %>% 
  filter(depto1 == "Paraná") %>% 
  arrange(dist)

head(tmp)
tail(tmp)

### Estudiamos si existen departamentos atípicos
gdat = dist_df %>% 
  group_by(depto1) %>% 
  summarise(median_dist = median(dist))

plt = 
  ggplot(gdat, aes(x=reorder(depto1, median_dist), y = median_dist)) +
  geom_point() +
  theme_bw() +
  theme(axis.text.x = element_blank())

ggplotly(plt, width = 1000, height = 450)

### Test de Hopkins para tendencia al agrupamiento
hopkins = factoextra::get_clust_tendency(dft[2:25], n = 14, seed = 321)
cat("Hopkins =", hopkins$hopkins_stat)

### ALGORITMO DE CLUSTER JERÁRQUICO
rownames(dft) <- dft$depto

hc = amap::hcluster(dft[2:25], method = "manhattan", link = "complete")

### Dendograma
grupos = ifelse(dft$depto[hc$order] %in% c("Paraná", "Concordia", "Gualeguaychú"), 2, 1)
colores = c("black", "red", "blue")

factoextra::fviz_dend(hc, horiz = T, k_colors = colores,
                      label_cols = grupos)

### Analisis sin outliers
gdat = dist_df %>% 
  mutate(
    depto1 = factor(depto1, levels=dft$depto[hc$order]),
    depto2 = factor(depto2, levels=dft$depto[hc$order])
  )


outliers_deptos = c("Paraná", "Gualeguaychú", "Concordia")

gdat_sout = gdat %>% 
  filter(!(depto1 %in% outliers_deptos | depto2 %in% outliers_deptos))


# Mapa de calor sin outliers
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

print(mapacalor(gdat_sout))

# Dendograma sin outliers
dft_sout <- dft %>% 
  filter(!(depto %in% outliers_deptos))
hc = amap::hcluster(dft_sout[2:25], method = "manhattan", link = "average")
factoextra::fviz_dend(hc, horiz = T)

# Punto de quiebre
factoextra::fviz_nbclust(dft[2:25], FUNcluster = factoextra::hcut, method = "wss", k.max = 10, diss = dist(dft, method = "manhattan"), hc_method = "average")

# ML
set.seed(321)
factoextra::fviz_nbclust(dft[2:25], FUNcluster = factoextra::hcut, method = "gap_stat", k.max = 10, nstart = 50, nboot = 100, 
                         diss = dist(dft, method = "manhattan"), hc_method = "average")

# Clusters
hc = factoextra::hcut(dft[2:25], k = 3, hc_method = "average",
                      hc_metric = "manhattan", stand = F)

plt = factoextra::fviz_silhouette(hc, label = T,
                                  print.summary = F) 
plt +
  theme(axis.text.x = element_text(angle=-90, size=4))

print(plt)

# Dendograma por cluster
factoextra::fviz_dend(hc, horiz = T, k = 4, repel = T)
factoextra::fviz_dend(hc, type = "phylogenic", horiz = T, k = 4, repel = T)

# Agrupamiento de Cluster
dat_hc = dft %>% 
  mutate(cluster = factor(hc$cluster))

depto_outlier = dat_hc %>% 
  group_by(cluster) %>% 
  filter(n() == 1) %>% 
  pull(depto)

dat_hc = dat_hc %>% 
  mutate(outlier = ifelse(depto %in% depto_outlier, 1, 0))

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

dftp$depto <- depto

shapeData <-




# PCA - Categorizo casos ########
dft$caso <- paste0(dft$depto, "_", dft$anio)
rownames(dft) <- dft$caso

# Primer análisis
res.pca <- FactoMineR::PCA(dft[3:21], scale = FALSE)
factoextra::fviz_eig(res.pca)

# Graficos
groups <- as.factor(dft$anio[1:34])
factoextra::fviz_pca_ind(res.pca,
                         col.ind = groups,
                         palette = c("#00AFBB", "#FC4E07"),
                         repel = FALSE,
                         geom = "point")

# sin atipicos
dft_sinout <- dft %>% filter(!depto %in% c("Paraná", "Concordia"))
groups <- as.factor(dft_sinout$anio[1:30])
res.pca <- FactoMineR::PCA(dft_sinout[3:21], scale = FALSE)
factoextra::fviz_pca_ind(res.pca,
                         col.ind = groups,
                         palette = c("#00AFBB", "#FC4E07"),
                         repel = TRUE)

