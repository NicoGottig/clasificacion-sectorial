
# CARGA Y TRANSFORMACIÓN DE LOS DATAFRAMES
library(tidyverse)
puestos <- read.csv("datos/puestos_depto_tot_emp_por_clae2.csv") %>% 
  filter(id_provincia_indec == 30) %>% 
  select(-id_provincia_indec)

clae <- read.csv("datos/diccionario_clae2.csv")
prov <- read.csv("datos/diccionario_cod_depto.csv")

df2022 <- left_join(puestos, prov, by = "codigo_departamento_indec")
df2022 <- left_join(df2022, clae, by = "clae2")
df2022 <- df2022 %>% 
  mutate(anio = lubridate::year(fecha), mes = lubridate::month(fecha)) %>% 
  select(clae2, anio, mes, depto = nombre_departamento_indec, letra = letra, desc_clae = clae2_desc, puestos) %>% 
  filter(anio %in% c(2022))


df2014 <- left_join(puestos, prov, by = "codigo_departamento_indec")
df2014 <- left_join(df2014, clae, by = "clae2")
df2014 <- df2014 %>% 
  mutate(anio = lubridate::year(fecha), mes = lubridate::month(fecha)) %>% 
  select(clae2, anio, mes, depto = nombre_departamento_indec, letra = letra, desc_clae = clae2_desc, puestos) %>% 
  filter(anio %in% c(2014))


# AGRUPAMIENTO Y ACOLUMNAMIENTO DE DATAFRAMES
# Corregir
df2022$letra <- if_else(nchar(df2022$letra) == 0, "OTR", df2022$letra)
df2022$puestos <- ifelse(df2022$puestos < 0, 0, df2022$puestos)

df2014$letra <- if_else(nchar(df2014$letra) == 0, "OTR", df2014$letra)
df2014$puestos <- ifelse(df2014$puestos < 0, 0, df2014$puestos)

# Resumir
df2022 <- df2022 %>% 
  select(-c(clae2, anio)) %>% 
  group_by(depto, letra, desc_clae) %>% 
  summarise(puesto = round(mean(puestos),0))

df2022 <- df2022 %>% 
  group_by(depto, letra) %>% 
  summarise(puesto = sum(puesto))

df2014 <- df2014 %>% 
  select(-c(clae2, anio)) %>% 
  group_by(depto, letra, desc_clae) %>% 
  summarise(puesto = round(mean(puestos),0))

df2014 <- df2014 %>% 
  group_by(depto, letra) %>% 
  summarise(puesto = sum(puesto))

# Pivotear
dft2022 <- df2022 %>%
  pivot_wider(names_from = letra, values_from = puesto) 

dft2014 <- df2014 %>%
  pivot_wider(names_from = letra, values_from = puesto) 

# Sumar columnas (servicios o produccion)
dftg2022 <- dft2022 %>% 
  mutate(produccion = sum(A, B, C, D, E, F),
         servicios = sum(G, H, I, J, K, L, M, N, OTR, P, Q, R, S)) %>% 
  select(depto, produccion, servicios)

dftg2014 <- dft2014 %>% 
  mutate(produccion = sum(A, B, C, D, E, F),
         servicios = sum(G, H, I, J, K, L, M, N, OTR, P, Q, R, S)) %>% 
  select(depto, produccion, servicios)

# Calcular variacion
dfvar <- (dftg2022[2:3] - dftg2014[2:3]) / dftg2014[2:3] %>% 
  data.frame()
dfvar$depto <- dft2022$depto

# ver el comportamiento de los departamentos
dfvar %>% 
  ggplot(aes(label = depto, x = servicios, y = produccion)) +
  geom_point(col = "red") +
  theme_bw() +
  ggrepel::geom_text_repel() +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0)

# Variacion y pca para los 20 sectores. Se trabaja con la variacion absoluta y se estandariza.
dfvarabs <- dft2022[2:20] - dft2014[2:20]
rownames(dfvarabs) <-  dft2022$depto

# Resultados
pca <- FactoMineR::PCA(dfvarabs, scale.unit = TRUE)

pca$eig
pca$var$cor

# Visualización de resultados
factoextra::fviz(pca, "ind", axes = c(1,2))
factoextra::fviz(pca, "ind", axes = c(2,3))
factoextra::fviz(pca, "ind", axes = c(2,4))

# Cluster segun variación absoluta
minmax = function(x) (x - min(x)) / (max(x) - min(x))
rob_scale = function(x) (x - median(x)) / IQR(x)

# data numerica normalizada
dfnorm = dfvarabs %>%
  select_if(is.numeric) %>% 
  mutate_all(minmax) %>% # normalizacion
  as.data.frame() # las funciones de cluster se llevan mejor con data.frame (admite row.names)

hopkins = factoextra::get_clust_tendency(dfnorm, n = 14, seed = 321)
cat("Hopkins =", hopkins$hopkins_stat)

# Screeplot
factoextra::fviz_nbclust(dfnorm,
                         FUNcluster = factoextra::hcut, 
                         method = "gap_stat",
                         k.max = 10,
                         nboot = 50,
                         diss = dist(dfnorm, method = "euclidean"),
                         hc_method = "ward.D")

