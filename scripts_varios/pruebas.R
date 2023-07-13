
# Prueba con la diferencia de especializacion relativa
library(tidyverse)

dict_depto <- read.csv("datos/diccionario_cod_depto.csv")
dict_clae <- readxl::read_excel("datos/diccionario_clae2.xlsx")
dict_caes1 <- readxl::read_excel("datos/caes_1.xlsx")
raw <- read.csv("datos/puestos_depto_total_por_letra.csv") %>% 
  filter(id_provincia_indec == 30)
raw <- merge(raw, dict_depto, by = "codigo_departamento_indec")
raw <- merge(raw, dict_clae, by = "letra")
raw$fecha <- as.Date(raw$fecha)
raw <- raw %>% 
  select(fecha, departamento = nombre_departamento_indec, letra, letra_desc, puestos) %>% 
  mutate(anio = lubridate::year(fecha),
         mes = lubridate::month(fecha)) %>% 
  select(anio, mes, departamento, letra, letra_desc, puestos)

# Especializacion relativa para 2014 (df2014)
tmp <- raw %>% 
  filter(anio == 2014) %>% 
  group_by(anio, mes, departamento, letra_desc) %>% 
  summarise(puestos = sum(puestos))

tmp <- tmp %>% 
  group_by(anio, departamento, letra_desc) %>% 
  summarise(puestos = round(mean(puestos), 0))

tmp <- merge(tmp, dict_caes1, by = "letra_desc")

tmp <- tmp %>%
  select(-c(letra_desc, anio)) %>% 
  group_by(departamento, caes_1) %>% 
  summarise(puestos = sum(puestos))

tmp <- tmp %>% 
  pivot_wider(names_from = departamento, values_from = puestos) 

col_df <- colnames(tmp)[2:18]
row_df <- tmp$caes_1

tmp <- tmp[2:18] %>% 
  data.frame()

sum_sect <- colSums(tmp[1:17])
sum_reg <- rowSums(tmp[1:17])
tot <- sum(sum_reg)

mat <- matrix(nrow = 7, ncol= 17)

# EjVij
# sum(colSums(tmp[tmp$caes_1 == "COMHOTREST", 2:18]))

# EiVij
# sum(tmp$Gualeguaychú)


for (i in 1:7) {
  for(j in 1:17){
    mat[i, j] <- (tmp[i, j] / sum_sect[j]) / (sum_reg[i] / tot)
  }
}

mat <- data.frame(mat)
colnames(mat) <- col_df
mat <- bind_cols(sector = row_df, mat)

# trasposición
dff <- t(mat) %>% data.frame()
colnames(dff) <- dff[1,]
dff <- dff[-1, ]

dff <- apply(dff, MARGIN = 2, FUN = as.numeric) %>% data.frame()
dff <- bind_cols(depto = col_df, dff)

df2014 <- dff
rm(dff, mat, tmp)


# Grafico de especializacion relativa para manufactura y servicios empresariales
ggplot(df2014) +
  aes(y = forcats::fct_reorder(depto, SERVEMP), x = SERVEMP) +
  geom_col(fill = "orange") +
  theme_bw() +
  ggtitle("2014 - servicios")

ggplot(df2014) +
  aes(y = forcats::fct_reorder(depto, MANUF), x = MANUF) +
  geom_col(fill = "darkorange") +
  theme_bw() +
  ggtitle("2014 - manufactura")

# Especializacion relativa para 2022 (df2022)
tmp <- raw %>% 
  filter(anio == 2022) %>% 
  group_by(anio, mes, departamento, letra_desc) %>% 
  summarise(puestos = sum(puestos))

tmp <- tmp %>% 
  group_by(anio, departamento, letra_desc) %>% 
  summarise(puestos = round(mean(puestos), 0))

tmp <- merge(tmp, dict_caes1, by = "letra_desc")

tmp <- tmp %>%
  select(-c(letra_desc, anio)) %>% 
  group_by(departamento, caes_1) %>% 
  summarise(puestos = sum(puestos))

tmp <- tmp %>% 
  pivot_wider(names_from = departamento, values_from = puestos) 

col_df <- colnames(tmp)[2:18]
row_df <- tmp$caes_1

tmp <- tmp[2:18] %>% 
  data.frame()

sum_sect <- colSums(tmp[1:17])
sum_reg <- rowSums(tmp[1:17])
tot <- sum(sum_reg)

mat <- matrix(nrow = 7, ncol= 17)

# EjVij
# sum(colSums(tmp[tmp$caes_1 == "COMHOTREST", 2:18]))

# EiVij
# sum(tmp$Gualeguaychú)

for (i in 1:7) {
  for(j in 1:17){
    mat[i, j] <- (tmp[i, j] / sum_sect[j]) / (sum_reg[i] / tot)
  }
}

mat <- data.frame(mat)
colnames(mat) <- col_df
mat <- bind_cols(sector = row_df, mat)

# trasposición
dff <- t(mat) %>% data.frame()
colnames(dff) <- dff[1,]
dff <- dff[-1, ]

dff <- apply(dff, MARGIN = 2, FUN = as.numeric) %>% data.frame()
dff <- bind_cols(depto = col_df, dff)

df2022 <- dff
rm(dff, mat, tmp, dict_caes1, dict_clae, dict_depto, raw, i, j, row_df, col_df, sum_reg, sum_sect, tot)

ggplot(df2022) +
  aes(y = forcats::fct_reorder(depto, SERVEMP), x = SERVEMP) +
  geom_col(fill = "violet") +
  theme_bw() +
  ggtitle("2022 - servicios")

ggplot(df2022) +
  aes(y = forcats::fct_reorder(depto, MANUF), x = MANUF) +
  geom_col(fill = "darkviolet") +
  theme_bw() +
  ggtitle("2022 - manufactura")


# Diferencias entre especializaciones relativas
dvar <- (df2022[2:8] - df2014[2:8]) / df2014[2:8] %>% data.frame()
rownames(dvar) <- df2022$depto

# Pca
pca <- FactoMineR::PCA(dvar, scale.unit = TRUE)

pca$eig
pca$var$cor

factoextra::fviz_pca_ind(pca, geom = c("point","text"), col.ind = "#678ADD", repel = TRUE, axes = c(1, 2))

# cluster
factoextra::fviz_nbclust(x = dvar, FUNcluster = kmeans, method = "wss", k.max = 15, 
             diss = factoextra::get_dist(dvar, method = "euclidean"), nstart = 50)

km_clusters <- kmeans(x = dvar, centers = 3, nstart = 50)

factoextra::fviz_cluster(object = km_clusters, data = dvar, show.clust.cent = TRUE,
             ellipse.type = "euclid", star.plot = TRUE, repel = TRUE) +
  labs(title = "Resultados clustering K-means") +
  theme_bw() +
  theme(legend.position = "none")
