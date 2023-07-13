
# Evolución de la productividad y sector manufacturero y servicios
library(tidyverse)

vab <- read_delim("datalimpia/vab_xrama.txt")
l <- read_delim("datalimpia/trab_xanio.txt")

vab <- vab %>% 
  group_by(clae1, anio) %>% 
  summarise(VAB = sum(VAB)) %>% 
  filter(anio >= 2014 & anio <= 2021) %>% 
  data.frame()

l <- l %>% 
  group_by(anio, clae1 = caes_1) %>% 
  summarise(puestos = sum(puestos)) %>% 
  filter(anio < 2022) %>% 
  data.frame()

head(vab)

df <- vab %>% 
  left_join(l, by = c("anio", "clae1"))

df <- df %>% 
  mutate(product = VAB/puestos)

# Gráfico de evolución
df %>% 
  filter(clae1 %in% c("MANUF", "SERVEMP")) %>% 
  pivot_longer(-all_of(c("clae1","anio")), names_to = "var", values_to = "val") %>% 
  ggplot() +
  aes(x = anio, y = val, group = clae1) +
  geom_line() +
  geom_point(aes(x = anio, y = val, shape = clae1), size = 3) +
  theme_bw() +
  facet_wrap(~var, scales = "free", nrow = 3) +
  xlab("") +
  ylab("")

# UNO CHETO
# ggplot(cant_partos) +
#   aes(x = año_mes, y = n, color = efector) +
#   geom_line(lwd = 1.5) +
#   geom_point(size = 3, pch = 1) +
#   scale_y_continuous(
#     name = "Número de nacimientos",
#     limits = c(0, 600), expand = c(0, 0),
#     sec.axis = dup_axis(breaks = ultimos$n, labels = ultimos$efector, name = NULL)
#   ) +
#   scale_x_date(name = "Meses", date_labels = "%b-%y", date_breaks = "5 months") +
#   theme(legend.position = "none",
#         axis.line.y.right = element_blank(),
#         axis.ticks.y.right = element_blank(),
#         axis.text.y.right = element_text(margin = margin(0, 0, 0, 0)),
#         plot.margin = margin(14, 7, 3, 1.5),
#   )


# Grafico de barras de variación de la productividad por sector
df2014 <- df %>% 
  filter(anio == 2014)

df2021 <- df %>% 
  filter(anio == 2021)

var <- cbind(clae1 = df2021$clae1, var = (df2021[2:5] - df2014[2:5]) /df2014[2:5])

var %>% 
  ggplot() + 
  aes(y = forcats::fct_reorder(clae1, var.product), x = var.product) +
  geom_col(fill = if_else(var$var.product > 0,"red", "darkred")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 0)) + 
  xlab("Variación de la productividad") +
  ylab("Sector")


# Analisis de la variacion del empleo de ambos sectores
df2014 <- read_delim("datalimpia/df_puestos.txt", delim = "\t") %>% 
  filter(anio == 2014) %>% 
  pivot_wider(names_from = caes_1, values_from = puestos) %>% 
  data.frame() %>% 
  select(-anio) %>% 
  select(MANUF, SERVEMP, departamento)

df2021 <- read_delim("datalimpia/df_puestos.txt", delim = "\t") %>% 
  filter(anio == 2021) %>% 
  pivot_wider(names_from = caes_1, values_from = puestos) %>% 
  data.frame() %>% 
  select(-anio) %>% 
  select(MANUF, SERVEMP, departamento)


var <- (df2021[1:2] - df2014[1:2]) / df2014[1:2]

var$depto <- df2021$departamento

var %>% 
  ggplot() +
  aes(x = MANUF, y = SERVEMP, label = depto) +
  geom_point(col = "red") +
  geom_smooth(method = "lm", se = FALSE)+
  theme_bw() +
  ggrepel::geom_text_repel() +
  xlab("Variación en manufactura") +
  ylab("variación en serv. Empresariales") 

# sin atipico
var %>% 
  filter(depto != "Feliciano") %>% 
  ggplot() +
  aes(x = MANUF, y = SERVEMP, label = depto) +
  geom_point(col = "red") +
  geom_smooth(method = "lm", se = FALSE)+
  theme_bw() +
  ggrepel::geom_text_repel() +
  xlab("Variación en manufactura") +
  ylab("variación en serv. Empresariales") 


# Matriz de especialización relativa 2014
df <- read_delim("datalimpia/df_puestos.txt", delim = "\t") %>% 
  filter(anio == 2014) %>% 
  select(-anio)

df <- df %>% 
  pivot_wider(names_from = departamento, values_from = puestos) %>% 
  data.frame()

rownames(df) <- df$caes_1

df <- df[2:18] %>% 
  data.frame()

sum_sect <- colSums(df[1:17])
sum_reg <- rowSums(df[1:17])
tot <- sum(sum_reg)

# Pruebas
# EjVij
sum(colSums(dfw[dfw$caes_1 == "COMHOTREST", 2:18]))
# EiVij
sum(dfw$Gualeguaychú)

# Calculo de la matriz
mat <- matrix(nrow = 7, ncol= 17)
for (i in 1:7) {
  for(j in 1:17){
    mat[i, j] <- (df[i, j] / sum_sect[j]) / (sum_reg[i] / tot)
  }
}

mat <- data.frame(mat)
colnames(mat) <- colnames(df)
rownames(mat) <- rownames(df)

# trasposición
esp2014 <- t(mat) %>% data.frame()

# Gráfico de especialización relativa por region
esp2014 %>%
  mutate(depto = rownames(.)) %>% 
  pivot_longer(-depto, names_to = "variab", values_to = "valor") %>% 
  ggplot() +
  aes(x = depto, y = variab, fill = valor) +
  geom_tile(colour = "white") + 
  scale_fill_viridis_c()+
  theme(axis.text.x = element_text(angle = 90)) +
  xlab("") +
  ylab("") + 
  ggtitle("Especialización relativa 2014")

# Calculo de especialización relativa para 2021
df <- read_delim("datalimpia/df_puestos.txt", delim = "\t") %>% 
  filter(anio == 2021) %>% 
  select(-anio)

df <- df %>% 
  pivot_wider(names_from = departamento, values_from = puestos) %>% 
  data.frame()

rownames(df) <- df$caes_1

df <- df[2:18] %>% 
  data.frame()

sum_sect <- colSums(df[1:17])
sum_reg <- rowSums(df[1:17])
tot <- sum(sum_reg)

# Pruebas
# EjVij
# sum(colSums(dfw[dfw$caes_1 == "COMHOTREST", 2:18]))
# EiVij
# sum(dfw$Gualeguaychú)

# Calculo de la matriz
mat <- matrix(nrow = 7, ncol= 17)
for (i in 1:7) {
  for(j in 1:17){
    mat[i, j] <- (df[i, j] / sum_sect[j]) / (sum_reg[i] / tot)
  }
}

mat <- data.frame(mat)
colnames(mat) <- colnames(df)
rownames(mat) <- rownames(df)

# trasposición
esp2021 <- t(mat) %>% data.frame()

# Gráfico de especialización relativa por region
esp2021 %>%
  mutate(depto = rownames(.)) %>% 
  pivot_longer(-depto, names_to = "variab", values_to = "valor") %>% 
  ggplot() +
  aes(x = depto, y = variab, fill = valor) +
  geom_tile(colour = "white") + 
  scale_fill_viridis_c()+
  theme(axis.text.x = element_text(angle = 90)) +
  xlab("") +
  ylab("") +
  ggtitle("Especialización relativa 2021")

esp2021$depto <- rownames(esp2021)
write_delim(esp2021, "datalimpia/esp2021.txt", delim = "\t")
esp2021 <- select(esp2021, -depto)

# Departamentos ordenados según especialización relativa en ambos periodos
esp2014$depto <- rownames(esp2014)
esp2014$anio <- 2014
esp2021$depto <- rownames(esp2021)
esp2021$anio <- 2021

esp <- bind_rows(esp2014, esp2021)

esp %>% 
  select(c("depto", "anio", "SERVEMP")) %>% 
  ggplot() +
  aes(y = forcats::fct_reorder(depto, -SERVEMP), x = SERVEMP, fill = as.factor(anio)) +
  geom_col(position = "dodge") +
  geom_vline(xintercept = 1, size = 0.2) +
  scale_fill_discrete(name = "Año") + 
  theme_bw() +
  xlab("") +
  ylab("") +
  ggtitle("Especialización relativa de servicios empresariales")

esp %>% 
  select(c("depto", "anio", "MANUF")) %>% 
  ggplot() +
  aes(y = forcats::fct_reorder(depto, -MANUF), x = MANUF, fill = as.factor(anio)) +
  geom_col(position = "dodge") +
  geom_vline(xintercept = 1, size = 0.2) +
  scale_fill_discrete(name = "Año") + 
  theme_bw() +
  xlab("") +
  ylab("") +
  ggtitle("Especialización relativa de manufactura")

# Analisis de componentes principales a tres niveles
esp <- esp %>% 
  mutate(unid = paste0(depto, "-", anio))

rownames(esp) <- esp$uni
dfpca <- esp
rownames(dfpca) <- dfpca$unid
dfpca <- dfpca %>% 
  select(-c(depto, anio, unid))

pca <- FactoMineR::PCA(dfpca, scale.unit = "TRUE")

pca$eig

factoextra::fviz_screeplot(pca) +
  ggtitle("Contribución de las dimensiones")

pca$var$cor

factoextra::fviz_pca_ind(pca, geom = c("point", "text"), repel = TRUE, axes = c(1,2))
factoextra::fviz_pca_ind(pca, geom = c("point", "text"), repel = TRUE, axes = c(1,3))
factoextra::fviz_pca_ind(pca, geom = c("point", "text"), repel = TRUE, axes = c(1,4))

# Analisis de autocorrelacion espacial
library(sf)
library(spdep)
library(tmap)

# Carga de estructura de trabajo registrado
df <- read_delim("datalimpia/esp2021.txt", delim = "\t")

# Carga de mapas
shape <- rgdal::readOGR(dsn = "mapa/pxdptodatosok.shp", verbose = FALSE)
shapeData <- sp::spTransform(shape, sp::CRS("+proj=longlat +datum=WGS84 +no_defs"))
shapeData <- sf::st_as_sf(shapeData, wkt = 'polygons', crs = st_crs(4326)) # make sf
shapeData <- rmapshaper::ms_simplify(shapeData, keep = 0.01, keep_shapes = TRUE) %>% 
  filter(provincia == "Entre Ríos")

df$depto <- gsub("\\.", " ", df$depto)

mapdata <- sp::merge(x = shapeData, y = df, by.x = "departamen", by.y = "depto") 

# Análisis univariado del sector manufacturero
boxplot(mapdata$MANUF)

# Mapa
tm_shape(mapdata) + tm_fill(col="MANUF",
                            style="quantile",
                            n=4,
                            palette="Greens") +
  tm_legend(outside=TRUE)

# Definición de vecinos
nb <- poly2nb(mapdata, queen=TRUE)

# Asignar peso a los vecinos
lw <- nb2listw(nb, style="W", zero.policy=TRUE)

# Analisis con pesos rezagados
per.lag <- lag.listw(lw, mapdata$MANUF)
per.lag

plot(per.lag ~ mapdata$MANUF, pch=16, asp=1)
text(df$MANUF, (per.lag-0.02), labels=df$depto, cex =0.75)
M1 <- lm(per.lag ~ df$MANUF)
abline(M1, col="blue")
coef(M1)[2]

# Inferencia
moran.test(mapdata$MANUF,lw, alternative="greater")

# Analisis univariado del sector de servicios empresariales (esta sin islas)
boxplot(mapdata$SERVEMP)

mapdata <- mapdata

# Mapa
tm_shape(mapdata) + tm_fill(col="SERVEMP",
                            style="quantile",
                            n=4,
                            palette="Greens") +
  tm_legend(outside=TRUE)

# Definición de vecinos
nb <- poly2nb(mapdata, queen=TRUE)

# Asignar peso a los vecinos
lw <- nb2listw(nb, style="W", zero.policy=TRUE)

# Analisis con pesos rezagados
per.lag <- lag.listw(lw, mapdata$SERVEMP)
per.lag

plot(per.lag ~ mapdata$SERVEMP, pch=16, asp=1)
text(df$SERVEMP, (per.lag-0.02), labels=df$depto, cex =0.75)
M1 <- lm(per.lag ~ df$SERVEMP)
abline(M1, col="blue")
coef(M1)[2]

# Inferencia
moran.test(mapdata$SERVEMP,lw, alternative="greater")

