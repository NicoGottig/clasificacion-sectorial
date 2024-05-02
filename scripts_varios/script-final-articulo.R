
# Cambiar tipografía de los gráficos 
library(tidyverse)
library(ggtext)
library(showtext)
showtext_auto()
font_add_google(name="Noto Sans", family = "noto")
theme_set(theme_bw(base_family = "noto"))


# Evolución de la productividad y sector manufacturero y servicios -------<<<<<<<

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

df$clae1 <- case_when(
  df$clae1 == "PRIM" ~ "Sec. Primario",
  df$clae1 == "MANUF" ~ "Ind. Manuf.", 
  df$clae1 == "CONST" ~ "Cons. y Sumin.",
  df$clae1 == "COMHOTREST" ~ "Com., Hot. y Rest.",
  df$clae1 == "SERVEMP" ~ "Serv. Emp",
  df$clae1 == "ENSESALUD" ~ "Salud y educación",
  df$clae1 == "SOCADMIN"~ "Serv. Soc., y Adm. Púb."
)

df %>% 
  filter(clae1 %in% c("Ind. Manuf.", "Serv. Emp")) %>%
  select(Productividad = product, Cant.Puestos = puestos, V.A.B = VAB, clae1, anio) %>% 
  pivot_longer(-all_of(c("clae1","anio")), names_to = "var", values_to = "val") %>% 
  ggplot() +
  aes(x = anio, y = val, group = clae1) +
  geom_line(col = "#898989") +
  geom_point(aes(x = anio, y = val, shape = clae1), size = 2, col= "#C84630") +
  theme_bw() +
  facet_wrap(~factor(var,levels = c("Productividad", "V.A.B", "Cant.Puestos")), scales = "free", nrow = 3) +
  xlab("") +
  ylab("") +
  theme(legend.position = "bottom", text = element_text(size=8))


# Grafico de barras de variación de la productividad por sector
df2014 <- df %>% 
  filter(anio == 2014)

df2021 <- df %>% 
  filter(anio == 2021)


df2021 %>% 
  ggplot() +
  aes(x = product, y = forcats::fct_reorder(clae1, product)) +
  geom_col(fill = "darkred") +
  theme_light()


var <- cbind(clae1 = df2021$clae1, var = (df2021[2:5] - df2014[2:5]) /df2014[2:5])

var$clae1 <- case_when(
  var$clae1 == "PRIM" ~ "Sec. Primario",
  var$clae1 == "MANUF" ~ "Ind. Manuf.", 
  var$clae1 == "CONST" ~ "Cons. y Sumin.",
  var$clae1 == "COMHOTREST" ~ "Com., Hot. y Rest.",
  var$clae1 == "SERVEMP" ~ "Serv. Emp",
  var$clae1 == "ENSESALUD" ~ "Salud y educación",
  var$clae1 == "SOCADMIN"~ "Serv. Soc., y Adm. Púb."
)

var %>% 
  ggplot() + 
  aes(y = forcats::fct_reorder(clae1, var.product), x = var.product) +
  geom_col(fill = if_else(var$var.product > 0,"#898989", "#C84630")) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 0)) + 
  xlab("Variación de la productividad estimada") +
  ylab("Sector") +
  theme(legend.position = "bottom", text = element_text(size=8))

# Matriz de especialización relativa 2014  -------------------------------<<<<<<<
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


esp2014$clae1 <- case_when(
  esp2014$clae1 == "PRIM" ~ "Sec. Primario",
  esp2014$clae1 == "MANUF" ~ "Ind. Manuf.", 
  esp2014$clae1 == "CONST" ~ "Cons. y Sumin.",
  esp2014$clae1 == "COMHOTREST" ~ "Com., Hot. y Rest.",
  esp2014$clae1 == "SERVEMP" ~ "Serv. Emp",
  esp2014$clae1 == "ENSESALUD" ~ "Salud y educación",
  esp2014$clae1 == "SOCADMIN"~ "Serv. Soc., y Adm. Púb."
)

esp2014 <- esp2014 %>% 
  rename(
     "Sec. Primario"="PRIM",
      "Ind. Manuf."="MANUF", 
      "Cons. y Sumin."="CONST",
     "Com., Hot. y Rest."="COMHOTREST",
     "Serv. Emp"="SERVEMP",
      "Salud y educación"="ENSESALUD",
     "Serv. Soc., y Adm. Púb."="SOCADMIN" 
  )

# Gráfico de especialización relativa por region
esp2014 %>%
  mutate(depto = rownames(.)) %>% 
  pivot_longer(-depto, names_to = "variab", values_to = "valor") %>% 
  ggplot() +
  aes(x = depto, y = variab, fill = valor) +
  geom_tile(colour = "white") + 
  scale_fill_gradient(low="#14213d", high="#C84630")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(fill = "Índice" ) +
  xlab("") +
  ylab("")

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

esp2021 <- esp2021 %>% 
  rename(
    "Sec. Primario"="PRIM",
    "Ind. Manuf."="MANUF", 
    "Cons. y Sumin."="CONST",
    "Com., Hot. y Rest."="COMHOTREST",
    "Serv. Emp"="SERVEMP",
    "Salud y educación"="ENSESALUD",
    "Serv. Soc., y Adm. Púb."="SOCADMIN" 
  )

# Gráfico de especialización relativa por region
esp2021 %>%
  mutate(depto = rownames(.)) %>% 
  pivot_longer(-depto, names_to = "variab", values_to = "valor") %>% 
  ggplot() +
  aes(x = depto, y = variab, fill = valor) +
  geom_tile(colour = "white") + 
  scale_fill_gradient(low="#14213d", high="#C84630")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        plot.title = element_text(size = 10),
        plot.subtitle = element_text(size = 8),
        legend.title = element_text(size = 8)) +
  labs(fill = "Índice" ) +
  xlab("") +
  ylab("")
  # labs(title = "Especialización Relativa del Trabajo por Departamento",
  #      subtitle = "Año 2021",
  #      caption = "Fuente: Elaboración propia en base al Ministerio de Desarrollo Productivo")

# Gráfico de variacion especialización relativa por region
varesp <- esp2021/esp2014 - 1

varesp %>%
  mutate(depto = rownames(.)) %>% 
  pivot_longer(-depto, names_to = "variab", values_to = "valor") %>% 
  ggplot() +
  aes(x = depto, y = variab, fill = valor) +
  geom_tile(colour = "white") + 
  scale_fill_gradient(low="#14213d", high="#C84630")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.title=element_text(size=8)) +
  labs(fill = "Var. del Índice" ) +
  xlab("") +
  ylab("")


esp2021$depto <- rownames(esp2021)
write_delim(esp2021, "datalimpia/esp2021.txt", delim = "\t")
esp2021 <- select(esp2021, -depto)

# Departamentos ordenados según especialización relativa en ambos periodos
esp2014$depto <- rownames(esp2014)
esp2014$anio <- 2014
esp2021$depto <- rownames(esp2021)
esp2021$anio <- 2021

esp <- bind_rows(esp2014, esp2021)


# Analisis de componentes principales a tres niveles <------------------------<<<<<
esp <- esp %>% 
  mutate(unid = paste0(substr(esp$depto, 0, 3), substr(esp$depto, nchar(esp$depto), nchar(esp$depto)),".", substr(esp$anio, 3, 4)))

rownames(esp) <- esp$uni

esp <- esp %>% 
  select_if(is.numeric) %>% 
  select(-anio)

# Correlacion

col <- colorRampPalette(c("#C84630", "#EE9988", "#FFFFFF", "#77AADD", "#14213d"))

corrplot::corrplot(cor(esp2021), 
                   method = "color", 
                   order="hclust",
                   col=col(200),
                   tl.col="black", 
                   tl.srt=45, 
                   tl.cex = 0.8)  



my_fn <- function(data, mapping, ...){
  p <- ggplot(data = data, mapping = mapping) + 
    geom_point() + 
    geom_smooth(method=lm, fill="#EE9988", color="#EE9988", ...)
  p
}


dftmp <- esp2021 %>% 
  filter(depto != "Islas.del.Ibicuy")

GGally::ggpairs(dftmp,columns = 1:7, lower = list(continuous = my_fn)) + theme_bw()


# PCA
pca <- FactoMineR::PCA(esp, scale.unit = "TRUE")

pca$eig

factoextra::fviz_screeplot(pca, barfill ="#C84630", linecolor = "#14213d", barcolor = "#C84630") +
  ggtitle("Contribución de las dimensiones") +
  theme_light() +
  xlab("Componentes a retener") +
  ylab("% de varianza explicada") +
  labs(title = "") 

pca$var$cor

factoextra::fviz_pca_ind(pca, 
                         geom = c("point", "text"),
                         repel = TRUE, 
                         axes = c(1,2),
                         col.ind = "#C84630") +
  labs(title ="") +
  theme_light() +
  theme_set(theme_bw(base_family = "noto"))

factoextra::fviz_pca_ind(pca, 
                         geom = c("point", "text"), 
                         repel = TRUE, 
                         axes = c(1,3),
                         col.ind = "#C84630") +
  labs(title ="") +
  theme_light() +
  theme_set(theme_bw(base_family = "noto"))


# Servicios empresariales y prod. manufacturera

ggplot(dftmp) +
  aes(x = SERVEMP, y = MANUF, label = depto) +
  geom_point(col = "#C84630") + 
  theme_bw() + 
  geom_smooth(method = "lm", col = "darkred") + 
  ggrepel::geom_label_repel()


dfvar <- read_delim("datalimpia/var_manuf_serv.txt", delim = "\t")

ggplot(dfvar) +
  aes(x = variacion.manufactura, y = variacion.servicios, label = departamento) +
  geom_point(col = "#C84630") + 
  theme_bw() + 
  geom_smooth(method = "lm", col = "darkred") + 
  ggrepel::geom_label_repel()


varesp$depto <- rownames(varesp)

varesp %>% 
  ggplot() +
  aes(x = `Ind. Manuf.`, y = `Serv. Emp`, label = as.factor(depto)) +
  geom_point(col = "#C84630") + 
  theme_bw() + 
  geom_smooth(method = "lm", col = "darkred", se = FALSE) + 
  ggrepel::geom_label_repel() + 
  geom_hline(yintercept = 0, size = 0.15) +
  geom_vline(xintercept = 0, size = 0.15) +
  xlab("Variación del IER del sector manufacturero") +
  ylab("Variación del IER del sector de serv. Empresariales")

varesp <- varesp %>% 
  select(manuf = `Ind. Manuf.`,
         serv = `Serv. Emp`)

# 2021
esp2021$depto <- rownames(esp2021)
esp2021 %>% 
  ggplot() +
  aes(x = `Serv. Emp`, y = forcats::fct_reorder(depto,`Serv. Emp`)) +
  geom_col(fill = "Orange") +
  geom_vline(xintercept = 1)
  
esp2021 %>% 
  ggplot() +
  aes(x = `Ind. Manuf.`, y = forcats::fct_reorder(depto,`Ind. Manuf.`)) +
  geom_col(fill = "Orange") +
  geom_vline(xintercept = 1)

gesp2021 %>% 
  select(c("depto", "SERVEMP")) %>% 
  ggplot() +
  aes(y = forcats::fct_reorder(depto, -SERVEMP), x = SERVEMP) +
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

# df$depto <- gsub("\\.", " ", df$depto)

# mapdata <- sp::merge(x = shapeData, y = df, by.x = "departamen", by.y = "depto") 
varesp$departamen <- rownames(varesp)
varesp$departamen <- gsub("\\.", " ", varesp$departamen)
mapdata <- sp::merge(x = shapeData, y = varesp, by = "departamen")
  
  
# Análisis univariado del sector manufacturero
boxplot(mapdata$manuf)

# Mapa
tm_shape(mapdata) + tm_fill(col="serv",
                            style="quantile",
                            n=4,
                            palette="YlGnBu", 
                            title = "Esp. en Manufactura") +
  tm_legend(outside=TRUE) +
  tm_text("departamen", size = 2/3, col = "black")

# Definición de vecinos
nb <- poly2nb(mapdata, queen=TRUE)

# Asignar peso a los vecinos
lw <- nb2listw(nb, style="W", zero.policy=TRUE)

# Analisis con pesos rezagados
per.lag <- lag.listw(lw, mapdata$serv)
per.lag

plot(per.lag ~ mapdata$serv, pch=16, asp=1)
text(df$MANUF, (per.lag-0.02), labels=df$depto, cex =0.75)
M1 <- lm(per.lag ~ df$serv)
abline(M1, col="blue")
coef(M1)[2]

# Inferencia
moran.test(mapdata$serv, lw, alternative="greater")

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

