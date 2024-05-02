
# librerias y carga de datos
library(tidyverse)

# Importamos y completamos los datos
dict_depto <- read.csv("datos/diccionario_cod_depto.csv")
dict_clae <- readxl::read_excel("datos/diccionario_clae2.xlsx")
dict_caes1 <- readxl::read_excel("datos/caes_1.xlsx")

raw <- read.csv("datos/puestos_depto_tot_emp_por_clae2.csv") %>% 
  filter(id_provincia_indec == 30)
raw <- merge(raw, dict_depto, by = "codigo_departamento_indec")
raw <- merge(raw, dict_clae, by = "clae2")
raw$fecha <- as.Date(raw$fecha)

# Seleccionamos las columnas de interes
raw <- raw %>% 
  select(fecha, departamento = nombre_departamento_indec, letra, letra_desc, puestos) %>% 
  mutate(anio = lubridate::year(fecha),
         mes = lubridate::month(fecha)) %>% 
  select(anio, mes, departamento, letra, letra_desc, puestos)

df <- merge(raw, dict_caes1, by = "letra_desc")

# Exporto datos
library(openxlsx)
nombre_archivo <- "empleo_filtrado.xlsx"
wb <- createWorkbook()

# Agrega los dataframes a hojas separadas en el libro
# addWorksheet(wb, "empleo")
# writeData(wb, "empleo", df)
# saveWorkbook(wb, nombre_archivo)

# Corregimos nulos y errores
df$puestos <- ifelse(df$puestos < 0, 0, df$puestos)
df$puestos <- ifelse(is.na(df$puestos) < 0, 0, df$puestos)

# Agregamos los puestos a menos dígitos
df <- df %>% 
  group_by(anio, mes, departamento, caes_1) %>% 
  summarise(puestos = sum(puestos))

# Tomamos los valores de enero (es el mes del ultimo dato)
df <- df %>%
  filter(mes == 1)

df <- data.frame(df)

# volteamos datos para 2014 y 2023
dfw_2014 <- df %>% 
  filter(anio == 2014) %>% 
  select(-mes) %>% 
  pivot_wider(names_from = departamento, values_from = puestos) %>% 
  select(-anio)
dfw_2014 <- as.data.frame(dfw_2014)

dfw_2023 <- df %>% 
  filter(anio == 2023) %>% 
  select(-mes) %>% 
  pivot_wider(names_from = departamento, values_from = puestos) %>% 
  select(-anio)
dfw_2023 <- as.data.frame(dfw_2023)

col_df <- colnames(dfw_2023)[2:18]
row_df <- dfw_2023$caes_1
dfw_2014 <- as.data.frame(dfw_2014[,-1])
dfw_2023 <- as.data.frame(dfw_2023[,-1])

# Matriz de especialización relativa para 2014 ####
sum_sect <- colSums(dfw_2014[1:17])
sum_reg <- rowSums(dfw_2014[1:17])
tot <- sum(sum_reg)
mat <- matrix(nrow = 7, ncol= 17)

for (i in 1:7) {
  for(j in 1:17){
    mat[i, j] <- (dfw_2014[i, j] / sum_sect[j]) / (sum_reg[i] / tot)
  }
}

mat <- data.frame(mat)
colnames(mat) <- col_df
mat <- bind_cols(sector = unique(row_df), mat)

# trasposición
dff <- t(mat) %>% data.frame()
colnames(dff) <- dff[1,]
dff <- dff[-1, ]

dff <- apply(dff, MARGIN = 2, FUN = as.numeric) %>% data.frame()
dff <- bind_cols(depto = col_df, dff)

esp_2014 <- dff
esp_2014$anio <- 2014

# Matriz de especialización relativa para 2023 ####
sum_sect <- colSums(dfw_2023[1:17])
sum_reg <- rowSums(dfw_2023[1:17])
tot <- sum(sum_reg)
mat <- matrix(nrow = 7, ncol= 17)

for (i in 1:7) {
  for(j in 1:17){
    mat[i, j] <- (dfw_2023[i, j] / sum_sect[j]) / (sum_reg[i] / tot)
  }
}
mat <- data.frame(mat)
colnames(mat) <- col_df
mat <- bind_cols(sector = unique(row_df), mat)

# trasposición
dff <- t(mat) %>% data.frame()
colnames(dff) <- dff[1,]
dff <- dff[-1, ]

dff <- apply(dff, MARGIN = 2, FUN = as.numeric) %>% data.frame()
dff <- bind_cols(depto = col_df, dff)

esp_2023 <- dff
esp_2023$anio <- 2023

# Cálculo de variacion relativa ####
esp_var <- (esp_2023[2:8] - esp_2014[2:8]) / esp_2014[2:8]

rownames(esp_var) <- esp_2023$depto
Sys.setlocale("LC_CTYPE", "en_US.UTF-8")
colnames(esp_var) <- c("Com. Hot. y Rest.", "Construcción",
                      "Ens. y Salud", "Manufactura", "Mat. Prima", 
                      "Serv. Emp.", "Serv. Soc. y Admin")


# Exporto datos
library(openxlsx)
nombre_archivo <- "especializaciones.xlsx"
wb <- createWorkbook()

# Agrega los dataframes a hojas separadas en el libro
addWorksheet(wb, "esp_2014")
writeData(wb, "esp_2014", esp_2014)
addWorksheet(wb, "esp_2023")
writeData(wb, "esp_2023", esp_2023)
addWorksheet(wb, "esp_dif")
writeData(wb, "esp_dif", esp_var)

saveWorkbook(wb, "especializaciones.xlsx")

# Bivariados ####
x11()
esp_var %>%
  mutate(depto = rownames(esp_var)) %>% 
  pivot_longer(-c(Manufactura, depto), values_to = "val", names_to = "name") %>% 
  ggplot(aes(x = val*100, y = Manufactura*100, label = depto)) +
  geom_point(size = 2) +
  ylab("Variación en manufactura") +
  xlab("") +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ name, scales = "free") +
  theme_bw() +
  ggrepel::geom_text_repel(segment.size = 0.2)  # Agregar etiquetas con ggrepel

cor(esp_var)

# Realizo el mismo procedimiento pero sin islas del Ibicuy y sin Feliciano ####
dict_depto <- read.csv("datos/diccionario_cod_depto.csv")
dict_clae <- readxl::read_excel("datos/diccionario_clae2.xlsx")
dict_caes1 <- readxl::read_excel("datos/caes_1.xlsx")
raw <- read.csv("datos/puestos_depto_tot_emp_por_clae2.csv") %>% 
  filter(id_provincia_indec == 30)
raw <- merge(raw, dict_depto, by = "codigo_departamento_indec")
raw <- merge(raw, dict_clae, by = "clae2")
raw$fecha <- as.Date(raw$fecha)

# Seleccionamos las columnas de interes
raw <- raw %>% 
  select(fecha, departamento = nombre_departamento_indec, letra, letra_desc, puestos) %>% 
  mutate(anio = lubridate::year(fecha),
         mes = lubridate::month(fecha)) %>% 
  select(anio, mes, departamento, letra, letra_desc, puestos)
df <- merge(raw, dict_caes1, by = "letra_desc")

# Agregamos los puestos a menos dígitos
df <- df %>% 
  group_by(anio, mes, departamento, caes_1) %>% 
  summarise(puestos = sum(puestos))

# Corregimos nulos y errores
df$puestos <- ifelse(df$puestos < 0, 0, df$puestos)
df$puestos <- ifelse(is.na(df$puestos) < 0, 0, df$puestos)

# Tomamos los valores de enero (es el mes del ultimo dato) y sacar islas del ibicuy y feliciano
df <- df %>%
  filter(mes == 1) %>% 
  filter(!(departamento %in% c("Feliciano","Islas del Ibicuy")))
df <- data.frame(df)

# volteamos datos para 2014 y 2023 (sin islas)
dfw_2014 <- df %>% 
  filter(anio == 2014) %>% 
  select(-mes) %>% 
  pivot_wider(names_from = departamento, values_from = puestos) %>% 
  select(-anio)
dfw_2014 <- as.data.frame(dfw_2014)

dfw_2023 <- df %>% 
  filter(anio == 2023) %>% 
  select(-mes) %>% 
  pivot_wider(names_from = departamento, values_from = puestos) %>% 
  select(-anio)
dfw_2023 <- as.data.frame(dfw_2023)

col_df <- colnames(dfw_2023)[2:16]
row_df <- dfw_2023$caes_1
dfw_2014 <- as.data.frame(dfw_2014[,-1])
dfw_2023 <- as.data.frame(dfw_2023[,-1])

# Matriz de especialización relativa para 2014 ####
sum_sect <- colSums(dfw_2014[1:15])
sum_reg <- rowSums(dfw_2014[1:15])
tot <- sum(sum_reg)
mat <- matrix(nrow = 7, ncol= 15)

for (i in 1:7) {
  for(j in 1:15){
    mat[i, j] <- (dfw_2014[i, j] / sum_sect[j]) / (sum_reg[i] / tot)
  }
}

mat <- data.frame(mat)
colnames(mat) <- col_df
mat <- bind_cols(sector = unique(row_df), mat)

# trasposición
dff <- t(mat) %>% data.frame()
colnames(dff) <- dff[1,]
dff <- dff[-1, ]

dff <- apply(dff, MARGIN = 2, FUN = as.numeric) %>% data.frame()
dff <- bind_cols(depto = col_df, dff)

esp_2014 <- dff
esp_2014$anio <- 2014
    
# Matriz de especialización relativa para 2023 ####
sum_sect <- colSums(dfw_2023[1:15])
sum_reg <- rowSums(dfw_2023[1:15])
tot <- sum(sum_reg)
mat <- matrix(nrow = 7, ncol= 15)

for (i in 1:7) {
  for(j in 1:15){
    mat[i, j] <- (dfw_2023[i, j] / sum_sect[j]) / (sum_reg[i] / tot)
  }
}
mat <- data.frame(mat)
colnames(mat) <- col_df
mat <- bind_cols(sector = unique(row_df), mat)

# trasposición
dff <- t(mat) %>% data.frame()
colnames(dff) <- dff[1,]
dff <- dff[-1, ]

dff <- apply(dff, MARGIN = 2, FUN = as.numeric) %>% data.frame()
dff <- bind_cols(depto = col_df, dff)

esp_2023 <- dff
esp_2023$anio <- 2023

# Cálculo de variacion relativa ####
esp_var <- (esp_2023[2:8] - esp_2014[2:8])

rownames(esp_var) <- esp_2023$depto
Sys.setlocale("LC_CTYPE", "en_US.UTF-8")
colnames(esp_var) <- c("Com. Hot. y Rest.", "Construcción",
                       "Ens. y Salud", "Manufactura", "Mat. Prima", 
                       "Serv. Emp.", "Serv. Soc. y Admin")


# Exporto datos
library(openxlsx)
nombre_archivo <- "especializaciones_sin_Feliciano_Ibicuy.xlsx"
wb <- createWorkbook()

# Agrega los dataframes a hojas separadas en el libro
addWorksheet(wb, "esp_2014")
writeData(wb, "esp_2014", esp_2014)
addWorksheet(wb, "esp_2023")
writeData(wb, "esp_2023", esp_2023)
addWorksheet(wb, "esp_dif")
writeData(wb, "esp_dif", esp_var)

saveWorkbook(wb, nombre_archivo)

# Bivariados ####
esp_var %>%
  mutate(depto = rownames(esp_var)) %>% 
  pivot_longer(-c(Manufactura, depto), values_to = "val", names_to = "name") %>% 
  ggplot(aes(x = val, y = Manufactura, label = depto)) +
  geom_point(size = 2) +
  ylab("Variación en manufactura") +
  xlab("") +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ name, scales = "free") +
  theme_bw() +
  ggrepel::geom_text_repel(segment.size = 0.2)  # Agregar etiquetas con ggrepel

cor(esp_var)

# Elimino todos los df que no necesito
rm(df, dff, dfw_2014, dfw_2023, dict_caes1, dict_clae, dict_depto, mat, raw)
rm(col_df, i, j, row_df, sum_reg, sum_sect, tot)

# Análisis espacial ####
mi_paleta <- c("#002D72", "#0053A0", "#0078CE", "#C8102E","steelblue", "#FFC72C", "firebrick2")

# Convertir el dataframe en formato largo (long format)
esp_var_long <- tidyr::gather(esp_var, key = "variable", value = "value")

# Crear el boxplot utilizando ggplot con facet_wrap y theme_bw
ggplot(esp_var_long, aes(x = value, fill = variable)) +
  geom_histogram(bins = 5, col = "black") +
  scale_fill_manual(values = mi_paleta, guide = FALSE) +  # Utilizar la paleta de colores pasteles oscuros y ocultar la leyenda
  facet_wrap(~ variable, scales = "free") +  # Facetar según la variable y ajustar la escala de los ejes x
  theme_bw() +  # Aplicar el tema_bw
  labs(x = "", y = "Valor") 

# descriptivas
esp_var_long %>% 
  group_by(variable) %>% 
  summarise(promedio = mean(value),
            sd = sd(value),
            cv = sd / promedio)

# Cargo mapa 
library(sf)
library(spdep)
library(tmap)

shape <- rgdal::readOGR(dsn = "mapa/pxdptodatosok.shp", verbose = FALSE)
shapeData <- sp::spTransform(shape, sp::CRS("+proj=longlat +datum=WGS84 +no_defs"))
shapeData <- sf::st_as_sf(shapeData, wkt = 'polygons', crs = st_crs(4326)) # make sf
shapeData <- rmapshaper::ms_simplify(shapeData, keep = 0.01, keep_shapes = TRUE) %>%
  filter(provincia == "Entre Ríos")

esp_var$departamen <- rownames(esp_var)
mapdata <- sp::merge(x = shapeData, y = esp_var, by = "departamen")

# filtro deptos
mapdata <- mapdata %>% 
  dplyr::filter(departamen != "Feliciano")

# análisis de autocorrelación espacial

# definición de vecinos
nb <- poly2nb(mapdata, queen=TRUE)

# asigno pesos estandarizados por fila
lw <- nb2listw(nb, style="W", zero.policy=TRUE)

# Analisis con pesos rezagados
per.lag <- lag.listw(lw, mapdata$`Serv. Emp.`)
per.lag

plot(per.lag ~ mapdata$`Serv. Emp.`, pch=16, asp=1, ylab = "Pesos rezagados", xlab = "Var. Servicios Empresariales")
text(mapdata$`Serv. Emp.`, (per.lag-0.01), labels=mapdata$departamen, cex =0.75)
M1 <- lm(per.lag ~ mapdata$`Serv. Emp.`)
abline(M1, col="blue")
coef(M1)[2]

# Indice global y local
moran.test(mapdata$`Serv. Emp.`, lw, alternative="greater")
moranlocal <- localmoran(mapdata$`Serv. Emp.`, lw) %>% 
  data.frame()

mapdata <- mapdata %>% 
  mutate(lmoran = localmoran(x = `Serv. Emp.`, listw = lw)[, 1],
         lmoran_pval = localmoran(x = `Serv. Emp.`, listw = lw)[, 5]
  )

# Distribucion espacial del estadistico
ggplot(mapdata) +
  geom_sf(aes(fill = lmoran))+
  labs(fill = " Estadística local de Moran") +
  scale_fill_viridis_c() +
  theme_classic() +
  theme(axis.text.x = element_blank())

# mapa lisa
mapdata <- mapdata %>% 
  mutate(
    # Estandarizar el Gini y el Moran local a sus valores medios:
    st_gini = Manufactura - mean(Manufactura),
    st_lmoran = lmoran - mean(lmoran),
    # Crear la nueva variable categórica:
    cuadrante = case_when(
      lmoran_pval > 0.05 ~ "Insignificante",
      st_gini > 0 & st_lmoran > 0 ~ "Alto-Alto",
      st_gini < 0 & st_lmoran < 0 ~ "Bajo-Bajo",
      st_gini < 0 & st_lmoran > 0 ~ "Bajo-Alto",
      st_gini > 0 & st_lmoran < 0 ~ "Alto-Bajo"
    )
  )

# mapa
ggplot(mapdata, aes(fill = cuadrante)) +
  geom_sf()+
  labs(fill = "Cuadrante")+
  scale_fill_manual(values = c("red", "lightblue", "white")) +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank())

# Analisis de componentes principales ####
componentes <- FactoMineR::PCA(esp_var, scale.unit = T)
summary(componentes)

con_cluster <- esp_var
con_cluster$cluster <- kmeans(scale(esp_var), 2)$cluster %>% matrix()
con_cluster$cluster <- as.factor(con_cluster$cluster)

con_cluster <- cbind(con_cluster, componentes$ind$coord)

# 3 dimensiones para buscar grupos
library(plotly)
colors <- c('#4AC6B7', 
            '#1972A4', 
            '#965F8A', 
            '#FF7070',
            '#C61951')

# Grafico
plot_ly(data = con_cluster,
        x = ~Dim.1,
        y = ~Dim.2,
        z = ~Dim.3,
        color = ~cluster,
        colors = colors,
        type = "scatter3d",
        size = 8) %>% 
  layout(title = "PCAS y provincias")

# coordenadas
factoextra::fviz_pca_ind(componentes, repel = T)
