# Con islas del Ibicuy ####


#### Para 2014 ####
library(tidyverse)

# Importamos y completamos los datos
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

# Calculamos la suma (agregamos) y el promedio por año
df <- merge(raw, dict_caes1, by = "letra_desc")

df <- df %>% 
  group_by(anio, mes, departamento, caes_1) %>% 
  summarise(puestos = sum(puestos))

# Corregimos nulos y errores
df$puestos <- ifelse(df$puestos < 0, 0, df$puestos)
df$puestos <- ifelse(is.na(df$puestos) < 0, 0, df$puestos)

df <- df %>% 
  group_by(anio, departamento, caes_1) %>% 
  summarise(puestos = round(mean(puestos), 0))

df <- data.frame(df)


# volteamos datos 
dfw <- df %>% 
  filter(anio == 2014) %>% 
  pivot_wider(names_from = departamento, values_from = puestos) %>% 
  select(-anio)

# Primero calculamos la especializacion para un año
col_df <- colnames(dfw)[2:18]
row_df <- dfw$caes_1

dfw <- dfw[2:18] %>% 
  data.frame()

# Seleccionar solo numericas
sum_sect <- colSums(dfw[1:17])
sum_reg <- rowSums(dfw[1:17])
tot <- sum(sum_reg)

mat <- matrix(nrow = 7, ncol= 17)

for (i in 1:7) {
  for(j in 1:17){
    mat[i, j] <- (dfw[i, j] / sum_sect[j]) / (sum_reg[i] / tot)
  }
}

mat <- data.frame(mat)
colnames(mat) <- col_df
mat$sector <- as.vector(row_df)
mat <- mat %>% 
  select(sector, everything())

# trasposición
dff <- t(mat) %>% data.frame()
colnames(dff) <- dff[1,]
dff <- dff[-1, ]

dff <- apply(dff, MARGIN = 2, FUN = as.numeric) %>% data.frame()
dff <- bind_cols(depto = col_df, dff)

esp_2014 <- dff
esp_2014$anio = 2014

#### Para 2015 ####
# volteamos datos 
dfw <- df %>% 
  filter(anio == 2015) %>% 
  pivot_wider(names_from = departamento, values_from = puestos) %>% 
  select(-anio)

# Primero calculamos la especializacion para un año
col_df <- colnames(dfw)[2:18]
row_df <- dfw$caes_1

dfw <- dfw[2:18] %>% 
  data.frame()

# Seleccionar solo numericas

sum_sect <- colSums(dfw[1:17])
sum_reg <- rowSums(dfw[1:17])
tot <- sum(sum_reg)

mat <- matrix(nrow = 7, ncol= 17)

for (i in 1:7) {
  for(j in 1:17){
    mat[i, j] <- (dfw[i, j] / sum_sect[j]) / (sum_reg[i] / tot)
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

esp_2015 <- dff
esp_2015$anio <- 2015

#### Para 2016 ####
# volteamos datos 
dfw <- df %>% 
  filter(anio == 2016) %>% 
  pivot_wider(names_from = departamento, values_from = puestos) %>% 
  select(-anio)

# Primero calculamos la especializacion para un año
col_df <- colnames(dfw)[2:18]
row_df <- dfw$caes_1

dfw <- dfw[2:18] %>% 
  data.frame()

# Seleccionar solo numericas

sum_sect <- colSums(dfw[1:17])
sum_reg <- rowSums(dfw[1:17])
tot <- sum(sum_reg)

mat <- matrix(nrow = 7, ncol= 17)

for (i in 1:7) {
  for(j in 1:17){
    mat[i, j] <- (dfw[i, j] / sum_sect[j]) / (sum_reg[i] / tot)
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

esp_2016 <- dff
esp_2016$anio <- 2016

#### Para 2017 ####
# volteamos datos 
dfw <- df %>% 
  filter(anio == 2017) %>% 
  pivot_wider(names_from = departamento, values_from = puestos) %>% 
  select(-anio)

# Primero calculamos la especializacion para un año
col_df <- colnames(dfw)[2:18]
row_df <- dfw$caes_1

dfw <- dfw[2:18] %>% 
  data.frame()

# Seleccionar solo numericas

sum_sect <- colSums(dfw[1:17])
sum_reg <- rowSums(dfw[1:17])
tot <- sum(sum_reg)

mat <- matrix(nrow = 7, ncol= 17)

for (i in 1:7) {
  for(j in 1:17){
    mat[i, j] <- (dfw[i, j] / sum_sect[j]) / (sum_reg[i] / tot)
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

esp_2017 <- dff
esp_2017$anio <- 2017

#### Para 2018 ####
# volteamos datos 
dfw <- df %>% 
  filter(anio == 2018) %>% 
  pivot_wider(names_from = departamento, values_from = puestos) %>% 
  select(-anio)

# Primero calculamos la especializacion para un año
col_df <- colnames(dfw)[2:18]
row_df <- dfw$caes_1

dfw <- dfw[2:18] %>% 
  data.frame()

# Seleccionar solo numericas

sum_sect <- colSums(dfw[1:17])
sum_reg <- rowSums(dfw[1:17])
tot <- sum(sum_reg)

mat <- matrix(nrow = 7, ncol= 17)

for (i in 1:7) {
  for(j in 1:17){
    mat[i, j] <- (dfw[i, j] / sum_sect[j]) / (sum_reg[i] / tot)
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

esp_2018 <- dff
esp_2018$anio <- 2018

#### Para 2019 ####
# volteamos datos 
dfw <- df %>% 
  filter(anio == 2019) %>% 
  pivot_wider(names_from = departamento, values_from = puestos) %>% 
  select(-anio)

# Primero calculamos la especializacion para un año
col_df <- colnames(dfw)[2:18]
row_df <- dfw$caes_1

dfw <- dfw[2:18] %>% 
  data.frame()

# Seleccionar solo numericas

sum_sect <- colSums(dfw[1:17])
sum_reg <- rowSums(dfw[1:17])
tot <- sum(sum_reg)

mat <- matrix(nrow = 7, ncol= 17)

for (i in 1:7) {
  for(j in 1:17){
    mat[i, j] <- (dfw[i, j] / sum_sect[j]) / (sum_reg[i] / tot)
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

esp_2019 <- dff
esp_2019$anio <- 2019

#### Para 2020 ####
# volteamos datos 
dfw <- df %>% 
  filter(anio == 2020) %>% 
  pivot_wider(names_from = departamento, values_from = puestos) %>% 
  select(-anio)

# Primero calculamos la especializacion para un año
col_df <- colnames(dfw)[2:18]
row_df <- dfw$caes_1

dfw <- dfw[2:18] %>% 
  data.frame()

# Seleccionar solo numericas

sum_sect <- colSums(dfw[1:17])
sum_reg <- rowSums(dfw[1:17])
tot <- sum(sum_reg)

mat <- matrix(nrow = 7, ncol= 17)

for (i in 1:7) {
  for(j in 1:17){
    mat[i, j] <- (dfw[i, j] / sum_sect[j]) / (sum_reg[i] / tot)
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

esp_2020 <- dff
esp_2020$anio <- 2020

#### Para 2021 ####
# volteamos datos 
dfw <- df %>% 
  filter(anio == 2021) %>% 
  pivot_wider(names_from = departamento, values_from = puestos) %>% 
  select(-anio)

# Primero calculamos la especializacion para un año
col_df <- colnames(dfw)[2:18]
row_df <- dfw$caes_1

dfw <- dfw[2:18] %>% 
  data.frame()

# Seleccionar solo numericas

sum_sect <- colSums(dfw[1:17])
sum_reg <- rowSums(dfw[1:17])
tot <- sum(sum_reg)

mat <- matrix(nrow = 7, ncol= 17)

for (i in 1:7) {
  for(j in 1:17){
    mat[i, j] <- (dfw[i, j] / sum_sect[j]) / (sum_reg[i] / tot)
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

esp_2021 <- dff
esp_2021$anio <- 2021

#### Para 2022 ####
# volteamos datos 
dfw <- df %>% 
  filter(anio == 2022) %>% 
  pivot_wider(names_from = departamento, values_from = puestos) %>% 
  select(-anio)

# Primero calculamos la especializacion para un año
col_df <- colnames(dfw)[2:18]
row_df <- dfw$caes_1

dfw <- dfw[2:18] %>% 
  data.frame()

# Seleccionar solo numericas

sum_sect <- colSums(dfw[1:17])
sum_reg <- rowSums(dfw[1:17])
tot <- sum(sum_reg)

mat <- matrix(nrow = 7, ncol= 17)

for (i in 1:7) {
  for(j in 1:17){
    mat[i, j] <- (dfw[i, j] / sum_sect[j]) / (sum_reg[i] / tot)
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

esp_2022 <- dff
esp_2022$anio <- 2022


### Guardado ####
especializacion.relativa <- rbind(esp_2014, 
                                      esp_2015, 
                                      esp_2016, 
                                      esp_2017, 
                                      esp_2018, 
                                      esp_2019, 
                                      esp_2020, 
                                      esp_2021,
                                  esp_2022) %>% 
  data.frame()

write_delim(especializacion.relativa, "matriz-especializacion-completa.txt", delim = "\t")


# Sin islas del Ibicuy ####

#### Para 2014 ####
library(tidyverse)

# Importamos y completamos los datos
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

# Calculamos la suma (agregamos) y el promedio por año
df <- merge(raw, dict_caes1, by = "letra_desc")

df <- df %>%
  filter(departamento != "Islas del Ibicuy")

df <- df %>% 
  group_by(anio, mes, departamento, caes_1) %>% 
  summarise(puestos = sum(puestos))

# Corregimos nulos y errores
df$puestos <- ifelse(df$puestos < 0, 0, df$puestos)
df$puestos <- ifelse(is.na(df$puestos) < 0, 0, df$puestos)

df <- df %>% 
  group_by(anio, departamento, caes_1) %>% 
  summarise(puestos = round(mean(puestos), 0))

df <- data.frame(df)


# volteamos datos 
dfw <- df %>% 
  filter(anio == 2014) %>% 
  pivot_wider(names_from = departamento, values_from = puestos) %>% 
  select(-anio)

# Primero calculamos la especializacion para un año
col_df <- colnames(dfw)[2:17]
row_df <- dfw$caes_1

dfw <- dfw[2:17] %>% 
  data.frame()

# Seleccionar solo numericas
sum_sect <- colSums(dfw[1:16])
sum_reg <- rowSums(dfw[1:16])
tot <- sum(sum_reg)

mat <- matrix(nrow = 7, ncol= 16)

for (i in 1:7) {
  for(j in 1:16){
    mat[i, j] <- (dfw[i, j] / sum_sect[j]) / (sum_reg[i] / tot)
  }
}

mat <- data.frame(mat)
colnames(mat) <- col_df
mat$sector <- as.vector(row_df)
mat <- mat %>% 
  select(sector, everything())

# trasposición
dff <- t(mat) %>% data.frame()
colnames(dff) <- dff[1,]
dff <- dff[-1, ]

dff <- apply(dff, MARGIN = 2, FUN = as.numeric) %>% data.frame()
dff <- bind_cols(depto = col_df, dff)

esp_2014 <- dff
esp_2014$anio = 2014

#### Para 2015 ####

# Importamos y completamos los datos
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

# Calculamos la suma (agregamos) y el promedio por año
df <- merge(raw, dict_caes1, by = "letra_desc")

df <- df %>%
  filter(departamento != "Islas del Ibicuy")

df <- df %>% 
  group_by(anio, mes, departamento, caes_1) %>% 
  summarise(puestos = sum(puestos))

# Corregimos nulos y errores
df$puestos <- ifelse(df$puestos < 0, 0, df$puestos)
df$puestos <- ifelse(is.na(df$puestos) < 0, 0, df$puestos)

df <- df %>% 
  group_by(anio, departamento, caes_1) %>% 
  summarise(puestos = round(mean(puestos), 0))

df <- data.frame(df)


# volteamos datos 
dfw <- df %>% 
  filter(anio == 2015) %>% 
  pivot_wider(names_from = departamento, values_from = puestos) %>% 
  select(-anio)

# Primero calculamos la especializacion para un año
col_df <- colnames(dfw)[2:17]
row_df <- dfw$caes_1

dfw <- dfw[2:17] %>% 
  data.frame()

# Seleccionar solo numericas
sum_sect <- colSums(dfw[1:16])
sum_reg <- rowSums(dfw[1:16])
tot <- sum(sum_reg)

mat <- matrix(nrow = 7, ncol= 16)

for (i in 1:7) {
  for(j in 1:16){
    mat[i, j] <- (dfw[i, j] / sum_sect[j]) / (sum_reg[i] / tot)
  }
}

mat <- data.frame(mat)
colnames(mat) <- col_df
mat$sector <- as.vector(row_df)
mat <- mat %>% 
  select(sector, everything())

# trasposición
dff <- t(mat) %>% data.frame()
colnames(dff) <- dff[1,]
dff <- dff[-1, ]

dff <- apply(dff, MARGIN = 2, FUN = as.numeric) %>% data.frame()
dff <- bind_cols(depto = col_df, dff)

esp_2015 <- dff
esp_2015$anio = 2015

#### Para 2016 ####
library(tidyverse)

# Importamos y completamos los datos
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

# Calculamos la suma (agregamos) y el promedio por año
df <- merge(raw, dict_caes1, by = "letra_desc")

df <- df %>%
  filter(departamento != "Islas del Ibicuy")

df <- df %>% 
  group_by(anio, mes, departamento, caes_1) %>% 
  summarise(puestos = sum(puestos))

# Corregimos nulos y errores
df$puestos <- ifelse(df$puestos < 0, 0, df$puestos)
df$puestos <- ifelse(is.na(df$puestos) < 0, 0, df$puestos)

df <- df %>% 
  group_by(anio, departamento, caes_1) %>% 
  summarise(puestos = round(mean(puestos), 0))

df <- data.frame(df)


# volteamos datos 
dfw <- df %>% 
  filter(anio == 2016) %>% 
  pivot_wider(names_from = departamento, values_from = puestos) %>% 
  select(-anio)

# Primero calculamos la especializacion para un año
col_df <- colnames(dfw)[2:17]
row_df <- dfw$caes_1

dfw <- dfw[2:17] %>% 
  data.frame()

# Seleccionar solo numericas
sum_sect <- colSums(dfw[1:16])
sum_reg <- rowSums(dfw[1:16])
tot <- sum(sum_reg)

mat <- matrix(nrow = 7, ncol= 16)

for (i in 1:7) {
  for(j in 1:16){
    mat[i, j] <- (dfw[i, j] / sum_sect[j]) / (sum_reg[i] / tot)
  }
}

mat <- data.frame(mat)
colnames(mat) <- col_df
mat$sector <- as.vector(row_df)
mat <- mat %>% 
  select(sector, everything())

# trasposición
dff <- t(mat) %>% data.frame()
colnames(dff) <- dff[1,]
dff <- dff[-1, ]

dff <- apply(dff, MARGIN = 2, FUN = as.numeric) %>% data.frame()
dff <- bind_cols(depto = col_df, dff)

esp_2016 <- dff
esp_2016$anio = 2016

#### Para 2017 ####
library(tidyverse)

# Importamos y completamos los datos
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

# Calculamos la suma (agregamos) y el promedio por año
df <- merge(raw, dict_caes1, by = "letra_desc")

df <- df %>%
  filter(departamento != "Islas del Ibicuy")

df <- df %>% 
  group_by(anio, mes, departamento, caes_1) %>% 
  summarise(puestos = sum(puestos))

# Corregimos nulos y errores
df$puestos <- ifelse(df$puestos < 0, 0, df$puestos)
df$puestos <- ifelse(is.na(df$puestos) < 0, 0, df$puestos)

df <- df %>% 
  group_by(anio, departamento, caes_1) %>% 
  summarise(puestos = round(mean(puestos), 0))

df <- data.frame(df)


# volteamos datos 
dfw <- df %>% 
  filter(anio == 2017) %>% 
  pivot_wider(names_from = departamento, values_from = puestos) %>% 
  select(-anio)

# Primero calculamos la especializacion para un año
col_df <- colnames(dfw)[2:17]
row_df <- dfw$caes_1

dfw <- dfw[2:17] %>% 
  data.frame()

# Seleccionar solo numericas
sum_sect <- colSums(dfw[1:16])
sum_reg <- rowSums(dfw[1:16])
tot <- sum(sum_reg)

mat <- matrix(nrow = 7, ncol= 16)

for (i in 1:7) {
  for(j in 1:16){
    mat[i, j] <- (dfw[i, j] / sum_sect[j]) / (sum_reg[i] / tot)
  }
}

mat <- data.frame(mat)
colnames(mat) <- col_df
mat$sector <- as.vector(row_df)
mat <- mat %>% 
  select(sector, everything())

# trasposición
dff <- t(mat) %>% data.frame()
colnames(dff) <- dff[1,]
dff <- dff[-1, ]

dff <- apply(dff, MARGIN = 2, FUN = as.numeric) %>% data.frame()
dff <- bind_cols(depto = col_df, dff)

esp_2017 <- dff
esp_2017$anio = 2017

#### Para 2018 ####
library(tidyverse)

# Importamos y completamos los datos
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

# Calculamos la suma (agregamos) y el promedio por año
df <- merge(raw, dict_caes1, by = "letra_desc")

df <- df %>%
  filter(departamento != "Islas del Ibicuy")

df <- df %>% 
  group_by(anio, mes, departamento, caes_1) %>% 
  summarise(puestos = sum(puestos))

# Corregimos nulos y errores
df$puestos <- ifelse(df$puestos < 0, 0, df$puestos)
df$puestos <- ifelse(is.na(df$puestos) < 0, 0, df$puestos)

df <- df %>% 
  group_by(anio, departamento, caes_1) %>% 
  summarise(puestos = round(mean(puestos), 0))

df <- data.frame(df)


# volteamos datos 
dfw <- df %>% 
  filter(anio == 2018) %>% 
  pivot_wider(names_from = departamento, values_from = puestos) %>% 
  select(-anio)

# Primero calculamos la especializacion para un año
col_df <- colnames(dfw)[2:17]
row_df <- dfw$caes_1

dfw <- dfw[2:17] %>% 
  data.frame()

# Seleccionar solo numericas
sum_sect <- colSums(dfw[1:16])
sum_reg <- rowSums(dfw[1:16])
tot <- sum(sum_reg)

mat <- matrix(nrow = 7, ncol= 16)

for (i in 1:7) {
  for(j in 1:16){
    mat[i, j] <- (dfw[i, j] / sum_sect[j]) / (sum_reg[i] / tot)
  }
}

mat <- data.frame(mat)
colnames(mat) <- col_df
mat$sector <- as.vector(row_df)
mat <- mat %>% 
  select(sector, everything())

# trasposición
dff <- t(mat) %>% data.frame()
colnames(dff) <- dff[1,]
dff <- dff[-1, ]

dff <- apply(dff, MARGIN = 2, FUN = as.numeric) %>% data.frame()
dff <- bind_cols(depto = col_df, dff)

esp_2018 <- dff
esp_2018$anio = 2018

#### Para 2019 ####
library(tidyverse)

# Importamos y completamos los datos
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

# Calculamos la suma (agregamos) y el promedio por año
df <- merge(raw, dict_caes1, by = "letra_desc")

df <- df %>%
  filter(departamento != "Islas del Ibicuy")

df <- df %>% 
  group_by(anio, mes, departamento, caes_1) %>% 
  summarise(puestos = sum(puestos))

# Corregimos nulos y errores
df$puestos <- ifelse(df$puestos < 0, 0, df$puestos)
df$puestos <- ifelse(is.na(df$puestos) < 0, 0, df$puestos)

df <- df %>% 
  group_by(anio, departamento, caes_1) %>% 
  summarise(puestos = round(mean(puestos), 0))

df <- data.frame(df)


# volteamos datos 
dfw <- df %>% 
  filter(anio == 2019) %>% 
  pivot_wider(names_from = departamento, values_from = puestos) %>% 
  select(-anio)

# Primero calculamos la especializacion para un año
col_df <- colnames(dfw)[2:17]
row_df <- dfw$caes_1

dfw <- dfw[2:17] %>% 
  data.frame()

# Seleccionar solo numericas
sum_sect <- colSums(dfw[1:16])
sum_reg <- rowSums(dfw[1:16])
tot <- sum(sum_reg)

mat <- matrix(nrow = 7, ncol= 16)

for (i in 1:7) {
  for(j in 1:16){
    mat[i, j] <- (dfw[i, j] / sum_sect[j]) / (sum_reg[i] / tot)
  }
}

mat <- data.frame(mat)
colnames(mat) <- col_df
mat$sector <- as.vector(row_df)
mat <- mat %>% 
  select(sector, everything())

# trasposición
dff <- t(mat) %>% data.frame()
colnames(dff) <- dff[1,]
dff <- dff[-1, ]

dff <- apply(dff, MARGIN = 2, FUN = as.numeric) %>% data.frame()
dff <- bind_cols(depto = col_df, dff)

esp_2019 <- dff
esp_2019$anio = 2019

#### Para 2020 ####
library(tidyverse)

# Importamos y completamos los datos
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

# Calculamos la suma (agregamos) y el promedio por año
df <- merge(raw, dict_caes1, by = "letra_desc")

df <- df %>%
  filter(departamento != "Islas del Ibicuy")

df <- df %>% 
  group_by(anio, mes, departamento, caes_1) %>% 
  summarise(puestos = sum(puestos))

# Corregimos nulos y errores
df$puestos <- ifelse(df$puestos < 0, 0, df$puestos)
df$puestos <- ifelse(is.na(df$puestos) < 0, 0, df$puestos)

df <- df %>% 
  group_by(anio, departamento, caes_1) %>% 
  summarise(puestos = round(mean(puestos), 0))

df <- data.frame(df)


# volteamos datos 
dfw <- df %>% 
  filter(anio == 2020) %>% 
  pivot_wider(names_from = departamento, values_from = puestos) %>% 
  select(-anio)

# Primero calculamos la especializacion para un año
col_df <- colnames(dfw)[2:17]
row_df <- dfw$caes_1

dfw <- dfw[2:17] %>% 
  data.frame()

# Seleccionar solo numericas
sum_sect <- colSums(dfw[1:16])
sum_reg <- rowSums(dfw[1:16])
tot <- sum(sum_reg)

mat <- matrix(nrow = 7, ncol= 16)

for (i in 1:7) {
  for(j in 1:16){
    mat[i, j] <- (dfw[i, j] / sum_sect[j]) / (sum_reg[i] / tot)
  }
}

mat <- data.frame(mat)
colnames(mat) <- col_df
mat$sector <- as.vector(row_df)
mat <- mat %>% 
  select(sector, everything())

# trasposición
dff <- t(mat) %>% data.frame()
colnames(dff) <- dff[1,]
dff <- dff[-1, ]

dff <- apply(dff, MARGIN = 2, FUN = as.numeric) %>% data.frame()
dff <- bind_cols(depto = col_df, dff)

esp_2020 <- dff
esp_2020$anio = 2020

#### Para 2021 ####
library(tidyverse)

# Importamos y completamos los datos
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

# Calculamos la suma (agregamos) y el promedio por año
df <- merge(raw, dict_caes1, by = "letra_desc")

df <- df %>%
  filter(departamento != "Islas del Ibicuy")

df <- df %>% 
  group_by(anio, mes, departamento, caes_1) %>% 
  summarise(puestos = sum(puestos))

# Corregimos nulos y errores
df$puestos <- ifelse(df$puestos < 0, 0, df$puestos)
df$puestos <- ifelse(is.na(df$puestos) < 0, 0, df$puestos)

df <- df %>% 
  group_by(anio, departamento, caes_1) %>% 
  summarise(puestos = round(mean(puestos), 0))

df <- data.frame(df)


# volteamos datos 
dfw <- df %>% 
  filter(anio == 2021) %>% 
  pivot_wider(names_from = departamento, values_from = puestos) %>% 
  select(-anio)

# Primero calculamos la especializacion para un año
col_df <- colnames(dfw)[2:17]
row_df <- dfw$caes_1

dfw <- dfw[2:17] %>% 
  data.frame()

# Seleccionar solo numericas
sum_sect <- colSums(dfw[1:16])
sum_reg <- rowSums(dfw[1:16])
tot <- sum(sum_reg)

mat <- matrix(nrow = 7, ncol= 16)

for (i in 1:7) {
  for(j in 1:16){
    mat[i, j] <- (dfw[i, j] / sum_sect[j]) / (sum_reg[i] / tot)
  }
}

mat <- data.frame(mat)
colnames(mat) <- col_df
mat$sector <- as.vector(row_df)
mat <- mat %>% 
  select(sector, everything())

# trasposición
dff <- t(mat) %>% data.frame()
colnames(dff) <- dff[1,]
dff <- dff[-1, ]

dff <- apply(dff, MARGIN = 2, FUN = as.numeric) %>% data.frame()
dff <- bind_cols(depto = col_df, dff)

esp_2021 <- dff
esp_2021$anio = 2021

#### Para 2022 ####
library(tidyverse)

# Importamos y completamos los datos
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

# Calculamos la suma (agregamos) y el promedio por año
df <- merge(raw, dict_caes1, by = "letra_desc")

df <- df %>%
  filter(departamento != "Islas del Ibicuy")

df <- df %>% 
  group_by(anio, mes, departamento, caes_1) %>% 
  summarise(puestos = sum(puestos))

# Corregimos nulos y errores
df$puestos <- ifelse(df$puestos < 0, 0, df$puestos)
df$puestos <- ifelse(is.na(df$puestos) < 0, 0, df$puestos)

df <- df %>% 
  group_by(anio, departamento, caes_1) %>% 
  summarise(puestos = round(mean(puestos), 0))

df <- data.frame(df)


# volteamos datos 
dfw <- df %>% 
  filter(anio == 2022) %>% 
  pivot_wider(names_from = departamento, values_from = puestos) %>% 
  select(-anio)

# Primero calculamos la especializacion para un año
col_df <- colnames(dfw)[2:17]
row_df <- dfw$caes_1

dfw <- dfw[2:17] %>% 
  data.frame()

# Seleccionar solo numericas
sum_sect <- colSums(dfw[1:16])
sum_reg <- rowSums(dfw[1:16])
tot <- sum(sum_reg)

mat <- matrix(nrow = 7, ncol= 16)

for (i in 1:7) {
  for(j in 1:16){
    mat[i, j] <- (dfw[i, j] / sum_sect[j]) / (sum_reg[i] / tot)
  }
}

mat <- data.frame(mat)
colnames(mat) <- col_df
mat$sector <- as.vector(row_df)
mat <- mat %>% 
  select(sector, everything())

# trasposición
dff <- t(mat) %>% data.frame()
colnames(dff) <- dff[1,]
dff <- dff[-1, ]

dff <- apply(dff, MARGIN = 2, FUN = as.numeric) %>% data.frame()
dff <- bind_cols(depto = col_df, dff)

esp_2022 <- dff
esp_2022$anio = 2022

### Guardado ####
especializacion.relativa.sinIslas <- rbind(esp_2014, 
                                  esp_2015, 
                                  esp_2016, 
                                  esp_2017, 
                                  esp_2018, 
                                  esp_2019, 
                                  esp_2020, 
                                  esp_2021,
                                  esp_2022) %>% 
  data.frame()

write_delim(especializacion.relativa.sinIslas, "matriz-especializacion-sinIslas.txt", delim = "\t")


