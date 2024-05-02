# Carga dataframes
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

# Data frame de los datos para el 2020
df <- raw %>% 
  filter(anio == 2022) %>% 
  group_by(anio, mes, departamento, letra_desc) %>% 
  summarise(puestos = sum(puestos))

df <- df %>% 
  group_by(anio, departamento, letra_desc) %>% 
  summarise(puestos = round(mean(puestos), 0))

df <- merge(df, dict_caes1, by = "letra_desc")

df <- df %>%
  select(-c(letra_desc, anio)) %>% 
  group_by(departamento, caes_1) %>% 
  summarise(puestos = sum(puestos))

# Indice de especialización relativa
dfw <- df %>% 
  pivot_wider(names_from = departamento, values_from = puestos) 

col_df <- colnames(dfw)[2:18]
row_df <- dfw$caes_1

dfw <- dfw[2:18] %>% 
  data.frame()

sum_sect <- colSums(dfw[1:17])
sum_reg <- rowSums(dfw[1:17])
tot <- sum(sum_reg)

mat <- matrix(nrow = 7, ncol= 17)

# EjVij
sum(colSums(dfw[dfw$caes_1 == "COMHOTREST", 2:18]))

# EiVij
sum(dfw$Gualeguaychú)


for (i in 1:7) {
  for(j in 1:17){
    mat[i, j] <- (dfw[i, j] / sum_sect[j]) / (sum_reg[i] / tot)
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

pca <- FactoMineR::PCA(dff, scale.unit = TRUE)
