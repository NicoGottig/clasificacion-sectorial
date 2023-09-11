# Anexo de transformacion de datos
library(tidyverse)
library(janitor)
library(readxl)

# analisis historico de vab
raw <- read_excel("datos/depurado_sector_vab.xlsx") %>% data.frame()
colnames(raw) <- c("clae1", "sector", seq(2004, 2021, 1))
id_cols <- c("clae1", "sector")
df <- pivot_longer(raw, -all_of(id_cols), names_to = "anio", values_to = "VAB")
write_delim(df, "datalimpia/vab_xrama.txt", delim = "\t")

# analisis historico de trabajo por rama
dict_depto <- read.csv("datos/diccionario_cod_depto.csv")
dict_clae <- readxl::read_excel("datos/diccionario_clae2.xlsx")
dict_caes1 <- readxl::read_excel("datos/caes_1.xlsx")
raw <- read.csv("datos/puestos_depto_total_por_letra.csv") %>% 
  filter(id_provincia_indec == 30)
raw <- merge(raw, dict_depto, by = "codigo_departamento_indec")
raw <- merge(raw, dict_clae, by = "letra")
raw <- merge(raw, dict_caes1, by = "letra_desc")
raw$fecha <- as.Date(raw$fecha)
raw <- raw %>% 
  select(fecha, departamento = nombre_departamento_indec, letra, letra_desc, puestos, caes_1) %>% 
  mutate(anio = lubridate::year(fecha),
         mes = lubridate::month(fecha)) %>% 
  select(anio, mes, departamento, letra, letra_desc, caes_1, puestos)

raw <- raw %>% 
  select(-letra) %>% 
  group_by(anio, departamento, letra_desc, caes_1) %>% 
  summarise(puestos = round(mean(puestos), 0))

write_delim(raw, "datalimpia/trab_xanio.txt", delim = "\t")


# Para matriz de especialización relativa
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

# Data frame 2014 y 2021
df2021 <- raw %>% 
  filter(anio == 2021) %>% 
  group_by(anio, mes, departamento, letra_desc) %>% 
  summarise(puestos = sum(puestos))

df2021 <- df2021 %>% 
  group_by(anio, departamento, letra_desc) %>% 
  summarise(puestos = round(mean(puestos), 0))

df2021 <- merge(df2021, dict_caes1, by = "letra_desc")

df2021 <- df2021 %>%
  select(-c(letra_desc, anio)) %>% 
  group_by(departamento, caes_1) %>% 
  summarise(puestos = sum(puestos))

df2021$anio <- 2021

df2014 <- raw %>% 
  filter(anio == 2014) %>% 
  group_by(anio, mes, departamento, letra_desc) %>% 
  summarise(puestos = sum(puestos))

df2014 <- df2014 %>% 
  group_by(anio, departamento, letra_desc) %>% 
  summarise(puestos = round(mean(puestos), 0))

df2014 <- merge(df2014, dict_caes1, by = "letra_desc")

df2014 <- df2014 %>%
  select(-c(letra_desc, anio)) %>% 
  group_by(departamento, caes_1) %>% 
  summarise(puestos = sum(puestos))

df2014$anio <- 2014

df <- bind_rows(df2014, df2021)

write_delim(df, "datalimpia/df_puestos.txt", delim = "\t")

# Variacion de trabajo en servicios y produccion

df2014 <- df2014 %>% 
  pivot_wider(names_from = "caes_1", values_from = "puestos") %>% 
  select(departamento, MANUF2014 = MANUF, SERVEMP2014 = SERVEMP)

df2021 <- df2021 %>% 
  pivot_wider(names_from = "caes_1", values_from = "puestos") %>% 
  select(departamento, MANUF2021 = MANUF, SERVEMP2021 = SERVEMP)

dfvar <- merge(df2014, df2021, by = "departamento")

dfvar <- dfvar %>% 
  mutate(variacion.manufactura = round(MANUF2021/MANUF2014-1, 3),
         variacion.servicios = round(SERVEMP2021/MANUF2014-1, 3)) %>% 
  select(departamento, variacion.manufactura, variacion.servicios)

write_delim(dfvar, "datalimpia/var_manuf_serv.txt", delim = "\t")
