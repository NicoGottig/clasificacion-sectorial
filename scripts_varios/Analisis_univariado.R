library(tidyverse)

df <- read.delim("datos/trabajoIndustria2022.txt")

df_perent <- df %>% 
  group_by(depto) %>% 
  summarise(puestorel = round((puestos / sum(puestos)),3))

df <- df %>% 
  select(depto, desc_clae, puestos) %>% 
  mutate(puestorel = df_perent$puestorel)

rm(df_perent)

# frec. relativa de sector por departamento
depto <- unique(df$depto) 

for (i in 1:17) {
  loc <- depto[i]
  tmp <- df %>% 
    filter(depto == loc) %>% 
    arrange(-puestorel) %>% 
    head()
  
  print(tmp)
  print("")
  
  plt = tmp %>% 
    ggplot() + 
    geom_col(aes(y = reorder(desc_clae,puestorel), x = puestorel), fill = "orange", col = "black") + 
    theme_bw() + labs(title = depto[i])
  
  print(plt)
}


# Multiplicar por salario mediano
salar <- 