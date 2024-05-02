# funcion generadora de indices de especialización


# La función recibe como entrada:
# 1. Un dataframe con una columna denominada anio, departamento, cantidad de puestos y cae. (se debe considerar
# que para crear la matriz solo toma datos numericos mientras que para los nombres de las columnas toma caracteres)
# 2. Un parámetro (anio) sobre el que se calcula la matriz.

# Devuelve:
# Un dataframe con la especialización por sector y por departamento para el año solicitado


matrizEspecializacion <- function(df, anio){
  
  # filtra
  anio <- anio
  proxy <- df
  
  # vuelca
  proxy2 <- proxy %>%
    dplyr::filter(anio == anio) %>% 
    pivot_wider(names_from = departamento, values_from = puestos) %>% 
    select(-anio)
  
  # Seleccionar solo numericas
  col_df <- colnames(proxy2)[2:length(proxy2)]
  row_df <- proxy2$caes_1

  dfw <- data.frame(proxy2)
  
  # Calcula las sumatorias y crea la matriz
  sum_sect <- as.numeric(colSums(dfw[2:length(dfw)]))
  sum_reg <- rowSums(dfw[2:length(dfw)])
  tot <- sum(sum_reg)
  
  mat <- matrix(nrow = 7, ncol= 18)
  
  # Itera
  for (i in 1:7) {
    for(j in 1:17){
      mat[i, j] <- (dfw[i, j+1] / sum_sect[j]) / (sum_reg[i] / tot)
    }
  }
  
  mat <- mat[,1:17]
  
  # Almacena en un objeto nuevo que es el que devuelve
  mat <- data.frame(mat)
  colnames(mat) <- col_df
  mat$sector <- row_df
  
  mat <- mat %>% 
    select(sector, everything())
  
  # trasposición
  dff <- t(mat) %>% data.frame()
  colnames(dff) <- dff[1,]
  dff <- dff[-1, ]
  
  dff <- apply(dff, MARGIN = 2, FUN = as.numeric) %>% data.frame()
  dff$depto <- col_df
  
  # Devuelve dataframe
  tmp <- dff
  tmp$anio <- anio
  return(tmp)
}


