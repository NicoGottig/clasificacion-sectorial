library(tidyverse)

# Carga dataframe
df <- read.delim("datos/trabajoIndustria2022.txt")

# porcentaje
df_perent <- df %>% 
  group_by(depto) %>% 
  summarise(puestorel = (puestos / sum(puestos))*100)

df <- df %>% 
  select(depto, desc_clae) %>% 
  mutate(puestorel = df_perent$puestorel)
rm(df_perent)

# version wide
dft <- df %>% 
  pivot_wider(names_from = desc_clae, values_from = puestorel) %>% 
  as.data.frame()

dft[is.na(dft)] <- 0

# Boxplot
df %>% 
  ggplot() +
  aes(x = puestorel) +
  geom_histogram(bins = 5, col = "black", fill = "#22A699") +
  facet_wrap(~desc_clae, scales = "free") +
  theme_bw()

# Estandarizacion
minmax = function(x) (x - min(x)) / (max(x) - min(x))

dfe <- dft %>% 
  select_if(is.numeric) %>% 
  mutate_all(minmax) %>% 
  data.frame()

rownames(dfe) <- dft$depto

# correlación
GGally::ggcorr(dft[2:25], label=T, method=c("all.obs","pearson"))

# Componentes
cp <- FactoMineR::PCA(dft[2:25], scale.unit = TRUE, ncp = 10)
cp$eig

cp$var$cor

# Screeplot
factoextra::fviz_eig(cp, geom = "line")

# Grafico de cargas
windows()
factoextra::fviz_pca_var(cp, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)

# Grafico de scores
windows()
factoextra::fviz_pca_ind(cp,
                         geom.ind = "text",
                         repel = TRUE)

# Clusters
cpr <- cp$ind$coord %>% 
  data.frame()


library(NbClust)
res <- NbClust(cpr, distance = "manhattan",
               min.nc = 2, max.nc = 6,
               method = "complete", index = "ccc")

res$All.index
res$Best.nc
res$Best.partition
