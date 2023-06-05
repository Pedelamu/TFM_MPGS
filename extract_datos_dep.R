setwd("C:/Users/Pedro/Desktop/TFM/R")

library(foreign)
library(tidyverse)

# Cargamos la base de datos original
data <- read.spss("./base_datos/datos_originales.sav", to.data.frame=TRUE)

# Seleccionamos los ítems de interés y calculamos la puntuación en el test para definir el diagnóstico
datos_phq <- data %>%
  select(c(ID, starts_with("PHQ_9"))) %>%
  select(c(ID, ends_with("REC"))) %>%
  mutate(punt_dep = rowSums(across(contains("PHQ")))) %>%
  mutate(diag_dep_W1 = punt_dep >= 10)

# Guardamos los datos
saveRDS(datos_phq, file = "base_datos/datos_dep.rds")

  
  