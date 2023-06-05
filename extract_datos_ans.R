setwd("C:/Users/Pedro/Desktop/TFM/R")

library(foreign)
library(tidyverse)

# Cargamos la base de datos original
data <- read.spss("./base_datos/datos_originales.sav", to.data.frame=TRUE)

# Seleccionamos los ítems de interés y calculamos la puntuación en el test para definir el diagnóstico
datos_gad <- data %>%
  select(c(ID, starts_with("GAD"))) %>%
  select(c(ID, ends_with("REC"))) %>%
  mutate(punt_ans = rowSums(across(contains("GAD")))) %>%
  mutate(diag_ans_W1 = punt_ans >= 10)

# Guardamos los datos
saveRDS(datos_gad, file = "base_datos/datos_ans.rds")
