setwd("C:/Users/Pedro/Desktop/TFM/R")

library(foreign)
library(tidyverse)

# Cargamos los datos de los 3 instrumentos. La base de trauma ya incluye datos sociodemográficos
datos_trauma <- readRDS("./base_datos/datos_trauma.rds")
datos_dep <- readRDS("./base_datos/datos_dep.rds")
datos_ans <- readRDS("./base_datos/datos_ans.rds")

# Unimos las tres bases de datos mediante el ID forzándolo a la base ya filtrada de trauma
datos_trauma_dep <- merge(datos_trauma, datos_dep, by = "ID")
datos_trauma_dep_ans <- merge(datos_trauma_dep, datos_ans, by = "ID")

#Guardamos la base de datos completa
saveRDS(datos_trauma_dep_ans, file = "base_datos/datos_trauma_dep_ans.rds")
saveRDS(datos_trauma_dep_ans, file = "base_datos/datos_total.rds")