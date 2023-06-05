setwd("C:/Users/Pedro/Desktop/TFM/R")

library(foreign)
library(tidyverse)
library(optmatch)

# Cargamos la base de datos completa
dat <- readRDS("./base_datos/datos_total.rds")


# Definimos las variables qué vamos a utilizar para el matching
vars_for_match <- c("Sexo", "Edad", "Zona_vivienda", "Titulación", "Ingresos",
                    "super_resilient")


# Realizamos el matching
pm <- pairmatch(super_resilient ~., controls = 1, data = dat %>%
                  select(all_of(vars_for_match)))
pm

# Se excluyen aquellos casos que no han recibido un emparejamiento
match_dat <- dat %>%
  mutate(mgroup = pm) %>%
  filter(!is.na(mgroup))

# Se guardan tanto los datos totales como los de cada uno de los grupos por separado.
saveRDS(match_dat, file = "base_datos/datos_matching.rds")

dat_resilient <- match_dat %>%
  filter(super_resilient)
saveRDS(dat_resilient, file = "base_datos/datos_matching_resilient.rds")

dat_noresilient <- match_dat %>%
  filter(!super_resilient)
saveRDS(dat_noresilient, file = "base_datos/datos_matching_noresilient.rds")

