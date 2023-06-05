setwd("C:/Users/Pedro/Desktop/TFM/R")

library(foreign)
library(tidyverse)
#library(ltm) #Este paquete carga las dependencias del paquete MASS, que tiene
#su propia función select que entra en conflicto con la  de dplyr.


# Cargamos las bases de datos
old_data <- read.spss("./base_datos/datos_originales.sav", to.data.frame=TRUE)
dat_ID <- readRDS("./base_datos/datos_total.rds")


# Utilizamos los ID de nuestra muestra para seleccionar los casos que nos interesan en la base total
ID_muestra <- dat_ID %>% pull(ID)
data <- old_data %>% filter(ID %in% ID_muestra)

# Seleccionamos todos los ítems que nos interesan
data_items <- data %>%
  select((contains("ITQ") | contains("GAD") | contains("PHQ")) &
           ends_with("REC"))

# Guardamos la base de datos de los ítems
saveRDS(data_items, file = "base_datos/data_items.rds")

############################################################################

# Separamos los ítems ITQ para cada una de las olas
data_ITQ <- data_items %>%
  select(contains("ITQ"))

data_ITQ_w1 <- data_ITQ %>%
  select(starts_with("ITQ"))

data_ITQ_w2 <- data_ITQ %>%
  select(starts_with("W2"))

data_ITQ_w3 <- data_ITQ %>%
  select(starts_with("W3"))

data_ITQ_w4 <- data_ITQ %>%
  select(starts_with("W4"))

data_ITQ_w5 <- data_ITQ %>%
  select(starts_with("W5"))

############################################################################

# Separamos los ítems PHQ para cada una de las olas
data_PHQ <- data_items %>%
  select(contains("PHQ"))

data_PHQ_w1 <- data_PHQ %>%
  select(starts_with("PHQ"))

data_PHQ_w2 <- data_PHQ %>%
  select(starts_with("W2"))

data_PHQ_w3 <- data_PHQ %>%
  select(starts_with("W3"))

data_PHQ_w4 <- data_PHQ %>%
  select(starts_with("W4"))

data_PHQ_w5 <- data_PHQ %>%
  select(starts_with("W5"))

############################################################################

# Separamos los ítems GAD para cada una de las olas
data_GAD <- data_items %>%
  select(contains("GAD"))

data_GAD_w1 <- data_GAD %>%
  select(starts_with("GAD"))

data_GAD_w2 <- data_GAD %>%
  select(starts_with("W2"))

data_GAD_w3 <- data_GAD %>%
  select(starts_with("W3"))

data_GAD_w4 <- data_GAD %>%
  select(starts_with("W4"))

data_GAD_w5 <- data_GAD %>%
  select(starts_with("W5"))

############################################################################
############################################################################
############################################################################

#library(ltm)

# Calculamos el alpha para cada test en cada una de las olas
cronbach.alpha(data_ITQ_w1)
cronbach.alpha(data_ITQ_w2[complete.cases(data_ITQ_w2),])
cronbach.alpha(data_ITQ_w3[complete.cases(data_ITQ_w3),])
cronbach.alpha(data_ITQ_w4[complete.cases(data_ITQ_w4),])
cronbach.alpha(data_ITQ_w5[complete.cases(data_ITQ_w5),])

cronbach.alpha(data_PHQ_w1)
cronbach.alpha(data_PHQ_w2[complete.cases(data_PHQ_w2),])
cronbach.alpha(data_PHQ_w3[complete.cases(data_PHQ_w3),])
cronbach.alpha(data_PHQ_w4[complete.cases(data_PHQ_w4),])
cronbach.alpha(data_PHQ_w5[complete.cases(data_PHQ_w5),])

cronbach.alpha(data_GAD_w1)
cronbach.alpha(data_GAD_w2[complete.cases(data_GAD_w2),])
cronbach.alpha(data_GAD_w3[complete.cases(data_GAD_w3),])
cronbach.alpha(data_GAD_w4[complete.cases(data_GAD_w4),])
cronbach.alpha(data_GAD_w5[complete.cases(data_GAD_w5),])







