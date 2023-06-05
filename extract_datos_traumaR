setwd("C:/Users/Pedro/Desktop/TFM/R")

library(foreign)
library(tidyverse)

####
#### EXTRACCIÓN DE LOS DATOS DE TRAUMA
####

# Cargamos la base de datos original
data <- read.spss("./base_datos/datos_originales.sav", to.data.frame=TRUE)

# Formamos la nueva variable categórica de edad y extraemos aquellas variables
# sociodemográficas de interés
datos_socdem <- data %>%
  mutate(Edad_grupo = as.factor(case_when(Edad < 25 ~ '18-24',
                                          Edad >= 25  & Edad < 35 ~ '25-34',
                                          Edad >= 35  & Edad < 45 ~ '35-44',
                                          Edad >= 45  & Edad < 55 ~ '45-54',
                                          Edad >= 55  & Edad < 65 ~ '55-64',
                                          Edad >= 65 ~ '65+',))) %>%
  select(ID, Sexo, Edad,  Edad_grupo, Zona_vivienda, Titulación, Ingresos)

# Extraemos los ítems de interés
items_itq <- data %>%
  select(c(ID, contains("ITQ"))) %>%
  select(c(ID, ends_with("REC")))

# Generamos las variables necesarias para definir el diagnóstico de TEPT
datos_itq <- items_itq %>%
  mutate(W1_reexp = ITQ_1_REC >=2 | ITQ_2_REC >= 2) %>% #Nivel patológico de reexperimentación
  mutate(W1_evit = ITQ_3_REC >=2 | ITQ_4_REC >= 2) %>% #Nivel patológico de evitación
  mutate(W1_amenaz = ITQ_5_REC >=2 | ITQ_6_REC >= 2) %>% #Nivel patológico de amenaza
  mutate(W2_reexp = W2_ITQ_1_REC >=2 | W2_ITQ_2_REC >= 2) %>%
  mutate(W2_evit = W2_ITQ_3_REC >=2 | W2_ITQ_4_REC >= 2) %>%
  mutate(W2_amenaz = W2_ITQ_5_REC >=2 | W2_ITQ_6_REC >= 2) %>%
  mutate(W3_reexp = W3_ITQ_1_REC >=2 | W3_ITQ_2_REC >= 2) %>%
  mutate(W3_evit = W3_ITQ_3_REC >=2 | W3_ITQ_4_REC >= 2) %>%
  mutate(W3_amenaz = W3_ITQ_5_REC >=2 | W3_ITQ_6_REC >= 2) %>%
  mutate(W4_reexp = W4_ITQ_1_REC >=2 | W4_ITQ_2_REC >= 2) %>%
  mutate(W4_evit = W4_ITQ_3_REC >=2 | W4_ITQ_4_REC >= 2) %>%
  mutate(W4_amenaz = W4_ITQ_5_REC >=2 | W4_ITQ_6_REC >= 2) %>%
  mutate(W5_reexp = W5_ITQ_1_REC >=2 | W5_ITQ_2_REC >= 2) %>%
  mutate(W5_evit = W5_ITQ_3_REC >=2 | W5_ITQ_4_REC >= 2) %>%
  mutate(W5_amenaz = W5_ITQ_5_REC >=2 | W5_ITQ_6_REC >= 2) %>%
  mutate(W1_interf = ITQ_12_REC >=2 | ITQ_13_REC >=2 | ITQ_14_REC >=2) %>% #Nivel patológico de amenaza
  mutate(W2_interf = W2_ITQ_12_REC >=2 | W2_ITQ_13_REC >=2 |
           W2_ITQ_14_REC >=2) %>%
  mutate(W3_interf = W3_ITQ_12_REC >=2 | W3_ITQ_13_REC >=2 |
           W3_ITQ_14_REC >=2) %>%
  mutate(W4_interf = W4_ITQ_LIFE_INTERF_REC >=2 | W4_ITQ_JOB_INTERF_REC >=2 |
           W4_ITQ_ACTIV_INTERF_REC >=2) %>%
  mutate(W5_interf = W5_ITQ_12_REC >=2 | W5_ITQ_13_REC >=2 |
           W5_ITQ_14_REC >=2) %>%
  mutate(W1_trauma_diag = W1_reexp & W1_evit & W1_amenaz & W1_interf) %>%
  mutate(W2_trauma_diag = W2_reexp & W2_evit & W2_amenaz & W2_interf) %>%
  mutate(W3_trauma_diag = W3_reexp & W3_evit & W3_amenaz & W3_interf) %>%
  mutate(W4_trauma_diag = W4_reexp & W4_evit & W4_amenaz & W4_interf) %>%
  mutate(W5_trauma_diag = W5_reexp & W5_evit & W5_amenaz & W5_interf)

# Seleccionamos las variables de interés y filtramos nuestra muestra
datos_trauma_diag <- datos_itq %>%
  select(ID, ITQ_1_REC, ITQ_2_REC, ITQ_3_REC, ITQ_4_REC, ITQ_5_REC, ITQ_6_REC,
         ITQ_12_REC, ITQ_13_REC, ITQ_14_REC, W1_reexp, W1_evit, W1_amenaz,
         W1_interf, W1_trauma_diag, W2_trauma_diag, W3_trauma_diag,
         W4_trauma_diag, W5_trauma_diag) %>%
  mutate(n_na = rowSums(is.na(.))) %>% #Realizamos un conteo de valores perdidos
  mutate(diag_waves = rowSums(across(ends_with("trauma_diag")), 
                              na.rm = TRUE)) %>% #Realizamos un conteo del número de diagnósticos
  mutate(any_trauma_diag = diag_waves > 0) %>% #Definimos la variable diagnóstico a través del tiempo
  mutate(super_resilient = diag_waves == 0) %>% #Definimos la variable superresiliencia
  filter(super_resilient | (!W1_trauma_diag & any_trauma_diag)) #Filtramos los datos de forma que únicamente se incluyan los participantes superresilientes y los participantes que desarrollaron el diagnóstico a través del tiempo SI estos NO tuvieron un diagnóstico en la primera ola

# Unimos los datos sociodemográficos y los de TEPT mediante el ID y aseguramos que no se den casos perdidos.
datos_trauma_final <- merge(datos_socdem, datos_trauma_diag, by = "ID") %>%
  filter(!is.na(W1_trauma_diag), 
         any_trauma_diag | (!any_trauma_diag & n_na == 0))

# Exploramos los datos
datos_trauma_final %>%
  mutate(n_na = as.factor(n_na)) %>%
  mutate(diag_waves = as.factor(diag_waves)) %>%
  summary()

# Guardamos los datos
saveRDS(datos_trauma_final, file = "base_datos/datos_trauma.rds")


