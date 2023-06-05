setwd("C:/Users/Pedro/Desktop/TFM/R")

library(foreign)
library(tidyverse)
library(qgraph)
library(ggplot2)
library(bootnet)
library(NetworkComparisonTest)


# Cargamos las bases de datos
dat_total <- readRDS("./base_datos/datos_total.rds")
dat_noresilient <- readRDS("./base_datos/datos_matching_noresilient.rds")

# Se seleccionan únicamente los ítems para estimar la red
analysis_vars <- dat_total %>%
  select(contains("ITQ") | contains("PHQ") | contains("GAD")) %>%
  names()


# NETWORK

# Se estiman la red del grupo de resiliencia inicial, que se mantiene constante
network_noresilient <- estimateNetwork(dat_noresilient[analysis_vars],
                                       default = "EBICglasso",
                                       corMethod = "spearman")

# Se genera un vector con las semillas que vamos a utilizar por si fuese necesario replicar el resultado
seed <- runif(n=200, min=1, max = 5000)

# Se generan los vectores vacíos donde almacenaremos los datos
NIT_M <- c()
NIT_p <- c()
GSIT_group <- c()
GSIT_S <- c()
GSIT_p <- c()

# Se genera un loop para realizar el bootstrap. Incluye una tácnica de bootstrap anidado dentro
# de el bootstrap que estamos generando, por lo que el proceso va a ser lento.
for(a in 1:200) {
  print(a) #Se imprime el número de loop para seguir el ritmo del proceso
  set.seed(seed[a])#Se Calcula
  dat_resilient <- dat_total %>% filter(super_resilient) %>%
    slice_sample(n = 284) #Se genera la submuestra de 284 casos sobre el grupo de superresiliencia
  
  network_resilient <- estimateNetwork(dat_resilient[analysis_vars],
                                       default = "EBICglasso",
                                       corMethod = "spearman") #Se estima la red de la submuestra

  nct_results <- NCT(network_resilient, network_noresilient,
                     it = 1000) #Se realiza la comparación

  NIT_M <- c(NIT_M, nct_results$nwinv.real) #Se almacenan los datos en los distintos vectores
  NIT_p <- c(NIT_p, nct_results$nwinv.pval)
  
  GSIT_group <- c(GSIT_group, paste(nct_results$glstrinv.sep[1],
                                    "-", nct_results$glstrinv.sep[2]))
  GSIT_S <- c(GSIT_S, nct_results$glstrinv.real)
  GSIT_p <- c(GSIT_p, nct_results$glstrinv.pval)
  print(a)
}

# Unimos los  vectores en una tabla que nos permita su fácil acceso.
subset_data <- data.frame(seed, NIT_M, NIT_p, GSIT_group, GSIT_S, GSIT_p)

# Guardamos la base de datos
saveRDS(subset_data, file = "base_datos/subset_data05.rds")

# Exploramos qué submuestras obtienen diferencias significativas en la comparación
subset_data %>% filter(NIT_p <= 0.05)
subset_data %>% filter(GSIT_p <= 0.05)
subset_data %>% filter(NIT_p <= 0.05 & GSIT_p <= 0.05)

##############################################################################
##############################################################################

# Si queremos acceder a los datos descriptivos de cada submuestra

# Seleccionamos las variables descriptivas
socdem_vars <- c("Sexo", "Edad", "Edad_grupo", "Zona_vivienda", "Titulación", "Ingresos")

# Establecemos un vector con los datos de la muestra de resiliencia inicial para facilitar la comparación
numer_noresilient <- as.numeric(str_split(summary_noresilient, ':',
                                          simplify = TRUE)[,2])
numer_noresilient <- numer_noresilient[!is.na(numer_noresilient)]
tabla_summary <- data.frame(numer_noresilient)
colnames(tabla_summary) <- "noresilient"

# Extraemos las semillas que se han utilizado en la extracción de submuestras
seed <- subset_data %>% filter(NIT_p <= 0.05 & GSIT_p <= 0.05) %>%
  pull(seed)

# Con un loop, vamos extrayendo las características muestrales de cada una de las submuestras.
for(a in 1:length(seed)) {
  set.seed(seed[a])
  dat_resilient <- dat_total %>% filter(super_resilient) %>%
    slice_sample(n = 284)
  summary_resilient <- summary(dat_resilient[socdem_vars]) #Utilizamos la función summary para extraer el resumen muestral
  
  numer_resilient <- as.numeric(str_split(summary_resilient, ':',
                                          simplify = TRUE)[,2]) #Nos quedamos únicamente con el valor del resumen
  numer_resilient <- numer_resilient[!is.na(numer_resilient)] #Eliminamos los casos perdidos
  
  tabla_summary <- cbind(tabla_summary, numer_resilient) #Conformamos la tabla de datos
  colnames(tabla_summary)[a+1] <- round(seed[a],4)
  
}
tabla_summary

# Para facilitar su entendimiento, le otorgamos nombres de las variables  que nos resulten cómodos.
nombres_fila <- c("sexo_Masculino", "sexo_Femenino", "sexo_Transgénero",
                  "sexo_Prefieronodecir", "sexo_Otro", "edad_Min.",
                  "edad_1stQu.", "edad_Median", "edad_Mean", "edad_3rdQu.",
                  "edad_Max.", "edad_grupo_18-24", "edad_grupo_25-34",
                  "edad_grupo_35-44", "edad_grupo_45-54", "edad_grupo_55-64",
                  "edad_grupo_65+", "zona_vivienda_Urbana",
                  "zona_vivienda_Rural", "titulacion_Bachillerato",
                  "titulacion_Licenciatura", "titulacion_Gradouniversitario",
                  "titulacion_Formaciónprofesional",
                  "titulacion_Educaciónsecundaria", "titulacion_Master",
                  "titulacion_(Other)", "ingresos_De12.450a20.200alaño",
                  "ingresos_de20.200a35.200alaño",
                  "ingresos_De35.200a60.000alaño", "ingresos_Másde60.000alaño")



