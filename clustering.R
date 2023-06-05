setwd("C:/Users/Pedro/Desktop/TFM/R")

library(foreign)
library(tidyverse)
library(qgraph)
library(ggplot2)
library(bootnet)
library(NetworkComparisonTest)
library(igraph)

# Se cargan los datos
dat <- readRDS("./base_datos/datos_matching.rds")
dat_resilient <- readRDS("./base_datos/datos_matching_resilient.rds")
dat_noresilient <- readRDS("./base_datos/datos_matching_noresilient.rds")

# Se seleccionan únicamente los ítems para estimar la red
analysis_vars <- dat %>%
  select(contains("ITQ") | contains("PHQ") | contains("GAD")) %>%
  names()
dat_resilient <- dat_resilient[analysis_vars]
dat_noresilient <- dat_noresilient[analysis_vars]

# Se cambia la denominación de los ítems de acuerdo con el resultado gráfico deseado
names(dat_resilient) <- c("ITQ 1", "ITQ 2", "ITQ 3", "ITQ 4", "ITQ 5", "ITQ 6",
                          "ITQ Int 1", "ITQ Int 2", "ITQ Int 3", "PHQ 1",
                          "PHQ 2", "PHQ 3", "PHQ 4", "PHQ 5", "PHQ 6", "PHQ 7",
                          "PHQ 8", "PHQ 9", "GAD 1", "GAD 2", "GAD 3", "GAD 4",
                          "GAD 5", "GAD 6", "GAD 7")
names(dat_noresilient) <- c("ITQ 1", "ITQ 2", "ITQ 3", "ITQ 4", "ITQ 5", "ITQ 6",
                            "ITQ Int 1", "ITQ Int 2", "ITQ Int 3", "PHQ 1",
                            "PHQ 2", "PHQ 3", "PHQ 4", "PHQ 5", "PHQ 6", "PHQ 7",
                            "PHQ 8", "PHQ 9", "GAD 1", "GAD 2", "GAD 3", "GAD 4",
                            "GAD 5", "GAD 6", "GAD 7")

# Se establece una breve leyenda para cada ítem
items <- c("Pesadillas",
           "Recuerdos intensos",
           "Evitación interna",
           "Evitación externa",
           "Estado de alerta",
           "Sobresalto fácil",
           "Interferencia en relaciones",
           "Interferencia en trabajo",
           "Interferencia en otras áreas",
           "Apatía",
           "Tristeza profunda",
           "Problemas de sueño",
           "Fatiga persistente",
           "Cambios en el apetito",
           "Autoestima negativa",
           "Problemas de concentración",
           "Enlentecimiento o agitación",
           "Ideación suicida",
           "Nerviosismo intenso",
           "Preocupación constante",
           "Preocupación excesiva",
           "Dificultad para relajarse",
           "Intranquilidad constante",
           "Irritabilidad y enfado",
           "Miedo intenso")

# NETWORK

# Se estiman las redes
network_resilient <- estimateNetwork(dat_resilient,
                                     default = "EBICglasso",
                                     corMethod = "spearman")


network_noresilient <- estimateNetwork(dat_noresilient,
                                       default = "EBICglasso",
                                       corMethod = "spearman")

# Se establece un máximo absoluto para permitir la comparación gráfica
max_value <- max(
  max(abs(network_resilient$graph)),
  max(abs(network_noresilient$graph))
)
max_value

# Se establece una disposición conjunta
net_layout <- averageLayout(network_resilient,
                            network_noresilient,
                            layout = "spring")

# Mediante este comando permitimos la representación de dos gráficos en la misma imagen
op <- par(mfrow=c(1,2))

#################################################################################
#################################################################################
#################################################################################

# Elaboramos los gráficos originales
plot_resilient <- plot(network_resilient,
               layout = net_layout,
               maximum = max_value,
               labels = colnames(dat_resilient),
               label.cex = 1,
               label.color = 'black',
               label.prop = 0.9,
               negDashed = T,
               groups = list("ITQ" = 1:9,
                             "PHQ-9" = 10:18,
                             "GAD-7" = 19:25),
               color = c("lightblue",
                         "lightsalmon",
                         "lightgreen"),
               font = 2,
               legend = FALSE)

plot_noresilient <- plot(network_noresilient,
                         layout = net_layout,
                         maximum = max_value,
                         labels = colnames(dat_noresilient),
                         label.cex = 1,
                         label.color = 'black',
                         label.prop = 0.9,
                         negDashed = T,
                         groups = list("ITQ" = 1:9,
                                       "PHQ-9" = 10:18,
                                       "GAD-7" = 19:25),
                         color = c("lightblue",
                                   "lightsalmon",
                                   "lightgreen"),
                         ont = 2,
                         legend = FALSE)




# Estimamos los conglomerados
network_resilient_IGRAPH = as.igraph(plot_resilient, attributes=TRUE)
sgc_resilient <- spinglass.community(network_resilient_IGRAPH, spins = 3)
sgc_resilient$membership

# En base al resultado anterior, definimos cada una de las comunidades
spinglass_resilient <- list(c(1:6), c(7:9, 19:25), c(10:18))


# Estimamos los conglomerados
network_noresilient_IGRAPH = as.igraph(plot_noresilient, attributes=TRUE)
sgc_noresilient <- spinglass.community(network_noresilient_IGRAPH, spins = 3)
sgc_noresilient$membership

# En base al resultado anterior, definimos cada una de las comunidades
spinglass_noresilient <- list(c(1:9), c(10, 11, 13:18), c(12, 19:25))


# Representamos de nuevo los gráficos utilizando como grupos las comunidades generadas
plot_resilient <- plot(network_resilient,
                       title = "Muestra resiliente emparejada",
                       layout = net_layout,
                       maximum = max_value,
                       labels = colnames(dat_resilient),
                       label.cex = 1.2,
                       label.color = 'black',
                       label.prop = 0.75,
                       negDashed = T,
                       groups = spinglass_resilient,
                       color = c("lightblue",
                                 "lightsalmon",
                                 "lightgreen",
                                 "violet",
                                 "yellow"),
                       font = 2,
                       legend = FALSE)

# Representamos de nuevo los gráficos utilizando como grupos las comunidades generadas
plot_noresilient <- plot(network_noresilient,
                         title = "Muestra no resiliente",
                         layout = net_layout,
                         maximum = max_value,
                         labels = colnames(dat_noresilient),
                         label.cex = 1,
                         label.color = 'black',
                         label.prop = 0.9,
                         negDashed = T,
                         groups = spinglass_noresilient,
                         color = c("lightblue",
                                   "lightgreen",
                                   "lightsalmon",
                                   "yellow",
                                   "violet"),
                         font = 2,
                         legend = FALSE)


###############################################################################
###############################################################################


# Calculamos los índices Q para el análisis de modularidad
modularity(network_resilient_IGRAPH, membership = sgc_resilient$membership)

modularity(network_noresilient_IGRAPH, membership = sgc_noresilient$membership)





