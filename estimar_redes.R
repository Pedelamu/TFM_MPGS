setwd("C:/Users/Pedro/Desktop/TFM/R")

library(foreign)
library(tidyverse)
library(qgraph)
library(ggplot2)
library(bootnet)
library(NetworkComparisonTest)



# Se cargan los datos
dat <- readRDS("./base_datos/datos_matching.rds")
dat_resilient <- readRDS("./base_datos/datos_matching_resilient.rds")
dat_noresilient <- readRDS("./base_datos/datos_matching_noresilient.rds")

# Se seleccionan únicamente los ítems para estimar la red
analysis_vars <- dat %>%
  select(contains("ITQ") | contains("PHQ") | contains("GAD")) %>%
  names()


# NETWORK

# Se estima cada una de las redes
network_resilient <- estimateNetwork(dat_resilient[analysis_vars],
                                     default = "EBICglasso",
                                     corMethod = "spearman")
network_resilient

network_noresilient <- estimateNetwork(dat_noresilient[analysis_vars],
                                     default = "EBICglasso",
                                     corMethod = "spearman")
network_noresilient

#####################################################################

# NETWORK COMPARISON TEST

M <-
  max(
    abs(c(network_resilient$graph) - c(network_noresilient$graph))
  )

cat("The biggest edge difference is:", M)

S <-
  abs(
    sum(
      abs(c(network_resilient$graph)) -
        abs(c(network_noresilient$graph))
    )
  )/2

cat("Strength difference between the two networks is:", S)

# Se realiza la comparación entre redes
nct_results <- NCT(network_resilient, network_noresilient,
                   it = 1000)

nct_results


# Si M es significativo:
#nct_test_edges <- NCT(network_resilient, network_noresilient, 
#                      it = 1000, test.edges = T,
#                      p.adjust.methods = "BH")

#difference_value(nct_test_edges)

# NCT CENTRAILTY

# Se explora si existen diferencias significativas en los índices de centralidad
nct_test_centrality <- NCT(network_resilient, network_noresilient,
                           it = 1000, test.centrality = T,
                           p.adjust.methods = "BH",
                           centrality = c("closeness",
                                          "betweenness",
                                          "strength",
                                          "expectedInfluence"))
nct_test_centrality$diffcen.pval

# Se generan los gráficos de los índices de centralidad
lables_items <- c("ITQ 1", "ITQ 2", "ITQ 3", "ITQ 4", "ITQ 5", "ITQ 6",
                  "ITQ Int 1", "ITQ Int 2", "ITQ Int 3", "PHQ 1",
                  "PHQ 2", "PHQ 3", "PHQ 4", "PHQ 5", "PHQ 6", "PHQ 7",
                  "PHQ 8", "PHQ 9", "GAD 1", "GAD 2", "GAD 3", "GAD 4",
                  "GAD 5", "GAD 6", "GAD 7")

centralityPlot(network_resilient, include = "all",
               labels = lables_items, scale = "relative", decreasing = TRUE)
               

centralityPlot(network_noresilient, include = "all", 
               labels = lables_items,  scale = "relative", decreasing = TRUE)
               

