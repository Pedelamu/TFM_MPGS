Este repositorio se ha creado con el fin de anexar el código utilizado en el trabajo de fin de máster al que hace referencia el archivo TFM.

A continuación, se describe cada uno de los escripts:

- alfa_conbach: Permite el cálculo de la fiabilidad interna de cada instrumento en cada una de las olas
- clustering: Permite realizar un análisis de conglomerados, así como representarlo gráficamente.
- combinar_datos_final: Genera la base de datos utilizada para los análisis, uniendo las variables sociodemográficas y los datos de los distintos instrumentos.
- datos_matching: Permite realizar el emparejamiento entre ambos grupos permitiendo el control de variables de confusión.
- estimar_redes: Permite estimar las redes para cada uno de los grupos, así como realizar las comparaciones y contrastes pertinentes y un análisis de centralidad.
- extract_datos_ans: Extrae los datos correspondientes al GAD-7.
- extract_datos_dep: Extrae los datos correspondientes al PHQ-9.
- extrac_datos_trauma: Extrae los datos correspondientes al ITQ y define los grupos en base al diagnóstico de TEPT atendiendo a los criterios del ITQ. Seguidamente se filtra la muestra incluyendo únicamente los casos que no obtienen un diagnóstico en la primera oleada y entran en las condiciones del grupo de superresiliencia o del grupo de resiliencia inicial.
- imprimir_redes: Permite elaborar los gráficos de las redes estimadas.
- subset_sample_redes: Permite realizar un bootstraping sobre la muestra total del grupo de superresiliencia para extraer submuestras de 284 casos que comparar con el grupo de resiliencia inicial. También permite explorar las características sociodemográficas de cada una de estas submuestras.
