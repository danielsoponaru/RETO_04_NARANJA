### COMPARACION DE ALGORITMOS ###

#Cargar librerias necesarias
library(recommenderlab)
library(rsparse)
library(dplyr)

#Cargar matriz de datos
data<- readRDS("matriz.RDS")

#Examinar datos
str(data)
dim(data)
class(data)

#Convertir a realRatingMatrix
matriz<- as(data, "realRatingMatrix")

#Train y test
eval_scheme<- evaluationScheme(matriz,
                               method = "split",
                               train = 0.8,
                               given = 10,
                               goodRating = 2) #Lo que se considera bueno y malo (es decir si el rango es de 1-5, el goodrating seria 2.)

#Get Data
getdata<- getData(eval_scheme)


######################            PREDICCIONES            ######################

#Realizar predicciones; Parametrizar algoritmos y hacer  (- Ratings,
#                                                         - topNlist
#                                                         - numericamnete o que le solicite los 5 mejores items a cada usuario)

##########    Predicciones con Ratings:

#POPULAR
modelo_POPULAR_ratings<- Recommender(getData(eval_scheme, "train"), "POPULAR", param = NULL)
predicciones_rating<- predict(modelo_POPULAR_ratings, getData(eval_scheme, "known"), type = "ratings")
prediccion_rating_matriz<- as(predicciones_rating, "matrix")
#Estas son las veces que cada cliente de test compraria cada producto

#RANDOM
modelo_RANDOM_ratings<- Recommender(getData(eval_scheme, "train"), "random", param = NULL)
predicciones_rating2<- predict(modelo_RANDOM_ratings, getData(eval_scheme, "known"), type = "ratings")
prediccion_rating_matriz2<- as(predicciones_rating2, "matrix")

#UBCF
modelo_UBCF_ratings<- Recommender(getData(eval_scheme, "train"), "UBCF", param = NULL)
predicciones_rating3<- predict(modelo_UBCF_ratings,getData(eval_scheme, "known"), type = "ratings")
prediccion_rating_matriz3<- as(predicciones_rating3, "matrix")

#IBCF
modelo_IBCF_ratings<- Recommender(getData(eval_scheme, "train"), "IBCF", param = NULL)
predicciones_rating4<- predict(modelo_IBCF_ratings, getData(eval_scheme, "known"), type = "ratings")
prediccion_rating_matriz4<- as(predicciones_rating4, "matrix")

#SVDF
modelo_SVDF_ratings<- Recommender(getData(eval_scheme, "train"), "SVDF", param = NULL)
predicciones_rating5<- predict(modelo_SVDF_ratings, getData(eval_scheme, "known"), type="ratings")
predicion_rating_matriz5<- as(predicciones_rating5, "matrix")

#ALS
modelo_ALS_ratings<- Recommender(getData(eval_scheme, "train"), "ALS", param = list(lambda = 0.1, n_factors = 10, n_iterations = 10))
predicciones_rating_ALS<- predict(modelo_ALS_ratings, getData(eval_scheme, "known"), type = "ratings")
prediccion_rating_matriz_ALS<- as(predicciones_rating_ALS, "matrix")



##########    Predicciones con TopNLIst:

#POPULAR
modelo_POPULAR_topNlist<- Recommender(getData(eval_scheme, "train"), "POPULAR", param = NULL)
predicciones_topNlist<- predict(modelo_POPULAR_topNlist,getData(eval_scheme, "known"), type="topNList")
prediccion_topNlist_matriz<- as(predicciones_topNlist, "list")

#RANDOM
modelo_RANDOM_topNlist<- Recommender(getData(eval_scheme, "train"), "random", param = NULL)
predicciones_topNlist2<- predict(modelo_RANDOM_topNlist,getData(eval_scheme, "known"), type="topNList")
prediccion_topNlist_matriz2<- as(predicciones_topNlist2, "list")

#UBCF
modelo_UBCF_topNlist<- Recommender(getData(eval_scheme, "train"), "UBCF", param = NULL)
predicciones_topNlist3<- predict(modelo_UBCF_topNlist, getData(eval_scheme, "known"), type="topNList")
prediccion_topNlist_matriz3<- as(predicciones_topNlist3, "list")

#IBCF
modelo_IBCF_topNlist<- Recommender(getData(eval_scheme, "train"), "IBCF", param = NULL)
predicciones_topNlist4<- predict(modelo_IBCF_topNlist, getData(eval_scheme, "known"), type="topNList")
prediccion_topNlist_matriz4<- as(predicciones_topNlist4, "list")

#SVDF
modelo_SVDF_topNlist<- Recommender(getData(eval_scheme, "train"), "SVDF", param = NULL)
predicciones_topNlist5<- predict(modelo_SVDF_topNlist, getData(eval_scheme, "known"), type="topNList")
predicion_topNlist_matriz5<- as(predicciones_topNlist5, "list")

#ALS
modelo_ALS_topNlist<- Recommender(getData(eval_scheme, "train"), "ALS", param = list(lambda = 0.1, n_factors = 10, n_iterations = 10))
predicciones_topNlist_ALS<- predict(modelo_ALS_topNlist, getData(eval_scheme, "known"), type = "topNList")
prediccion_topNlist_matriz_ALS<- as(predicciones_topNlist_ALS, "list")


##############################################################################################################################
#####################    CUAL ES EL MEJOR ALGORITMO? --> TOPNLIST / RATING   ####################3


#############################################################################################
####-----------------------------     RATING     ----------------------------------------####
#############################################################################################

####################-------------   GRAFICAR  (MAE, MSE, RMSE)  ----------------#######################
#-----RATING
# Calcular métricas para cada modelo. RATING
eval_accuracy_total_POPULAR<- calcPredictionAccuracy(predicciones_rating, getData(eval_scheme, "unknown"))
eval_accuracy_total_RANDOM<- calcPredictionAccuracy(predicciones_rating2, getData(eval_scheme, "unknown"))
eval_accuracy_total_UBCF<- calcPredictionAccuracy(predicciones_rating3, getData(eval_scheme, "unknown"))
eval_accuracy_total_IBCF<- calcPredictionAccuracy(predicciones_rating4, getData(eval_scheme, "unknown"))
eval_accuracy_total_SVDF<- calcPredictionAccuracy(predicciones_rating5, getData(eval_scheme, "unknown"))
eval_accuracy_total_ALS<- calcPredictionAccuracy(predicciones_rating_ALS, getData(eval_scheme, "unknown"))

#Crear el data frame con las métricas
comparativa_errores<- data.frame(
  Modelo = c("POPULAR", "RANDOM", "UBCF", "IBCF", "SVDF", "ALS"),
  RMSE = c(eval_accuracy_total_POPULAR["RMSE"],
           eval_accuracy_total_RANDOM["RMSE"],
           eval_accuracy_total_UBCF["RMSE"],
           eval_accuracy_total_IBCF["RMSE"],
           eval_accuracy_total_SVDF["RMSE"],
           eval_accuracy_total_ALS["RMSE"]),
  MAE = c(eval_accuracy_total_POPULAR["MAE"],
          eval_accuracy_total_RANDOM["MAE"],
          eval_accuracy_total_UBCF["MAE"],
          eval_accuracy_total_IBCF["MAE"],
          eval_accuracy_total_SVDF["MAE"],
          eval_accuracy_total_ALS["MAE"]),
  MSE = c(eval_accuracy_total_POPULAR["MSE"],
          eval_accuracy_total_RANDOM["MSE"],
          eval_accuracy_total_UBCF["MSE"],
          eval_accuracy_total_IBCF["MSE"],
          eval_accuracy_total_SVDF["MSE"],
          eval_accuracy_total_ALS["MSE"])
)

#Graficarlo (RMSE,MSE,MAE)
library(ggplot2)
library(tidyr)  

comparativa_larga<- pivot_longer(comparativa_errores,
                                 cols = c("RMSE", "MAE", "MSE"),
                                 names_to = "Metrica",
                                 values_to = "Valor")


ggplot(comparativa_larga, aes(x = Modelo, y = Valor, fill = Metrica)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  labs(title = "Comparación de errores por modelo (Ratings)",
       y = "Valor del error",
       x = "Modelo",
       fill = "Métrica") +
  theme_minimal(base_size = 14) +
  scale_fill_manual(values = c("RMSE" = "#FF5733", "MAE" = "#33C1FF", "MSE" = "#75FF33")) +
  theme(legend.position = "top")

#-------------------        Definiciones:
# METRICA:                                              DEFINICION:                                                                                                        VALOR OPTIMO
#RMSE (Root Mean Squared Error)    Raíz del promedio de los errores al cuadrado entre predicciones y valores reales. Penaliza más errores grandes.    Bajo (cercano a 0). Indica mejor precisión y menor error.
#..............................................................................................................................................................................................................
#MAE (Mean Absolute Error)        Promedio de las diferencias absolutas entre predicciones y valores reales. Mide error promedio.                    Bajo (cercano a 0). Más interpretable, menos sensible a errores grandes que RMSE.
#..............................................................................................................................................................................................................
#MSE (Mean Squared Error)          Promedio de los errores al cuadrado entre predicciones y valores reales.                                             Bajo (cercano a 0). Cuadrado del RMSE, enfatiza errores grandes.
#..............................................................................................................................................................................................................


##############################        CUAL ES EL MEJOR ALGORITMO RATING (FUNCIONES)        ################################

#Mejor algorito rating
mejor_algoritmo_rating<- function(df_metrics) {
  metricas <- c("RMSE", "MAE", "MSE")
  resultados <- list()
  
  for (metrica in metricas) {
    # Encontrar índice del valor mínimo para esa métrica
    idx_mejor <- which.min(df_metrics[[metrica]])
    # Obtener modelo y valor
    modelo_mejor<- df_metrics$Modelo[idx_mejor]
    valor_mejor<- df_metrics[[metrica]][idx_mejor]
    
    resultados[[metrica]]<- list(modelo = modelo_mejor, valor = valor_mejor)
    
    cat(sprintf("Mejor algoritmo según %s: %s (%.4f)\n", metrica, modelo_mejor, valor_mejor))
  }
  
  return(resultados)
}

# Usar la función con tu data frame comparativa_errores
mejor_rating<- mejor_algoritmo_rating(comparativa_errores)
#Se prioriza la metrica de RMSE



#############################################################################################
####-----------------------------     TOPNLIST     ----------------------------------------####
#############################################################################################


####################-------------   GRAFICAR (PRECISION, RECALL, COVERAGE, TPR)  ----------------#######################

#---- TOPNLIST

library(ggplot2)
library(tidyr)
library(dplyr)

# --- EVALUAR ALGORTIMOS TOPNLIST con n = 1,3,5
eval_results<- evaluate(eval_scheme, algos, type = "topNList", n = c(1,3,5))

# Obtener matriz de confusión para n=5 de cada algoritmo
conf_matrix_n5_list<- lapply(eval_results, function(res) getConfusionMatrix(res, n = 5))

# Ver estructura
str(conf_matrix_n5_list)

# Convertir cada matriz en dataframe y añadir nombre del modelo
df_list<- lapply(names(conf_matrix_n5_list), function(modelo){
  df<- as.data.frame(conf_matrix_n5_list[[modelo]])
  df$Modelo<- modelo
  df
})

# Unir todo en un solo dataframe
df_conf<- do.call(rbind, df_list)

# Mostrar primeras filas para revisar
head(df_conf)


df_long<- pivot_longer(df_conf,
                        cols = -Modelo,
                        names_to = "Metrica",
                        values_to = "Valor")

ggplot(df_long, aes(x = Modelo, y = Valor, fill = Metrica)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  labs(title = "Comparación de métricas para topNList (n=5)",
       x = "Modelo",
       y = "Valor Métrica",
       fill = "Métrica") +
  theme_minimal(base_size = 14) +
  scale_fill_manual(values = c("precision" = "#1f77b4",
                               "recall" = "#ff7f0e",
                               "TPR" = "#2ca02c",
                               "FPR" = "#d62728",
                               "coverage" = "#9467bd")) +
  theme(legend.position = "top")

#-------------------        Definiciones:
# METRICA:                                              DEFINICION:                                                                              VALOR OPTIMO
#PRECISION                     Porcentaje de ítems recomendados que son realmente relevantes para el usuario.                  Alto (cercano a 1). Más precisión = menos falsos positivos.
#..............................................................................................................................................................................................................
#RECALL                         Porcentaje de ítems relevantes que fueron correctamente recomendados al usuario.                Alto (cercano a 1). Más recall = menos falsos negativos.
#..............................................................................................................................................................................................................
#TPR (True Positive Rate)       Sensibilidad o tasa de verdaderos positivos: proporción de ítems relevantes que se detectan.    Alto (cercano a 1). Mide la capacidad de identificar correctamente los ítems relevantes.
#..............................................................................................................................................................................................................
#COVERAGE                       Porcentaje del total de ítems disponibles que el sistema es capaz de recomendar.                Alto. Mayor cobertura implica diversidad y menos sesgo en las recomendaciones.
#..............................................................................................................................................................................................................



##############################             CUAL ES EL MEJOR ALGORITMO TOPNLIST (FUNCIONES)                                ################################

#-------¿Cual es el mejor algoritmo? (FUNCION)
#####-----PRECISION
mejor_algoritmo_topN<- function(df_metrics, metrica = "precision") {
  df_met<- df_metrics[df_metrics$Metrica == metrica, ]
  idx_mejor<- which.max(df_met$Valor)
  mejor_modelo<- df_met$Modelo[idx_mejor]
  
  cat("El mejor algoritmo según", metrica, "es:", mejor_modelo, "con un valor de", df_met$Valor[idx_mejor], "\n")
  
  return(mejor_modelo)
}

# Filtramos solo precision
df_precision<- df_long[df_long$Metrica == "precision", ]

# Obtenemos el índice con el mayor valor de precision
idx_mejor<- which.max(df_precision$Valor)

# Extraemos el mejor modelo
mejor_modelo<- df_precision$Modelo[idx_mejor]

cat("El mejor algoritmo según precision es:", mejor_modelo, "con valor:", df_precision$Valor[idx_mejor], "\n")




###---------------------------------------------------------------
##### ----- COVERAGE, PRECISION, RECALL
mejor_algoritmo_topN_global<- function(df_metrics) {
  # Métricas importantes que queremos comparar
  metricas_importantes<- c("precision", "recall", "coverage")
  
  # Filtrar solo esas métricas
  df_filtrado<- df_metrics[df_metrics$Metrica %in% metricas_importantes, ]
  
  # Agrupar por modelo y calcular el promedio de las métricas seleccionadas
  resumen<- df_filtrado %>%
    group_by(Modelo) %>%
    summarise(Score = mean(Valor, na.rm = TRUE)) %>%
    arrange(desc(Score))
  
  # Mostrar ranking completo
  print(resumen)
  
  # Mejor algoritmo: el que tiene el promedio más alto
  mejor<- resumen$Modelo[1]
  
  # Imprimir resultado
  cat("El mejor algoritmo global para topNList es:", mejor, "\n")
  
  # Devolver solo el nombre del mejor algoritmo
  return(mejor)
}

mejor<- mejor_algoritmo_topN_global(df_long)
cat("Mejor algoritmo según todas las métricas:", mejor, "\n")