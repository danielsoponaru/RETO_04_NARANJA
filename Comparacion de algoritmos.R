### COMPARACION DE ALGORITMOS, RETO 04 ###

#Instalar librerias si es necesario
if(!requireNamespace("recommenderlab", quietly = TRUE)) {
  install.packages("recommenderlab")
}

if(!requireNamespace("rsparse", quietly = TRUE)) {
  install.packages("rsparse")
}

if(!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}

if(!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}

if(!requireNamespace("tidyr", quietly = TRUE)) {
  install.packages("tidyr")
}


#Cargar librerias necesarias
library(recommenderlab)
library(rsparse)
library(dplyr)
library(ggplot2)
library(tidyr)

#Cargar matriz de datos
data<- readRDS("MatrizSuperReducida.RDS")

#Examinar datos
str(data)
dim(data)
class(data)

#Convertir a realRatingMatrix
rownames(data)<- NULL
colnames(data)<- NULL
matriz<- as.matrix(data)
matriz<- as(matriz, "realRatingMatrix")

#Train y test
eval_scheme<- evaluationScheme(matriz,
                               method = "split",
                               train = 0.8,
                               given = 10,
                               goodRating = 2) #Lo que se considera bueno y malo (es decir si el rango es de 1-5, el goodrating seria 2.)

#Get Data
getdata<- getData(eval_scheme)


######################            PREDICCIONES            ######################


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
predicciones_rating5<- predict(modelo_SVDF_ratings, getData(eval_scheme, "known"), type = "ratings")
predicion_rating_matriz5<- as(predicciones_rating5, "matrix")


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



#####################    CUAL ES EL MEJOR ALGORITMO? --> TOPNLIST / RATING   ####################3



####-----------------------------     METRICAS DE RATING     -----------------------------####

#Calcular métricas para cada modelo
eval_accuracy_total_POPULAR<- calcPredictionAccuracy(predicciones_rating, getData(eval_scheme, "unknown"))
eval_accuracy_total_RANDOM<- calcPredictionAccuracy(predicciones_rating2, getData(eval_scheme, "unknown"))
eval_accuracy_total_UBCF<- calcPredictionAccuracy(predicciones_rating3, getData(eval_scheme, "unknown"))
eval_accuracy_total_IBCF<- calcPredictionAccuracy(predicciones_rating4, getData(eval_scheme, "unknown"))
eval_accuracy_total_SVDF<- calcPredictionAccuracy(predicciones_rating5, getData(eval_scheme, "unknown"))

#Crear el data frame con las métricas
comparativa_errores<- data.frame(
  Modelo = c("POPULAR", "RANDOM", "UBCF", "IBCF", "SVDF"),
  RMSE = c(eval_accuracy_total_POPULAR["RMSE"],
           eval_accuracy_total_RANDOM["RMSE"],
           eval_accuracy_total_UBCF["RMSE"],
           eval_accuracy_total_IBCF["RMSE"],
           eval_accuracy_total_SVDF["RMSE"]),
  MAE = c(eval_accuracy_total_POPULAR["MAE"],
          eval_accuracy_total_RANDOM["MAE"],
          eval_accuracy_total_UBCF["MAE"],
          eval_accuracy_total_IBCF["MAE"],
          eval_accuracy_total_SVDF["MAE"]),
  MSE = c(eval_accuracy_total_POPULAR["MSE"],
          eval_accuracy_total_RANDOM["MSE"],
          eval_accuracy_total_UBCF["MSE"],
          eval_accuracy_total_IBCF["MSE"],
          eval_accuracy_total_SVDF["MSE"])
)

#Graficarlo (RMSE,MSE,MAE)
comparativa_larga<- pivot_longer(comparativa_errores,
                                 cols = c("RMSE", "MAE", "MSE"),
                                 names_to = "Metrica",
                                 values_to = "Valor")

graficoRatings<- ggplot(comparativa_larga, aes(x = Modelo, y = Valor, fill = Metrica)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  scale_y_log10() +  # Escala logarítmica
  labs(title = "Comparación de errores por modelo (Ratings)",
       y = "Valor del error (escala logarítmica)",
       x = "Modelo",
       fill = "Métrica") +
  theme_minimal(base_size = 14) +
  scale_fill_manual(values = c("RMSE" = "#E60026", "MAE" = "#0033A0", "MSE" = "#80C342")) +
  theme(legend.position = "top")




####---------------------------     METRICAS DE TOPNLIST     ---------------------------####

#Número de recomendaciones a evaluar
topN<- 10

eval_POPULAR_topN<- evaluate(eval_scheme, method = "POPULAR", type = "topNList", n = topN)
eval_RANDOM_topN<- evaluate(eval_scheme, method = "RANDOM", type = "topNList", n = topN)
eval_UBCF_topN<- evaluate(eval_scheme, method = "UBCF", type = "topNList", n = topN)
eval_IBCF_topN<- evaluate(eval_scheme, method = "IBCF", type = "topNList", n = topN)
eval_SVDF_topN<- evaluate(eval_scheme, method = "SVDF", type = "topNList", n = topN)

#Extraer resumen de métricas (precision, recall...)
res_POPULAR<- avg(eval_POPULAR_topN)
res_RANDOM<- avg(eval_RANDOM_topN)
res_UBCF<- avg(eval_UBCF_topN)
res_IBCF<- avg(eval_IBCF_topN)
res_SVDF<- avg(eval_SVDF_topN)

#Crear df con las metricas
comparativa_topN <- data.frame(
  
  Modelo = c("POPULAR", "RANDOM", "UBCF", "IBCF", "SVDF"),
  
  Precision = c(res_POPULAR[,"precision"],
                res_RANDOM[,"precision"],
                res_UBCF[,"precision"],
                res_IBCF[,"precision"],
                res_SVDF[,"precision"]),
  
  Recall = c(res_POPULAR[,"recall"],
             res_RANDOM[,"recall"],
             res_UBCF[,"recall"],
             res_IBCF[,"recall"],
             res_SVDF[,"recall"]),
  
  TPR = c(res_POPULAR[,"TPR"],
          res_RANDOM[,"TPR"],
          res_UBCF[,"TPR"],
          res_IBCF[,"TPR"],
          res_SVDF[,"TPR"]),
  
  FPR = c(res_POPULAR[,"FPR"],
          res_RANDOM[,"FPR"],
          res_UBCF[,"FPR"],
          res_IBCF[,"FPR"],
          res_SVDF[,"FPR"])
)

#Visualizar las metricas

#Convertir el data frame a formato largo con todas las métricas
comparativa_topN_larga<- pivot_longer(comparativa_topN,
                                      cols = c("Precision", "Recall", "TPR", "FPR"),
                                      names_to = "Metrica",
                                      values_to = "Valor")

#Crear el gráfico de barras comparativo
grafico_topN<- ggplot(comparativa_topN_larga, aes(x = Modelo, y = Valor, fill = Metrica)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  labs(title = paste("Evaluación de modelos por métricas (Top", topN, ")"),
       y = "Valor de la métrica",
       x = "Modelo",
       fill = "Métrica") +
  theme_minimal(base_size = 14) +
  scale_fill_manual(values = c("Precision" = "#E60026",
                               "Recall" = "#0033A0",
                               "TPR" = "#80C342",
                               "FPR" = "#FFB900")) +
  theme(legend.position = "top")


########## GUARDAR RESULTADOS Y GRAFICOS ########## 
write.csv(comparativa_errores, file = "Comparacion de algoritmos/metricasRatings.csv", row.names = FALSE)
write.csv(comparativa_topN, file = "Comparacion de algoritmos/metricasTopN.csv", row.names = FALSE)

ggsave("Comparacion de algoritmos/GraficoMetricasRatings.png", plot = graficoRatings, width = 8, height = 6, dpi = 300, bg = "white")
ggsave("Comparacion de algoritmos/GraficoMetricasTopN.png", plot = grafico_topN, width = 8, height = 6, dpi = 300, bg = "white")
