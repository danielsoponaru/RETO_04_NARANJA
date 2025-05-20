### OBJETIVO 2, RETO 4 ### 

#Instalar las librerías necesarias

if(!requireNamespace("recommenderlab", quietly = TRUE)) {
  install.packages("recommenderlab")
}

if(!requireNamespace("rsparse", quietly = TRUE)) {
  install.packages("rsparse")
}

if(!requireNamespace("Matrix", quietly = TRUE)) {
  install.packages("Matrix")
}

if(!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}

#Cargar librerias necesarias
library(recommenderlab)
library(rsparse)
library(Matrix)
library(dplyr)

#1. Cargar datos
data<- readRDS("matriz.RDS")
objetivos<- readRDS("objetivos.RDS")
clientes_objetivo<- objetivos$objetivo2$obj
productos<- readRDS("maestroestr.RDS")

sum(data == 0)/(nrow(data) * ncol(data))

#2. Examinar datos y tipos de columnas
str(data)
summary(data)
class(data)
dim(data)

str(clientes_objetivo)
summary(clientes_objetivo)

#3. Preparar la matriz

#Reemplazar NA con 0 (porque sparseMatrix no admite NA)
matriz<- replace(data, is.na(data), 0)

#Convierte a matriz sparseMatrix
matriz_sparse<- as(as.matrix(matriz), "sparseMatrix")

#4. Entrenar modelo ALS
set.seed(7)
modelo_ALS<- WRMF$new(rank = 10L, lambda = 0.1, feedback = "implicit") 
modelo_ALS$fit_transform(matriz_sparse)

#5. Obtener el índice (en que fila estan) de los clientes objetivo en la matriz
id_usuarios<- match(clientes_objetivo, rownames(matriz))

#6. Hacer predicciones de productos
predicciones<- modelo_ALS$predict(matriz_sparse[id_usuarios, , drop = FALSE], k = 1)
productos_recomendados<- colnames(matriz)[predicciones]

#Resultado
resultado<- data.frame(cliente = clientes_objetivo,
                       producto_recomendado = productos_recomendados)

#Asignar el nombre del producto
resultado$producto_recomendado<- sub("^X", "", resultado$producto_recomendado)
resultado_final<- merge(resultado,
                         productos,
                         by.x = "producto_recomendado",
                         by.y = "cod_est",
                         all.x = TRUE)
resultado_final<- resultado_final[, c("cliente", "producto_recomendado", "descripcion")]
resultado_final

















#############

scores_matrix <- modelo_ALS$predict(matriz_sparse[id_usuarios, , drop = FALSE], type = "score")

resultados_con_score <- data.frame(cliente = clientes_objetivo,
                                   producto_recomendado = character(length(clientes_objetivo)),
                                   score = numeric(length(clientes_objetivo)),
                                   stringsAsFactors = FALSE)

for (i in seq_along(id_usuarios)) {
  user_idx <- id_usuarios[i]
  user_scores <- scores_matrix[i, ]
  user_compras <- matriz_sparse[user_idx, ]
  user_scores[user_compras > 0]<- -Inf
  best_product_idx <- which.max(user_scores)
  resultados_con_score$producto_recomendado[i] <- colnames(matriz_sparse)[best_product_idx]
  resultados_con_score$score[i] <- user_scores[best_product_idx]
}

resultados_con_score$producto_recomendado <- sub("^X", "", resultados_con_score$producto_recomendado)

resultado_final <- merge(resultados_con_score,
                         productos,
                         by.x = "producto_recomendado",
                         by.y = "cod_est",
                         all.x = TRUE)

resultado_final <- resultado_final[, c("cliente", "producto_recomendado", "descripcion", "score")]
str(scores_matrix)
