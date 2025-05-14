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
data<- readRDS("Datos//Transformados//matriz.RDS")
objetivos<- readRDS("Datos//Originales//objetivos.RDS")
clientes_objetivo<- objetivos$objetivo2$obj
productos<- readRDS("Datos//Originales//maestroestr.RDS")

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
matriz_sparse<- as(as.matrix(matriz), "dgCMatrix")

#4. Entrenar modelo ALS
modelo_ALS<- WRMF$new(rank = 30, lambda = 0.1, feedback = "implicit") #poner 10L
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
