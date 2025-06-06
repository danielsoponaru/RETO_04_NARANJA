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
data<- readRDS("Datos/Transformados/MatrizSuperReducida.RDS")
objetivos<- readRDS("Datos/Originales/objetivos.RDS")
clientes_objetivo<- objetivos$objetivo2$obj
productos<- readRDS("Datos/Originales/maestroestr.RDS")


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

matriz_sparseB<- ifelse(matriz_sparse > 0, 1, 0)
matriz_sparse@x[matriz_sparse@x>=1]<- 1
matriz_sparse

#4. Entrenar modelo ALS
set.seed(7)
modelo_ALS<- WRMF$new(rank = 10L, lambda = 0.1, feedback = "implicit") 
modelo_ALS$fit_transform(matriz_sparse)

#5. Obtener el índice (en que fila estan) de los clientes objetivo en la matriz
id_usuarios<- match(clientes_objetivo, rownames(matriz))

#6. Hacer predicciones de productos
predicciones<- modelo_ALS$predict(matriz_sparse[id_usuarios, , drop = FALSE], k = 1)
productos_recomendados<- colnames(matriz)[predicciones]


#7. Añadir el score de recomendación para justificar el resultado

#Obtener el score para cada producto recomendado para cada cliente
scores_recomendados<- sapply(1:length(id_usuarios), function(i) {
  usuario <- id_usuarios[i]
  producto <- predicciones[i]
  
  #La función predict permite pasar un solo usuario y producto para obtener el score
  score<- modelo_ALS$predict(matriz_sparse[usuario, , drop = FALSE], k = 1, items = producto)
  return(score)
})

#Construir data.frame con cliente, producto y score
resultado<- data.frame(
  cliente = clientes_objetivo,
  producto_recomendado = sub("^X", "", productos_recomendados),
  score = round(as.numeric(scores_recomendados), 4)
)

#Añadir la descripción del producto
resultado_final<- merge(resultado, productos,
                         by.x = "producto_recomendado",
                         by.y = "cod_est",
                         all.x = TRUE)

resultado_final<- resultado_final[, c("cliente", "producto_recomendado", "descripcion", "score")]

print(resultado_final)

#Guardar los resultados
write.csv(resultado_final, "Resultados/resultados_objetivo2.csv", row.names = FALSE)

#Guardar cosas extra para la api
saveRDS(modelo_ALS, "modelo_als.rds")
saveRDS(matriz_sparse, "matriz_sparse.rds")
saveRDS(clientes_objetivo, "clientes_objetivo.rds")
saveRDS(productos, "productos.rds")
