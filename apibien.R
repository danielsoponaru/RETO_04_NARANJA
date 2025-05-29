# plumber.R

library(plumber)
library(Matrix)
library(rsparse)
library(dplyr)

# Cargar modelo y datos
modelo_ALS <- readRDS("modelo_als.rds")
matriz_sparse <- readRDS("matriz_sparse.rds")
clientes_objetivo <- readRDS("clientes_objetivo.rds")
productos <- readRDS("productos.rds")

#* @apiTitle API de Recomendación ALS

#* Recomienda un producto al cliente
#* @param cliente_id ID del cliente
#* @get /recomendar
function(cliente_id) {
  cliente_id <- as.character(cliente_id)
  
  if (!(cliente_id %in% rownames(matriz_sparse))) {
    return(list(error = "Cliente no encontrado en matriz"))
  }
  
  user_idx <- which(rownames(matriz_sparse) == cliente_id)
  user_scores <- modelo_ALS$predict(matriz_sparse[user_idx, , drop = FALSE], k = ncol(matriz_sparse), type = "score")
  user_compras <- matriz_sparse[user_idx, ]
  user_scores[user_compras > 0] <- -Inf
  
  best_product_idx <- which.max(user_scores)
  cod_producto <- colnames(matriz_sparse)[best_product_idx]
  cod_producto <- sub("^X", "", cod_producto)
  
  descripcion <- productos$descripcion[productos$cod_est == cod_producto]
  score <- user_scores[best_product_idx]
  
  list(
    cliente = cliente_id,
    producto_recomendado = cod_producto,
    descripcion = descripcion,
    score = score
  )
}

