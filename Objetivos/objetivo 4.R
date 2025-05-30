# -------------------------------------------
# CARGA DE LIBRERÍAS Y DATOS
# -------------------------------------------

if(!requireNamespace("tidyverse", quietly = TRUE)) {
  install.packages("tidyverse")
}

if(!requireNamespace("lubridate", quietly = TRUE)) {
  install.packages("lubridate")
}

if(!requireNamespace("rsparse", quietly = TRUE)) {
  install.packages("rsparse")
}

if(!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}

if(!requireNamespace("Matrix", quietly = TRUE)) {
  install.packages("Matrix")
}


library(tidyverse)
library(lubridate)
library(rsparse)
library(dplyr)
library(Matrix)

maestro <- readRDS("Datos/Originales/maestroestr.RDS")       
objetivos <- readRDS("Datos/Originales/objetivos.RDS") 
tickets <- readRDS("Datos/Originales/tickets_enc.RDS")      

matriz_reducida <- readRDS("Datos/Transformados/MatrizSuperReducida.RDS")
# Como carga dataframe convertimos los NAs en 0 y cambio el formato
matriz_reducida[is.na(matriz_reducida)] <- 0
matriz_reducida <- as(matriz_reducida, "matrix")
matriz_reducida <- as(matriz_reducida, "dgCMatrix")
matriz_reducida@x[matriz_reducida@x >= 1] <- 1

# -------------------------------------------
# PREPARACIÓN DE DATOS PARA EL OBJETIVO 4
# -------------------------------------------

obj4 <- objetivos[[4]]$obj

colnames(matriz_reducida) <- sub("^.", "", colnames(matriz_reducida))

matriz_sparse_filt <- matriz_reducida[rownames(matriz_reducida) %in% obj4, ]

matriz_sparse_filt <- as(matriz_sparse_filt, "dgCMatrix")
matriz_reducida <- as(matriz_reducida, "dgCMatrix")

tickets_filtrados <- tickets %>%
  filter(id_cliente_enc %in% obj4) %>%
  mutate(fecha = ymd(dia))

ultimos_tickets <- tickets_filtrados %>%
  group_by(id_cliente_enc) %>%
  filter(fecha == max(fecha)) %>%
  ungroup()

# -------------------------------------------
# VERIFICACIÓN DE CLIENTES Y ENTRENAMIENTO DEL MODELO
# -------------------------------------------

# Verificar que los clientes objetivo estén presentes en la matriz de interacciones
clientes_en_matriz <- intersect(obj4, rownames(matriz_reducida))
length(clientes_en_matriz)

modelo_wrmf_o4 <- WRMF$new(rank = 10L, 
                           lambda = 0.1, 
                           feedback = 'implicit')

modelo_wrmf_o4$fit_transform(matriz_reducida, 
                             n_iter = 1000L, 
                             convergence_tol = 1e-6)

# -------------------------------------------
# EXCLUSIÓN DE PRODUCTOS YA COMPRADOS RECIENTEMENTE
# -------------------------------------------

productos_matriz <- colnames(matriz_reducida)

not_recommend_df <- matrix(0, 
                           nrow = nrow(matriz_reducida), 
                           ncol = ncol(matriz_reducida))
rownames(not_recommend_df) <- rownames(matriz_reducida)
colnames(not_recommend_df) <- productos_matriz

# Para cada cliente, marcar con 1 los productos de su último ticket para excluirlos de recomendaciones
for (cliente in clientes_en_matriz) {
  # Extraer productos comprados en el último ticket del cliente
  productos_ultimo_ticket <- ultimos_tickets %>%
    filter(id_cliente_enc == cliente) %>%
    pull(cod_est)
  
  # Mantener solo productos que están en la matriz de productos
  productos_validos <- intersect(productos_ultimo_ticket, productos_matriz)
  
  if (length(productos_validos) > 0) {
    # Marcar con 1 en la matriz de exclusión
    not_recommend_df[cliente, productos_validos] <- 1
    cat("Cliente", cliente, "tiene", length(productos_validos), "productos marcados como no recomendables\n")
  } else {
    cat("Cliente", cliente, "no tiene productos para excluir\n")
  }
}

not_recommend_matrix <- as(not_recommend_df, "dgCMatrix")

# -------------------------------------------
# GENERACIÓN DE RECOMENDACIONES
# -------------------------------------------

# Predecir recomendaciones para clientes objetivo,
# excluyendo productos comprados recientemente
preds_o4 <- modelo_wrmf_o4$predict(
  not_recommend_matrix[clientes_en_matriz, ],
  k = 1,                                  
  not_recommend = not_recommend_matrix[clientes_en_matriz, ]
)

# -------------------------------------------
# PROCESAMIENTO DE RESULTADOS
# -------------------------------------------

predicciones <- as.data.frame(attr(preds_o4, "ids"))
predicciones$clientes <- rownames(predicciones)
rownames(predicciones) <- NULL

predicciones <- predicciones %>% 
  select(clientes, V1)

predicciones <- predicciones %>% 
  left_join(maestro, by = c("V1" = "cod_est"))

colnames(predicciones) <- c("CLIENTES", "COD_PRODUCTO", "DESCRIPCION")

predicciones

write.csv(predicciones, "Resultados/resultados_objetivo4.csv", row.names = FALSE)


