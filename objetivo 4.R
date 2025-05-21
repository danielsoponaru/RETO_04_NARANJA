# -------------------------------------------
# CARGA DE LIBRERÍAS Y DATOS
# -------------------------------------------

library(tidyverse)
library(lubridate)
library(rsparse)
library(dplyr)
library(Matrix)

# 1. Cargar datos desde archivos RDS
maestro <- readRDS("maestroestr.RDS")       
objetivos <- readRDS("objetivos.RDS") 
tickets <- readRDS("tickets_enc.RDS")      

matriz_reducida <- readRDS("MatrizSuperReducida.RDS")
# Como carga dataframe convertimos los NAs en 0 y cambio el formato
matriz_reducida[is.na(matriz_reducida)] <- 0
matriz_reducida <- as(matriz_reducida, "matrix")
matriz_reducida <- as(matriz_reducida, "dgCMatrix")
matriz_reducida@x[matriz_reducida@x >= 1] <- 1

# -------------------------------------------
# PREPARACIÓN DE DATOS PARA EL OBJETIVO 4
# -------------------------------------------

# 3. Extraer clientes que forman parte del objetivo 4
obj4 <- objetivos[[4]]$obj

# Limpiar nombres de columnas de la matriz para eliminar un carácter inicial no deseado
colnames(matriz_reducida) <- sub("^.", "", colnames(matriz_reducida))

# Filtrar la matriz solo para los clientes del objetivo 4
matriz_sparse_filt <- matriz_reducida[rownames(matriz_reducida) %in% obj4, ]

# Convertir matrices a formato sparse para optimizar cálculos
matriz_sparse_filt <- as(matriz_sparse_filt, "dgCMatrix")
matriz_reducida <- as(matriz_reducida, "dgCMatrix")

# 4. Filtrar tickets solo para clientes del objetivo 4 y convertir fechas a formato Date
tickets_filtrados <- tickets %>%
  filter(id_cliente_enc %in% obj4) %>%
  mutate(fecha = ymd(dia))

# 5. Obtener el último ticket de compra por cada cliente (más reciente)
ultimos_tickets <- tickets_filtrados %>%
  group_by(id_cliente_enc) %>%
  filter(fecha == max(fecha)) %>%
  ungroup()

# -------------------------------------------
# VERIFICACIÓN DE CLIENTES Y ENTRENAMIENTO DEL MODELO
# -------------------------------------------

# 8. Verificar que los clientes objetivo estén presentes en la matriz de interacciones
clientes_en_matriz <- intersect(obj4, rownames(matriz_reducida))
length(clientes_en_matriz)

# 10. Inicializar y entrenar modelo WRMF para recomendaciones con parámetros dados
modelo_wrmf_o4 <- WRMF$new(rank = 10L, 
                           lambda = 0.1, 
                           feedback = 'implicit')

modelo_wrmf_o4$fit_transform(matriz_reducida, 
                             n_iter = 1000L, 
                             convergence_tol = 1e-6)

# -------------------------------------------
# EXCLUSIÓN DE PRODUCTOS YA COMPRADOS RECIENTEMENTE
# -------------------------------------------

# 11. Obtener lista de todos los productos en la matriz
productos_matriz <- colnames(matriz_reducida)

# 14. Crear matriz para marcar productos que no deben recomendarse (los ya comprados últimamente)
not_recommend_df <- matrix(0, 
                           nrow = nrow(matriz_reducida), 
                           ncol = ncol(matriz_reducida))
rownames(not_recommend_df) <- rownames(matriz_reducida)
colnames(not_recommend_df) <- productos_matriz

# 15. Para cada cliente, marcar con 1 los productos de su último ticket para excluirlos de recomendaciones
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

# Convertir la matriz de exclusión a formato sparse
not_recommend_matrix <- as(not_recommend_df, "dgCMatrix")

# -------------------------------------------
# GENERACIÓN DE RECOMENDACIONES
# -------------------------------------------

# 16. Predecir recomendaciones para clientes objetivo,
# excluyendo productos comprados recientemente
preds_o4 <- modelo_wrmf_o4$predict(
  not_recommend_matrix[clientes_en_matriz, ],
  k = 1,                                  
  not_recommend = not_recommend_matrix[clientes_en_matriz, ]
)

# -------------------------------------------
# PROCESAMIENTO DE RESULTADOS
# -------------------------------------------

# Extraer ids de productos recomendados (primer recomendación) y asignar nombres de clientes
predicciones <- as.data.frame(attr(preds_o4, "ids"))
predicciones$clientes <- rownames(predicciones)
rownames(predicciones) <- NULL

# Seleccionar columnas relevantes: cliente y producto recomendado
predicciones <- predicciones %>% 
  select(clientes, V1)

# Hacer join para obtener descripción de productos desde tabla maestro
predicciones <- predicciones %>% 
  left_join(maestro, by = c("V1" = "cod_est"))

# Renombrar columnas para mayor claridad
colnames(predicciones) <- c("CLIENTES", "COD_PRODUCTO", "DESCRIPCION")

predicciones
