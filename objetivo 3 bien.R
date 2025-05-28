# Cargar librerías necesarias
library(recommenderlab)
library(rsparse)
library(Matrix)
library(dplyr)

# 1. Cargar datos
data <- readRDS("MatrizSuperReducida.RDS")
objetivos <- readRDS("objetivos.RDS")
clientes_objetivo <- objetivos$objetivo2$obj
productos <- readRDS("maestroestr.RDS")

#  Vector con los códigos de los 20 productos en oferta (reemplaza con los reales si hace falta)
productos_en_oferta <- c("X12650103", "X01027405", "X05030101", "X05030102", "X01012310",
                         "X11040303", "X08230125", "X01201505", "X05040180", "X01201005", "X09070103",
                         "X04200505", "X01026410", "X05040181", "X04201005", "X12670111", "X08100903",
                         "X01013315", "X01027205", "X12650101"
)

# 2. Preparar la matriz
matriz <- replace(data, is.na(data), 0) 
matriz[matriz > 10] <- 0
matriz_dense <- as.matrix(matriz)
matriz_sparse <- as(Matrix(matriz_dense, sparse = TRUE), "dgCMatrix")

# 3. Entrenar modelo ALS
modelo_ALS <- WRMF$new(rank = 30, lambda = 0.1, feedback = "implicit")
modelo_ALS$fit_transform(matriz_sparse)

# 4. Recomendaciones para todos los clientes
id_usuarios <- 1:nrow(matriz)  # Todos los clientes

# 5. Predecir top 50 productos por cliente
predicciones <- modelo_ALS$predict(matriz_sparse[id_usuarios, , drop = FALSE], k = 1)

# 6. Filtrar predicciones solo a productos en oferta
cols_oferta <- which(colnames(matriz) %in% productos_en_oferta)

filtrar_oferta <- function(indices, cols_oferta) {
  inter <- intersect(indices, cols_oferta)
  if (length(inter) == 0) return(NA)
  return(inter[1])
}

recomendaciones_filtradas <- sapply(predicciones, filtrar_oferta, cols_oferta = cols_oferta)
productos_recomendados <- colnames(matriz)[recomendaciones_filtradas]

# 7. Fallback: sorteo ponderado por popularidad
popularidad <- colSums(matriz_sparse)[productos_en_oferta]

set.seed(123)
fallbacks <- sample(
  productos_en_oferta,
  sum(is.na(recomendaciones_filtradas)),
  replace = TRUE,
  prob = popularidad
)

# Convertir a nombres de producto
productos_recomendados <- colnames(matriz)[recomendaciones_filtradas]
productos_recomendados[is.na(productos_recomendados)] <- fallbacks

# 8. Construir resultado final
resultado <- data.frame(
  cliente = rownames(matriz),
  producto_recomendado = productos_recomendados
)

# Limpieza de formatos
resultado$producto_recomendado <- as.character(resultado$producto_recomendado)
productos$cod_est <- as.character(productos$cod_est)
resultado$producto_recomendado <- sub("^X", "", resultado$producto_recomendado)
productos$cod_est <- sub("^X", "", productos$cod_est)

# Merge para obtener descripción del producto
resultado_final <- merge(resultado,
                         productos,
                         by.x = "producto_recomendado",
                         by.y = "cod_est",
                         all.x = TRUE)

# Selección de columnas finales
resultado_final <- resultado_final[, c("cliente", "producto_recomendado", "descripcion")]

# Ver resumen de recomendaciones
ct <- resultado_final %>%
  group_by(descripcion) %>%
  summarise(cantidad = n()) %>% 
  arrange(desc(cantidad))

# Mostrar tabla resumen
print(ct)

##### GUARDAR LOS RESULTADOS #####
write.csv(ct, file = "Resultados/resultados_objetivo3.csv", row.names = FALSE)

# ----------------------------------------------------------
# 9. EVALUACIÓN DEL MODELO ALS (Feedback Implícito)
# ----------------------------------------------------------


