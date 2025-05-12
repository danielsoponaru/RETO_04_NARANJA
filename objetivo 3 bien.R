maestrostr <- readRDS("Datos/originales/maestroestr.RDS")
objetivos <- readRDS("Datos/originales/objetivos.RDS")
tickets_enc <- readRDS("Datos/originales/tickets_enc.RDS")
matriz=readRDS("Datos/transformados/matriz.RDS")

# Cargar librerías
library(tidyverse)
library(recosystem)

# TRANSFORMACIÓN DE MATRIZ
# -------------------------------------------
# 1. Transponer la matriz (clientes = filas, productos = columnas)
matriz_correcta <- t(matriz)

# 2. Verificar nombres
if (is.null(rownames(matriz_correcta)) | is.null(colnames(matriz_correcta))) {
  stop("La matriz transpuesta no tiene nombres de fila o columna.")
}

# 3. Verificar si hay valores positivos
if (sum(matriz_correcta > 0, na.rm = TRUE) == 0) {
  stop("La matriz no contiene valores positivos.")
}

# 4. Convertir matriz a formato largo
matriz_larga_check <- matriz_correcta %>%
  as.data.frame() %>%
  rownames_to_column("cliente") %>%
  pivot_longer(-cliente, names_to = "producto", values_to = "rating")

# 5. Verificar si hay suficientes datos tras filtrar ceros y NA
matriz_larga <- matriz_larga_check %>%
  filter(!is.na(rating) & rating > 0)

if (nrow(matriz_larga) == 0) {
  stop("No hay datos con rating > 0 para entrenar el modelo.")
}

# ENTRENAMIENTO DEL MODELO
# -------------------------------------------
# Guardar archivo temporal para entrenamiento
train_file <- tempfile()
write.table(matriz_larga, file = train_file, sep = " ", row.names = FALSE, col.names = FALSE)

# Crear y entrenar modelo ALS
r <- Reco()
r$train(data_file(train_file), opts = list(dim = 20, costp_l2 = 0.1, costq_l2 = 0.1, nthread = 2, niter = 30))

# RECOMENDACIONES
# -------------------------------------------
# Productos en promoción
productos_promocion <- c(
  "14351005", "12650103", "01027405", "05030101", "05030102", "01012310", "11040303",
  "08230125", "01201505", "05040180", "01201005", "09070103", "04200505",
  "01026410", "05040181", "04201005", "12670111", "08100903", "01013315", "01027205"
)

# Crear combinaciones cliente-producto
clientes <- unique(matriz_larga$cliente)
pred_data <- expand.grid(cliente = clientes, producto = productos_promocion)

# Guardar archivo temporal para predicción
test_file <- tempfile()
write.table(pred_data, file = test_file, sep = " ", row.names = FALSE, col.names = FALSE)

# Predecir scores
output_file <- tempfile()
r$predict(data_file(test_file), out_file(output_file))
scores <- scan(output_file)
pred_data$score <- scores

# Obtener el mejor producto para cada cliente
recomendaciones <- pred_data %>%
  group_by(cliente) %>%
  slice_max(order_by = score, n = 1, with_ties = FALSE)

# Mostrar resultado
print(recomendaciones)





# ¿Tiene filas la tabla?
nrow(matriz_larga)

# Mostrar las primeras filas (debería haber columnas: cliente, producto, rating)
head(matriz_larga)

# Ver formato del archivo que se va a usar para entrenar
train_file <- tempfile()
write.table(matriz_larga, file = train_file, sep = " ", row.names = FALSE, col.names = FALSE, quote = FALSE)

# Revisa el contenido del archivo
readLines(train_file, n = 10)

