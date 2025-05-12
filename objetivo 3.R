maestrostr <- readRDS("Datos/maestroestr.RDS")
objetivos <- readRDS("Datos/objetivos.RDS")
tickets_enc <- readRDS("Datos/tickets_enc.RDS")
matriz=readRDS("Datos/matriz.RDS")

# Cargar librerías necesarias
library(tidyverse)
library(recosystem)

# 1. Transponer la matriz: necesitamos clientes en filas, productos en columnas
matriz_correcta <- t(matriz)  # ahora filas = clientes, columnas = productos

# 2. Convertir matriz a formato largo (cliente, producto, rating)
matriz_larga <- matriz_correcta %>%
  as.data.frame() %>%
  rownames_to_column("cliente") %>%
  pivot_longer(cols = -cliente, names_to = "producto", values_to = "rating") %>%
  filter(!is.na(rating) & rating > 0)

# 3. Guardar en formato para recosystem
train_file <- tempfile()
write.table(matriz_larga, file = train_file, sep = " ", row.names = FALSE, col.names = FALSE)

# 4. Crear y entrenar modelo ALS
r <- Reco()
r$train(data_file(train_file), opts = list(dim = 20, costp_l2 = 0.1, costq_l2 = 0.1, nthread = 2, niter = 30))

# 5. Lista de productos promocionados
productos_promocion <- c(
  "14351005", "12650103", "01027405", "05030101", "05030102", "01012310", "11040303",
  "08230125", "01201505", "05040180", "01201005", "09070103", "04200505",
  "01026410", "05040181", "04201005", "12670111", "08100903", "01013315",
  "01027205"
)

# 6. Crear todas las combinaciones cliente-producto en promoción
clientes <- unique(matriz_larga$cliente)
pred_data <- expand.grid(cliente = clientes, producto = productos_promocion)

# 7. Guardar combinaciones para predicción
test_file <- tempfile()
write.table(pred_data, file = test_file, sep = " ", row.names = FALSE, col.names = FALSE)

# 8. Archivo para guardar puntuaciones
output_file <- tempfile()

# 9. Predecir scores
r$predict(data_file(test_file), out_file(output_file))
scores <- scan(output_file)
pred_data$score <- scores

# 10. Elegir el mejor producto por cliente
recomendaciones <- pred_data %>%
  group_by(cliente) %>%
  slice_max(order_by = score, n = 1, with_ties = FALSE)

# 11. Mostrar recomendaciones finales
print(recomendaciones)
