# --- LIBRERÍAS ---
library(dplyr)
library(tidyr)
library(recommenderlab)
library(recosystem)

# --- CARGAR DATOS ---
maestrostr <- readRDS("Datos/Originales/maestroestr.RDS")
objetivos <- readRDS("Datos/Originales/objetivos.RDS")
tickets_enc <- readRDS("Datos/Originales/tickets_enc.RDS")

# Enriquecer con descripciones
tickets_enc <- tickets_enc %>%
  left_join(maestrostr, by = "cod_est")

# Clientes de objetivo 4
clientes_obj4 <- objetivos$objetivo4$obj

# --- MATRIZ CLIENTE-PRODUCTO ---
resumen <- tickets_enc %>%
  count(id_cliente_enc, cod_est, name = "n_compras_producto")

t <- pivot_wider(resumen, names_from = "cod_est", values_from = "n_compras_producto") %>%
  data.frame()
rownames(t) <- t[, 1]
t <- t[, -1]
t_rec <- as(as.matrix(t), "realRatingMatrix")

# --- REDUCCIÓN DE DIMENSIONALIDAD ---
t_rec_filtered <- t_rec[
  rowCounts(t_rec) > 23 | rownames(t_rec) %in% clientes_obj4, ]
t_rec_filtered <- t_rec_filtered[
  rowMeans(t_rec_filtered) > 1.0 | rownames(t_rec_filtered) %in% clientes_obj4, ]
t_rec_filtered <- t_rec_filtered[, colCounts(t_rec_filtered) > 20]

# Verificación de clientes objetivo 4
verif_clientes <- data.frame(
  ID = clientes_obj4,
  Presente = clientes_obj4 %in% rownames(t_rec_filtered)
)
print("Verificación de clientes de objetivo 4:")
print(verif_clientes)

# --- ALS: ENTRENAMIENTO Y PREDICCIÓN PERSONALIZADA ---

# Convertir a data.frame largo user-item-rating
mat <- as(t_rec_filtered, "matrix")
ratings_df <- as.data.frame(as.table(mat)) %>%
  filter(Freq > 0) %>%
  rename(user = Var1, item = Var2, rating = Freq)

# Guardar temporalmente los datos
train_file <- tempfile()
write.table(ratings_df, file = train_file, sep = " ", row.names = FALSE, col.names = FALSE)

# Entrenar modelo ALS
reco <- Reco()
reco$train(data_file(train_file), opts = list(dim = 30, niter = 30, costp_l2 = 0.1, costq_l2 = 0.1))

# Crear todas las combinaciones cliente-producto
clientes <- clientes_obj4[clientes_obj4 %in% rownames(mat)]
productos <- colnames(mat)
combinaciones <- expand.grid(user = clientes, item = productos)

# Guardar combinaciones
test_file <- tempfile()
write.table(combinaciones, file = test_file, sep = " ", row.names = FALSE, col.names = FALSE)

# Predecir
pred_file <- tempfile()
reco$predict(data_file(test_file), out_file(pred_file))
combinaciones$score <- scan(pred_file)

# Historial de productos ya comprados
productos_comprados <- as(t_rec_filtered[clientes, ], "list")

# Filtrar productos ya comprados
combinaciones_filtradas <- combinaciones %>%
  rowwise() %>%
  filter(!(item %in% productos_comprados[[user]])) %>%
  ungroup()

# Obtener mejor producto no comprado
recomendaciones_als <- combinaciones_filtradas %>%
  group_by(user) %>%
  slice_max(score, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  rename(cliente = user, producto_recomendado = item)

# --- AÑADIR DESCRIPCIÓN DEL PRODUCTO ---
recomendaciones_als <- recomendaciones_als %>%
  mutate(cod_est = gsub("^X", "", producto_recomendado)) %>%
  left_join(maestrostr, by = "cod_est")

# --- EXPORTAR RESULTADOS ---
write.csv(recomendaciones_als, "recomendaciones_objetivo4_als.csv", row.names = FALSE)
saveRDS(recomendaciones_als, "recomendaciones_objetivo4_als.RDS")

# --- LIMPIEZA FINAL ---
file.remove(train_file, test_file, pred_file)

# --- REVISIÓN FINAL ---
print("Recomendaciones ALS para Objetivo 4:")
print(recomendaciones_als[, c("cliente", "producto_recomendado", "descripcion")])










# --- COMPARACIÓN CON IBCF ---

# Entrenar modelo IBCF con método Pearson
modelo_ibcf <- Recommender(t_rec_filtered, method = "IBCF", param = list(method = "Pearson"))

# Predecir 1 ítem para los clientes del objetivo 4
clientes_presentes <- clientes_obj4[clientes_obj4 %in% rownames(t_rec_filtered)]
pred_ibcf <- predict(modelo_ibcf, newdata = t_rec_filtered[clientes_presentes, ], type = "topNList", n = 1)

# Convertir a lista
recom_ibcf <- as(pred_ibcf, "list")

# Formatear
df_ibcf <- data.frame(
  cliente = names(recom_ibcf),
  producto_recomendado_ibcf = sapply(recom_ibcf, function(x) ifelse(length(x) > 0, x[1], NA))
)

