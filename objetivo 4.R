#### PRIMER TEST ####



library(dplyr)
library(tidyr)
library(recommenderlab)
library(recosystem)


maestrostr <- readRDS("maestroestr.RDS")
objetivos <- readRDS("objetivos.RDS")
tickets_enc <- readRDS("tickets_enc.RDS")

tickets_enc <- tickets_enc %>%
  left_join(maestrostr, by = "cod_est")

clientes_obj4 <- objetivos$objetivo4$obj

resumen <- tickets_enc %>%
  count(id_cliente_enc, cod_est, name = "n_compras_producto")

t <- pivot_wider(resumen, names_from = "cod_est", values_from = "n_compras_producto") %>%
  data.frame()
rownames(t) <- t[, 1]
t <- t[, -1]
t_rec <- as(as.matrix(t), "realRatingMatrix")

t_rec_filtered <- t_rec[
  rowCounts(t_rec) > 23 | rownames(t_rec) %in% clientes_obj4, ]
t_rec_filtered <- t_rec_filtered[
  rowMeans(t_rec_filtered) > 1.0 | rownames(t_rec_filtered) %in% clientes_obj4, ]
t_rec_filtered <- t_rec_filtered[, colCounts(t_rec_filtered) > 20]

verif_clientes <- data.frame(
  ID = clientes_obj4,
  Presente = clientes_obj4 %in% rownames(t_rec_filtered)
)
print("Verificación de clientes de objetivo 4:")
print(verif_clientes)

# --- ALS: ENTRENAMIENTO Y PREDICCIÓN PERSONALIZADA ---

mat <- as(t_rec_filtered, "matrix")
ratings_df <- as.data.frame(as.table(mat)) %>%
  filter(Freq > 0) %>%
  rename(user = Var1, item = Var2, rating = Freq)

train_file <- tempfile()
write.table(ratings_df, file = train_file, sep = " ", row.names = FALSE, col.names = FALSE)

reco <- Reco()
reco$train(data_file(train_file), opts = list(dim = 30, niter = 30, costp_l2 = 0.1, costq_l2 = 0.1))

clientes <- clientes_obj4[clientes_obj4 %in% rownames(mat)]
productos <- colnames(mat)
combinaciones <- expand.grid(user = clientes, item = productos)

test_file <- tempfile()
write.table(combinaciones, file = test_file, sep = " ", row.names = FALSE, col.names = FALSE)

pred_file <- tempfile()
reco$predict(data_file(test_file), out_file(pred_file))
combinaciones$score <- scan(pred_file)

productos_comprados <- as(t_rec_filtered[clientes, ], "list")

combinaciones_filtradas <- combinaciones %>%
  rowwise() %>%
  filter(!(item %in% productos_comprados[[user]])) %>%
  ungroup()

recomendaciones_als <- combinaciones_filtradas %>%
  group_by(user) %>%
  slice_max(score, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  rename(cliente = user, producto_recomendado = item)

# --- AÑADIR DESCRIPCIÓN DEL PRODUCTO ---
recomendaciones_als <- recomendaciones_als %>%
  mutate(cod_est = gsub("^X", "", producto_recomendado)) %>%
  left_join(maestrostr, by = "cod_est")

write.csv(recomendaciones_als, "recomendaciones_objetivo4_als.csv", row.names = FALSE)
saveRDS(recomendaciones_als, "recomendaciones_objetivo4_als.RDS")

file.remove(train_file, test_file, pred_file)

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


#### SEGUNDO TEST ####

library(dplyr)
library(tidyr)
library(recosystem)

# --- Cargar datos ---
maestrostr <- readRDS("maestroestr.RDS")
objetivos <- readRDS("objetivos.RDS")
tickets_enc <- readRDS("tickets_enc.RDS")

clientes_obj4 <- objetivos$objetivo4$obj

# --- Crear matriz cliente-producto ---
resumen <- tickets_enc %>%
  count(id_cliente_enc, cod_est, name = "n_compras_producto")

t <- pivot_wider(resumen, names_from = "cod_est", values_from = "n_compras_producto") %>%
  data.frame()
rownames(t) <- t[, 1]
t <- t[, -1]

# --- Crear realRatingMatrix ---
library(recommenderlab)
t_rec <- as(as.matrix(t), "realRatingMatrix")

# --- Filtrar clientes y productos más relevantes ---
t_rec_filtered <- t_rec[
  rowCounts(t_rec) > 23 | rownames(t_rec) %in% clientes_obj4, ]
t_rec_filtered <- t_rec_filtered[
  rowMeans(t_rec_filtered) > 1.0 | rownames(t_rec_filtered) %in% clientes_obj4, ]
t_rec_filtered <- t_rec_filtered[, colCounts(t_rec_filtered) > 20]

# --- Verificar clientes objetivo 4 ---
verif_clientes <- data.frame(
  ID = clientes_obj4,
  Presente = clientes_obj4 %in% rownames(t_rec_filtered)
)
print("Verificación de clientes de objetivo 4:")
print(verif_clientes)

# --- ALS: Entrenamiento ---
mat <- as(t_rec_filtered, "matrix")
ratings_df <- as.data.frame(as.table(mat)) %>%
  filter(Freq > 0) %>%
  rename(user = Var1, item = Var2, rating = Freq)

train_file <- tempfile()
write.table(ratings_df, file = train_file, sep = " ", row.names = FALSE, col.names = FALSE)

reco <- Reco()
reco$train(data_file(train_file), opts = list(dim = 30, niter = 30, costp_l2 = 0.1, costq_l2 = 0.1))

# --- Predicción para clientes del objetivo 4 ---
clientes <- clientes_obj4[clientes_obj4 %in% rownames(mat)]
productos <- colnames(mat)
combinaciones <- expand.grid(user = clientes, item = productos)

test_file <- tempfile()
write.table(combinaciones, file = test_file, sep = " ", row.names = FALSE, col.names = FALSE)

pred_file <- tempfile()
reco$predict(data_file(test_file), out_file(pred_file))
combinaciones$score <- scan(pred_file)

# Extraer productos de la última compra (último ticket)
ultimas_compras <- tickets_enc %>%
  filter(id_cliente_enc %in% clientes) %>%
  group_by(id_cliente_enc) %>%
  filter(dia == max(dia)) %>%
  summarise(cesta_ultima_compra = list(unique(cod_est)))

# Solución → Hacer left_join antes:
combinaciones_filtradas <- combinaciones %>%
  mutate(user = as.character(user)) %>%
  left_join(ultimas_compras, by = c("user" = "id_cliente_enc")) %>%
  mutate(
    cod_est = as.integer(gsub("^X", "", item)),
    en_cesta_ultima = cod_est %in% cesta_ultima_compra
  ) %>%
  filter(!en_cesta_ultima) %>%
  select(-en_cesta_ultima, -cesta_ultima_compra)

# --- Seleccionar mejor recomendación por cliente ---
recomendaciones_als <- combinaciones_filtradas %>%
  group_by(user) %>%
  slice_max(score, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  rename(cliente = user, producto_recomendado = item) %>%
  mutate(cod_est = gsub("^X", "", producto_recomendado))

# --- Añadir descripción del producto ---
recomendaciones_als <- recomendaciones_als %>%
  left_join(maestrostr, by = "cod_est")

# --- Guardar resultados ---
write.csv(recomendaciones_als, "recomendaciones_objetivo4_als_mejorado.csv", row.names = FALSE)
saveRDS(recomendaciones_als, "recomendaciones_objetivo4_als_mejorado.RDS")

# --- Mostrar ---
print("Recomendaciones ALS mejoradas para Objetivo 4:")
print(recomendaciones_als[, c("cliente", "producto_recomendado", "descripcion")])

# --- Limpiar archivos temporales ---
file.remove(train_file, test_file, pred_file)


#### TEST 3 ####

library(dplyr)
library(tidyr)
library(recosystem)
library(recommenderlab)

# --- Cargar datos ---
maestrostr <- readRDS("maestroestr.RDS")
objetivos <- readRDS("objetivos.RDS")
tickets_enc <- readRDS("tickets_enc.RDS")

clientes_obj4 <- objetivos$objetivo4$obj

# --- Matriz cliente-producto ---
resumen <- tickets_enc %>%
  count(id_cliente_enc, cod_est, name = "n_compras_producto")

t <- pivot_wider(resumen, names_from = "cod_est", values_from = "n_compras_producto") %>%
  data.frame()
rownames(t) <- t[, 1]
t <- t[, -1]

# --- Crear realRatingMatrix ---
t_rec <- as(as.matrix(t), "realRatingMatrix")

# --- Reducción menos agresiva ---
t_rec_filtered <- t_rec[
  rowCounts(t_rec) > 10 | rownames(t_rec) %in% clientes_obj4, ] # Reducido a 10
t_rec_filtered <- t_rec_filtered[
  rowMeans(t_rec_filtered) > 0.5 | rownames(t_rec_filtered) %in% clientes_obj4, ] # Reducido a 0.5
t_rec_filtered <- t_rec_filtered[, colCounts(t_rec_filtered) > 10] # Reducido a 10

# --- Verificación ---
verif_clientes <- data.frame(
  ID = clientes_obj4,
  Presente = clientes_obj4 %in% rownames(t_rec_filtered)
)
print("Verificación de clientes de objetivo 4:")
print(verif_clientes)

# --- ALS: Entrenamiento ---
mat <- as(t_rec_filtered, "matrix")
ratings_df <- as.data.frame(as.table(mat)) %>%
  filter(Freq > 0) %>%
  rename(user = Var1, item = Var2, rating = Freq)

train_file <- tempfile()
write.table(ratings_df, file = train_file, sep = " ", row.names = FALSE, col.names = FALSE)

reco <- Reco()
reco$train(data_file(train_file), opts = list(dim = 40, niter = 50, costp_l2 = 0.05, costq_l2 = 0.05)) # Más iteraciones y regularización más suave

# --- Predicción ---
clientes <- clientes_obj4[clientes_obj4 %in% rownames(mat)]
productos <- colnames(mat)
combinaciones <- expand.grid(user = clientes, item = productos)

test_file <- tempfile()
write.table(combinaciones, file = test_file, sep = " ", row.names = FALSE, col.names = FALSE)

pred_file <- tempfile()
reco$predict(data_file(test_file), out_file(pred_file))
combinaciones$score <- scan(pred_file)

# --- Extraer productos de la última compra ---
ultimas_compras <- tickets_enc %>%
  filter(id_cliente_enc %in% clientes) %>%
  group_by(id_cliente_enc) %>%
  filter(dia == max(dia)) %>%
  summarise(cesta_ultima_compra = list(unique(cod_est)))

# --- Añadir info de última compra ---
combinaciones <- combinaciones %>%
  mutate(user = as.character(user)) %>%
  left_join(ultimas_compras, by = c("user" = "id_cliente_enc")) %>%
  mutate(
    cod_est = as.integer(gsub("^X", "", item)),
    en_cesta_ultima = cod_est %in% cesta_ultima_compra
  ) %>%
  filter(!en_cesta_ultima) %>%
  select(-en_cesta_ultima, -cesta_ultima_compra)

# --- Penalización de productos hiper-populares (diversificación) ---
producto_popularidad <- tickets_enc %>%
  count(cod_est, name = "n_veces_comprado")

# Forzar cod_est a character para el join
combinaciones <- combinaciones %>%
  mutate(cod_est = as.character(cod_est))

# Ahora sí, left_join sin problemas
combinaciones <- combinaciones %>%
  left_join(producto_popularidad, by = "cod_est") %>%
  mutate(score_ajustado = score / log1p(n_veces_comprado))

# --- Seleccionar mejor producto (diversificado) ---
recomendaciones_als <- combinaciones %>%
  group_by(user) %>%
  slice_max(score_ajustado, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  rename(cliente = user, producto_recomendado = item) %>%
  mutate(cod_est = gsub("^X", "", producto_recomendado))

# --- Añadir descripción ---
recomendaciones_als <- recomendaciones_als %>%
  left_join(maestrostr, by = "cod_est")

# --- Guardar resultados ---
write.csv(recomendaciones_als, "recomendaciones_objetivo4_als_diversificado.csv", row.names = FALSE)
saveRDS(recomendaciones_als, "recomendaciones_objetivo4_als_diversificado.RDS")

# --- Mostrar ---
print("Recomendaciones ALS diversificadas (última compra):")
print(recomendaciones_als[, c("cliente", "producto_recomendado", "descripcion")])

# --- Limpiar ---
file.remove(train_file, test_file, pred_file)




#### TEST 4 ####

# ---------------------------
# 🔵 Librerías
# ---------------------------
library(dplyr)
library(tidyr)
library(recommenderlab)
library(recosystem)

# ---------------------------
# 🔵 Cargar datos
# ---------------------------
maestrostr <- readRDS("maestroestr.RDS")
objetivos <- readRDS("objetivos.RDS")
tickets_enc <- readRDS("tickets_enc.RDS")

clientes_obj4 <- objetivos$objetivo4$obj

# ---------------------------
# 🔵 Última compra de cada cliente objetivo
# ---------------------------
ultimas_compras <- tickets_enc %>%
  filter(id_cliente_enc %in% clientes_obj4) %>%
  group_by(id_cliente_enc) %>%
  filter(dia == max(dia)) %>%
  summarise(cesta_ultima = list(unique(cod_est)))

# ---------------------------
# 🔵 Crear resumen cliente-producto (frecuencia de compra)
# ---------------------------
resumen <- tickets_enc %>%
  count(id_cliente_enc, cod_est, name = "n_compras_producto")

# Transformar a formato ancho
t <- pivot_wider(resumen, names_from = "cod_est", values_from = "n_compras_producto") %>%
  data.frame()

rownames(t) <- t[, 1]
t <- t[, -1]

# Convertir a matriz de ratings
t_rec <- as(as.matrix(t), "realRatingMatrix")

# ---------------------------
# 🔵 Filtrar clientes
# ---------------------------

# Filtrar clientes a conservar (según el segundo código)
clientes_a_salvar <- c(
  "b51353fcf07cb61280eda45e33679871", "02ff5edaa057b63ea0a0010c5402205c", 
  "25d259d32a2bc254343715f2e347c518", "53ffb83e85fd51cf1ec2fdef3c78b4fd", 
  "26f424b3bba6aaf97952ac599ed39f75", "32cc820ac27ff143c3ea976f3fe69d34", 
  "a57938025d714b65612bf2cfde12136d", "af30d404e282749ccd5f5ad0c8e834c7", 
  "8b9aa623b654a8be21b316a5fdf41007", "e27ceb0a1576648212c4325fdf7d8002",
  "fe234baf66f020e01feb5253dfb398f0", "d85ceefcf666f2b27e3e1e1252e5a1ac", 
  "a8a16b0b76cb14783348e920a59588ed", "1d98f84a5f074ed9c7a47515d4f5f329", 
  "528435b91691a75f5a60c6ccf4c6294c", "8e8315ed119c1382c4d351bbb188510e", 
  "fe52311b246f88407a1142d891ad77ae", "503a6539df48964124fe026b9deb5d13", 
  "a809525fe25b3de695bc87e00bea215f", "ec926181c315b758d775ee64a6a8e033"
)

# Aplicar filtro de clientes
filtro_row_1 <- rowCounts(t_rec) > 23 | rownames(t_rec) %in% clientes_a_salvar
t_rec_1 <- t_rec[filtro_row_1, ]

filtro_row_2 <- rowMeans(t_rec_1) > 1.042 | rownames(t_rec_1) %in% clientes_a_salvar
t_rec_2 <- t_rec_1[filtro_row_2, ]

# ---------------------------
# 🔵 Reducción de la matriz (productos)
# ---------------------------

# Reducir la mitad de la matriz filtrada (filtrar a la mitad de los productos)
n_productos <- ncol(t_rec_2)
n_productos_a_reducir <- floor(n_productos / 2)

# Seleccionar los productos con mayor frecuencia de compras
producto_frecuencias <- colCounts(t_rec_2)
productos_validos <- order(producto_frecuencias, decreasing = TRUE)[1:n_productos_a_reducir]
t_rec_3 <- t_rec_2[, productos_validos]

# Verificación de los clientes seleccionados
verif_clientes <- data.frame(
  ID = clientes_a_salvar,
  Presente = clientes_a_salvar %in% rownames(t_rec_3)
)

# Mostrar la verificación de los clientes
print("Verificación de clientes:")
print(verif_clientes)

# Mostrar el número de clientes y productos seleccionados
cat("\nNúmero de clientes seleccionados: ", nrow(t_rec_3), "\n")
cat("Número de productos seleccionados: ", ncol(t_rec_3), "\n")

# ---------------------------
# 🔵 Recomendaciones
# ---------------------------

# Convertir el objeto 'realRatingMatrix' a un formato de matriz normal
ratings_matrix <- as(t_rec_3, "matrix")

# Convertir la matriz a un data.frame
ratings_long <- as.data.frame(as.table(ratings_matrix)) %>%
  filter(Freq > 0) %>%
  rename(user = Var1, item = Var2, rating = Freq)

train_file <- tempfile()
write.table(ratings_long, file = train_file, sep = " ", row.names = FALSE, col.names = FALSE)

reco <- Reco()
reco$train(data_file(train_file), opts = list(dim = 40, niter = 30, costp_l2 = 0.1, costq_l2 = 0.1))

# Predicción ALS
productos <- colnames(ratings_matrix)
combinaciones <- expand.grid(user = rownames(t_rec_3), item = productos)

test_file <- tempfile()
write.table(combinaciones, file = test_file, sep = " ", row.names = FALSE, col.names = FALSE)

pred_file <- tempfile()
reco$predict(data_file(test_file), out_file(pred_file))
combinaciones$score <- scan(pred_file)

# Filtrar productos ya comprados en la última compra
ultimas_compras_df <- ultimas_compras %>% rename(user = id_cliente_enc)

combinaciones <- combinaciones %>%
  mutate(cod_est = as.character(item)) %>%
  left_join(ultimas_compras_df, by = "user") %>%
  rowwise() %>%
  filter(!(cod_est %in% cesta_ultima)) %>%
  ungroup()

# Seleccionar mejor recomendación ALS
recomendaciones_als <- combinaciones %>%
  group_by(user) %>%
  slice_max(score, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  rename(cliente = user, producto_recomendado = cod_est) %>%
  left_join(maestrostr, by = c("producto_recomendado" = "cod_est"))

# ---------------------------
# 🔵 Limpiar temporales
# ---------------------------
file.remove(train_file, test_file, pred_file)

# Mostrar recomendaciones ALS
cat("\n--- Recomendaciones ALS ---\n")
print(recomendaciones_als[, c("cliente", "producto_recomendado", "descripcion")])



