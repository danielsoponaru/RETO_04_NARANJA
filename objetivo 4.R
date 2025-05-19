# -----------------------------------------
# CARGA DE LIBRERÍAS Y DATOS
# -----------------------------------------
library(tidyverse)
library(lubridate)
library(rsparse)

# Cargar datos
maestro <- readRDS("maestroestr.RDS")
objetivos <- readRDS("objetivos.RDS")
tickets <- readRDS("tickets_enc.RDS")
matriz <- readRDS("matriz.RDS")

# -----------------------------------------
# FILTRADO DE CLIENTES DEL OBJETIVO 4
# -----------------------------------------
obj4 <- objetivos[[4]]$obj  # IDs de los clientes objetivo

# Filtrar los tickets de esos clientes
tickets_filtrados <- tickets %>%
  filter(id_cliente_enc %in% obj4) %>%
  mutate(fecha = ymd(dia))  # convertir fecha

# -----------------------------------------
# SEPARACIÓN DEL HISTORIAL Y ÚLTIMA COMPRA
# -----------------------------------------
ultimos_tickets <- tickets_filtrados %>%
  group_by(id_cliente_enc) %>%
  filter(fecha == max(fecha)) %>%
  ungroup()

historial_tickets <- anti_join(tickets_filtrados, ultimos_tickets, by = "num_ticket")

# -----------------------------------------
# MATRIZ CLIENTE - PRODUCTO
# -----------------------------------------
tickets_matriz_agrupado <- historial_tickets %>%
  group_by(id_cliente_enc, cod_est) %>%
  summarise(N_compras = n(), .groups = "drop")

# Formato ancho sin añadir prefijo "id_"
df_matriz <- pivot_wider(
  tickets_matriz_agrupado,
  names_from = cod_est,
  values_from = N_compras,
  values_fill = 0
)

# Crear matriz dispersa
matriz_sparse_o4 <- as(as.matrix(df_matriz[,-1]), "dgCMatrix")
rownames(matriz_sparse_o4) <- df_matriz$id_cliente_enc
colnames(matriz_sparse_o4) <- colnames(df_matriz)[-1]  # mantener cod_est como nombres de columna

# -----------------------------------------
# ENTRENAR MODELO WRMF
# -----------------------------------------
modelo_wrmf_o4 <- WRMF$new(rank = 10L, lambda = 0.1, feedback = 'implicit')
modelo_wrmf_o4$fit_transform(matriz_sparse_o4, n_iter = 1000L, convergence_tol = 1e-6)

# -----------------------------------------
# GENERAR RECOMENDACIONES
# -----------------------------------------
preds_o4 <- modelo_wrmf_o4$predict(matriz_sparse_o4, k = 1)

clientes <- rownames(matriz_sparse_o4)
productos_predichos <- attr(preds_o4, "ids")

recomendaciones_o4 <- data.frame(
  id_cliente_enc = clientes,
  cod_est = productos_predichos
)

# -----------------------------------------
# UNIR CON MAESTRO PARA DESCRIPCIÓN
# -----------------------------------------
recomendaciones_o4 <- recomendaciones_o4 %>%
  left_join(maestro %>% select(cod_est, descripcion), by = "cod_est")

# -----------------------------------------
# MOSTRAR RESULTADO FINAL
# -----------------------------------------
print(recomendaciones_o4)

















# -----------------------------------------
# CARGA DE LIBRERÍAS Y DATOS
# -----------------------------------------
library(tidyverse)
library(lubridate)
library(rsparse)

# Cargar datos
maestro <- readRDS("maestroestr.RDS")
objetivos <- readRDS("objetivos.RDS")
tickets <- readRDS("tickets_enc.RDS")
matriz <- readRDS("matriz.RDS")

# -----------------------------------------
# FILTRADO DE CLIENTES DEL OBJETIVO 4
# -----------------------------------------
obj4 <- objetivos[[4]]$obj  # IDs de los clientes objetivo

# Filtrar los tickets de esos clientes y convertir fecha
tickets_filtrados <- tickets %>%
  filter(id_cliente_enc %in% obj4) %>%
  mutate(fecha = ymd(dia))

# -----------------------------------------
# SEPARACIÓN DEL HISTORIAL Y ÚLTIMA COMPRA
# -----------------------------------------
ultimos_tickets <- tickets_filtrados %>%
  group_by(id_cliente_enc) %>%
  filter(fecha == max(fecha)) %>%
  ungroup()

historial_tickets <- anti_join(tickets_filtrados, ultimos_tickets, by = "num_ticket")

# -----------------------------------------
# MATRIZ CLIENTE - PRODUCTO
# -----------------------------------------
tickets_matriz_agrupado <- historial_tickets %>%
  group_by(id_cliente_enc, cod_est) %>%
  summarise(N_compras = n(), .groups = "drop")

# Formato ancho sin prefijo
df_matriz <- pivot_wider(
  tickets_matriz_agrupado,
  names_from = cod_est,
  values_from = N_compras,
  values_fill = 0
)

# Crear matriz dispersa
matriz_sparse_o4 <- as(as.matrix(df_matriz[,-1]), "dgCMatrix")
rownames(matriz_sparse_o4) <- df_matriz$id_cliente_enc
colnames(matriz_sparse_o4) <- colnames(df_matriz)[-1]  # mantener cod_est como nombres de columna

# -----------------------------------------
# ENTRENAR MODELO WRMF
# -----------------------------------------
modelo_wrmf_o4 <- WRMF$new(rank = 10L, lambda = 0.1, feedback = 'implicit')
modelo_wrmf_o4$fit_transform(matriz_sparse_o4, n_iter = 1000L, convergence_tol = 1e-6)

# -----------------------------------------
# GENERAR RECOMENDACIONES
# -----------------------------------------
preds_o4 <- modelo_wrmf_o4$predict(matriz_sparse_o4, k = 1)

clientes <- rownames(matriz_sparse_o4)
productos_predichos <- attr(preds_o4, "ids")

recomendaciones_o4 <- data.frame(
  id_cliente_enc = clientes,
  cod_est = productos_predichos
)

# -----------------------------------------
# UNIR CON MAESTRO PARA DESCRIPCIÓN
# -----------------------------------------
recomendaciones_o4 <- recomendaciones_o4 %>%
  left_join(maestro %>% select(cod_est, descripcion), by = "cod_est")

# -----------------------------------------
# MOSTRAR RESULTADO FINAL
# -----------------------------------------
print(recomendaciones_o4)


















# Cargar librerías
library(tidyverse)
library(lubridate)
library(rsparse)

# 1. Cargar datos
maestro <- readRDS("maestroestr.RDS")
objetivos <- readRDS("objetivos.RDS")
tickets <- readRDS("tickets_enc.RDS")

# 2. Obtener los clientes del objetivo 4
obj4 <- objetivos[[4]]$obj

# 3. Filtrar solo tickets de los clientes objetivo
tickets_filtrados <- tickets %>%
  filter(id_cliente_enc %in% obj4) %>%
  mutate(fecha = ymd(dia))

# 4. Obtener última compra por cliente
ultimos_tickets <- tickets_filtrados %>%
  group_by(id_cliente_enc) %>%
  filter(fecha == max(fecha)) %>%
  ungroup()

# 5. Historial anterior (para entrenamiento)
historial_tickets <- anti_join(tickets_filtrados, ultimos_tickets, by = "num_ticket")

# 6. Agrupar historial para crear la matriz
tickets_matriz_agrupado <- historial_tickets %>%
  group_by(id_cliente_enc, cod_est) %>%
  summarise(N_compras = n(), .groups = "drop")

# 7. Crear matriz de cliente-producto
df_matriz <- pivot_wider(
  tickets_matriz_agrupado,
  names_from = "cod_est",
  values_from = "N_compras",
  values_fill = 0,
  names_prefix = "id_"
)

# 8. Convertir a sparseMatrix
matriz_sparse_o4 <- as(as.matrix(df_matriz[,-1]), "dgCMatrix")
rownames(matriz_sparse_o4) <- df_matriz$id_cliente_enc

# 9. ENTRENAMIENTO CON WRMF
modelo_wrmf_o4 <- WRMF$new(rank = 10L, lambda = 0.1, feedback = 'implicit')
modelo_wrmf_o4$fit_transform(matriz_sparse_o4, n_iter = 1000L, convergence_tol = 1e-6)

# 10. Crear matriz de productos NO recomendables (última compra)
ultimos_tickets_bin <- ultimos_tickets %>%
  mutate(cod_est = paste0("id_", cod_est)) %>%
  mutate(valor = 1) %>%
  pivot_wider(names_from = cod_est, values_from = valor, values_fill = 0) %>%
  column_to_rownames("id_cliente_enc")

# 11. Asegurar que tenga mismas columnas y orden que matriz_sparse_o4
columnas_faltantes <- setdiff(colnames(matriz_sparse_o4), colnames(ultimos_tickets_bin))
ultimos_tickets_bin[, columnas_faltantes] <- 0
ultimos_tickets_bin <- ultimos_tickets_bin[, colnames(matriz_sparse_o4)]

# 12. Convertir a matriz sparse para `not_recommend`
not_recommend_matrix <- as(as.matrix(ultimos_tickets_bin), "dgCMatrix")

# 13. Predecir recomendaciones (evitando últimos productos comprados)
preds_o4 <- modelo_wrmf_o4$predict(
  matriz_sparse_o4,
  k = 1,
  not_recommend = not_recommend_matrix
)

# 14. Preparar output
clientes <- rownames(matriz_sparse_o4)
productos_predichos <- attr(preds_o4, "ids") %>% str_remove("id_")

# 15. Armar tabla final con descripciones
recomendaciones_o4 <- data.frame(
  id_cliente_enc = clientes,
  cod_est = productos_predichos
) %>%
  left_join(maestro %>% select(cod_est, descripcion), by = "cod_est") %>%
  select(id_cliente_enc, cod_est, descripcion)

# 16. Ver resultado
print(recomendaciones_o4)
