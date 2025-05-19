#### TEST 5 ####

# ===========================================
# 🔵 1. Librerías
# ===========================================
library(dplyr)
library(tidyr)
library(recosystem)

# ===========================================
# 🔵 2. Cargar datos
# ===========================================
maestrostr   <- readRDS("maestroestr.RDS")
objetivos    <- readRDS("objetivos.RDS")
tickets_enc  <- readRDS("tickets_enc.RDS")

clientes_obj <- objetivos$objetivo4$obj  

# ===========================================
# 🔵 3. Última compra de cada cliente objetivo
# ===========================================
ultimas_compras <- tickets_enc %>%
  filter(id_cliente_enc %in% clientes_obj) %>%
  group_by(id_cliente_enc) %>%
  filter(dia == max(dia)) %>%               
  summarise(cesta_ultima = list(unique(cod_est)), .groups = "drop")

# ===========================================
# 🔵 4. Reducción de dimensionalidad
#    –quedarse con ~50% de productos y clientes
#    –sin perder a los clientes objetivo
# ===========================================
# Contar productos por frecuencia
productos_conteo <- tickets_enc %>%
  count(cod_est) %>%
  arrange(desc(n))

# Calcular la cantidad (50%) fuera del pipeline
n_productos_50 <- floor(nrow(productos_conteo) * 0.5)

# Seleccionar top 50% de productos más comprados
top_productos <- productos_conteo %>%
  slice_head(n = n_productos_50) %>%
  pull(cod_est)

# Contar clientes por actividad
clientes_conteo <- tickets_enc %>%
  count(id_cliente_enc) %>%
  arrange(desc(n))

# Calcular cuántos conservar (50%)
n_clientes_50 <- floor(nrow(clientes_conteo) * 0.5)

# Seleccionar top 50% de clientes
top_clientes <- clientes_conteo %>%
  slice_head(n = n_clientes_50) %>%
  pull(id_cliente_enc)

clientes_finales <- union(top_clientes, clientes_obj)

# 4.3 Dataset reducido
train_data <- tickets_enc %>%
  filter(id_cliente_enc %in% clientes_finales,
         cod_est        %in% top_productos)

# ===========================================
# 🔵 5. Formato user–item–rating (frecuencia)
# ===========================================
rating_df <- train_data %>%
  count(id_cliente_enc, cod_est, name = "rating") %>%
  rename(user = id_cliente_enc,
         item = cod_est)
rating_df <- rating_df %>%
  mutate(user = as.character(user),
         item = as.character(item))

# Crear matriz dispersa user-item
rating_matrix_sparse <- sparseMatrix(
  i = as.integer(factor(rating_df$user)),
  j = as.integer(factor(rating_df$item)),
  x = rating_df$rating,
  dimnames = list(
    user = levels(factor(rating_df$user)),
    item = levels(factor(rating_df$item))
  )
)

rating_df_for_recosystem <- summary(rating_matrix_sparse) %>%
  mutate(user = rownames(rating_matrix_sparse)[i],
         item = colnames(rating_matrix_sparse)[j]) %>%
  select(user, item, x) %>%
  rename(rating = x)

# ===========================================
# 🔵 6. Entrenar ALS (WRMF) con recosystem
# ===========================================
# Guardar en fichero SIN cabecera ni rownames
train_file <- tempfile()
write.table(rating_df_for_recosystem, file = train_file,
            sep = " ", row.names = FALSE, col.names = FALSE, quote = FALSE)

# Entrenar ALS (WRMF) con recosystem
reco <- Reco()
reco$train(
  data_file(train_file),
  opts = list(dim = 30,               
              niter = 30,             
              costp_l2 = 0.1,         
              costq_l2 = 0.1,         
              verbose = FALSE))
# ===========================================
# 🔵 7. Predecir para los 10 clientes objetivo
#    •No se podrá recomendar nada de su última compra
# ===========================================
productos_validos <- unique(rating_df$item)

candidates <- expand.grid(user = clientes_obj,
                          item = productos_validos,
                          KEEP.OUT.ATTRS = FALSE) %>%
  mutate(user = as.character(user),
         item = as.character(item))

test_file <- tempfile()
write.table(candidates, file = test_file,
            sep = " ", row.names = FALSE, col.names = FALSE)

pred_file <- tempfile()
reco$predict(data_file(test_file), out_file(pred_file))
candidates$score <- scan(pred_file)

# ——— Filtrar productos comprados en la última compra
candidates_clean <- candidates %>%
  left_join(ultimas_compras %>% rename(user = id_cliente_enc),
            by = "user") %>%
  rowwise() %>%
  filter(!(item %in% cesta_ultima)) %>%
  ungroup()

# ——— Top‑1 recomendación por cliente
recomendaciones <- candidates_clean %>%
  group_by(user) %>%
  slice_max(score, n = 1, with_ties = FALSE) %>%
  rename(cliente = user,
         cod_est = item) %>%
  left_join(maestrostr, by = "cod_est") %>%
  select(cliente, cod_est, descripcion, score)

# ===========================================
# 🔵 8. Limpiar temporales y mostrar resultado
# ===========================================
file.remove(train_file, test_file, pred_file)

cat("\n=== Recomendaciones ALS (WRMF) –Objetivo 4 ===\n")
print(recomendaciones, row.names = FALSE)

#### Basarse en ####

library(dplyr)
library(lubridate)
library(recosystem)
library(recommenderlab)


maestrostr   <- readRDS("maestroestr.RDS")
objetivos    <- readRDS("objetivos.RDS")
tickets  <- readRDS("tickets_enc.RDS")

obj4<-objetivos[[4]]$obj

tickets_filtrados <- tickets[tickets$id_cliente_enc %in% obj4, ]

tickets_filtrados_agrupados<- tickets_filtrados%>%  
  group_by(id_cliente_enc, cod_est) %>% 
  summarise(N_compras = n())


tickets_filtrados <- tickets_filtrados %>%
  mutate(fecha = ymd(dia)) 

ultimos_tickets <- tickets_filtrados %>%
  group_by(id_cliente_enc) %>%
  filter(fecha == max(fecha)) %>%
  ungroup()

historial_tickets <- anti_join(tickets_filtrados, ultimos_tickets, by = "num_ticket")


tickets_matriz_agrupado <- historial_tickets %>%
  group_by(id_cliente_enc, cod_est) %>%
  summarise(N_compras = n(), .groups = "drop")

df_matriz <- pivot_wider(
  tickets_matriz_agrupado, 
  names_from = "cod_est", 
  values_from = "N_compras", 
  values_fill = 0,
  names_prefix = "id_"
)

matriz_sparse_o4 <- as(as.matrix(df_matriz[,-1]), "dgCMatrix")
rownames(matriz_sparse_o4) <- df_matriz$id_cliente_enc


modelo_wrmf_o4 <- WRMF$new(rank = 10L, lambda = 0.1, feedback = 'implicit')
modelo_wrmf_o4$fit_transform(matriz_sparse_o4, n_iter = 1000L, convergence_tol = 1e-6)

preds_o4 <- modelo_wrmf_o4$predict(matriz_sparse_o4, k = 1)

clientes <- rownames(matriz_sparse_o4)
productos_predichos <- attr(preds_o4, "ids")

recomendaciones_o4 <- data.frame(
  id_cliente_enc = clientes,
  producto_olvidado = productos_predichos
)

recomendaciones_o4 <- recomendaciones_o4 %>%
  mutate(cod_est = str_remove(producto_olvidado, "id_")) %>%
  left_join(maestro %>% select(cod_est, descripcion), by = "cod_est") %>%
  select(id_cliente_enc, cod_est, descripcion)



# Librerías necesarias
library(data.table)
library(Matrix)
library(recosystem)

# 1. Cargar los datos
maestroestr <- readRDS("maestroestr.RDS")
objetivos <- readRDS("objetivos.RDS")
tickets_enc <- readRDS("tickets_enc.RDS")

# 2. Clientes del Objetivo 4 (CORRECTO AHORA)
clientes_obj4 <- objetivos$objetivo4$obj

# 3. Identificar última compra de cada cliente del Objetivo 4
tickets_obj4 <- tickets_enc[id_cli %in% clientes_obj4]
ultima_compra <- tickets_obj4[order(id_cli, -fecha_compra), .SD[1], by = id_cli]

# 4. Crear dataset de entrenamiento (sin la última compra de esos clientes)
tickets_train <- tickets_enc[!id_ticket %in% ultima_compra$id_ticket]

# 5. Crear matriz Cliente-Producto (usando cantidad comprada)
matriz_cp <- tickets_train[, .(cantidad = sum(cantidad)), by = .(id_cli, id_articulo)]

# 6. Convertir a Sparse Matrix
clientes <- unique(matriz_cp$id_cli)
productos <- unique(matriz_cp$id_articulo)

i <- match(matriz_cp$id_cli, clientes)
j <- match(matriz_cp$id_articulo, productos)
x <- matriz_cp$cantidad

sparse_mat <- sparseMatrix(i = i, j = j, x = x,
                           dims = c(length(clientes), length(productos)),
                           dimnames = list(clientes, productos))

# 7. Preparar datos para ALS (recosystem requiere formato tripleta)
train_data <- data_memory(user_index = i, item_index = j, rating = x)

# 8. Entrenar ALS
r <- Reco()
set.seed(123)
r$train(train_data, opts = list(dim = 20, lrate = 0.1, costp_l2 = 0.01, costq_l2 = 0.01, nthread = 4, niter = 20, verbose = TRUE))

# 9. Predecir para clientes del objetivo 4
# Solo predecimos para clientes que están en la matriz de train
clientes_pred_i <- match(clientes_obj4, clientes)
clientes_pred_i <- clientes_pred_i[!is.na(clientes_pred_i)] # Evitar NAs

# Crear pares cliente-producto para predecir
pred_pairs <- expand.grid(user = clientes_pred_i, item = 1:length(productos))
pred_data <- data_memory(user_index = pred_pairs$user, item_index = pred_pairs$item)

# Predicciones ALS
scores <- r$predict(pred_data, out_memory())

# Resultado final en data.table
resultados <- data.table(
  id_cli = clientes[clientes_pred_i][pred_pairs$user],
  id_articulo = productos[pred_pairs$item],
  score = scores
)

# 10. Top-N recomendaciones por cliente
top_n <- 5  # Cambia si quieres más recomendaciones
recomendaciones <- resultados[order(-score), head(.SD, top_n), by = id_cli]

# Mostrar recomendaciones finales
print(recomendaciones)








obj4<-objetivos[[4]]$obj

tickets_filtrados <- tickets[tickets$id_cliente_enc %in% obj4, ]

tickets_filtrados_agrupados<- tickets_filtrados%>%  
  group_by(id_cliente_enc, cod_est) %>% 
  summarise(N_compras = n())


tickets_filtrados <- tickets_filtrados %>%
  mutate(fecha = ymd(dia)) 

ultimos_tickets <- tickets_filtrados %>%
  group_by(id_cliente_enc) %>%
  filter(fecha == max(fecha)) %>%
  ungroup()

historial_tickets <- anti_join(tickets_filtrados, ultimos_tickets, by = "num_ticket")


tickets_matriz_agrupado <- historial_tickets %>%
  group_by(id_cliente_enc, cod_est) %>%
  summarise(N_compras = n(), .groups = "drop")

df_matriz <- pivot_wider(
  tickets_matriz_agrupado, 
  names_from = "cod_est", 
  values_from = "N_compras", 
  values_fill = 0,
  names_prefix = "id_"
)

matriz_sparse_o4 <- as(as.matrix(df_matriz[,-1]), "dgCMatrix")
rownames(matriz_sparse_o4) <- df_matriz$id_cliente_enc


modelo_wrmf_o4 <- WRMF$new(rank = 10L, lambda = 0.1, feedback = 'implicit')
modelo_wrmf_o4$fit_transform(matriz_sparse_o4, n_iter = 1000L, convergence_tol = 1e-6)

preds_o4 <- modelo_wrmf_o4$predict(matriz_sparse_o4, k = 1)

clientes <- rownames(matriz_sparse_o4)
productos_predichos <- attr(preds_o4, "ids")

recomendaciones_o4 <- data.frame(
  id_cliente_enc = clientes,
  producto_olvidado = productos_predichos
)

recomendaciones_o4 <- recomendaciones_o4 %>%
  mutate(cod_est = str_remove(producto_olvidado, "id_")) %>%
  left_join(maestro %>% select(cod_est, descripcion), by = "cod_est") %>%
  select(id_cliente_enc, cod_est, descripcion)







# ——————————————————————————————————————
# Script ALS para Objetivo 4 (Eroski)
# ——————————————————————————————————————

# 0. Instalación (si no las tienes):
# install.packages(c("data.table", "Matrix", "recosystem"))

# 1. Librerías necesarias
library(data.table)
library(Matrix)
library(recosystem)

# 2. Cargar los datos
maestroestr <- readRDS("maestroestr.RDS")      # Datos maestros de clientes
objetivos   <- readRDS("objetivos.RDS")        # Lista con objetivos, incluyendo objetivo4$obj
tickets_enc <- readRDS("tickets_enc.RDS")      # Historial de tickets

# 3. Revisar nombres de columnas y renombrar si hace falta
# Imprimir nombres para verificar:
print(names(tickets_enc))
# Si tus columnas no se llaman id_ticket, id_cli, id_articulo, cantidad, fecha_compra,
# cámbialas con setnames. Ejemplo:
# setnames(tickets_enc,
#          old = c("cliente_id", "articulo_id", "qty", "fecha"),
#          new = c("id_ticket", "id_cli", "id_articulo", "cantidad", "fecha_compra"))

# 4. Extraer clientes del Objetivo 4
clientes_obj4 <- objetivos$objetivo4$obj       # Vector con IDs de cliente

# 5. Identificar la última compra de cada cliente del Objetivo 4
tickets_obj4  <- tickets_enc[id_cli %in% clientes_obj4]
ultima_compra <- tickets_obj4[order(id_cli, -fecha_compra), .SD[1], by = id_cli]

# 6. Crear dataset de entrenamiento (excluyendo esas últimas compras)
tickets_train <- tickets_enc[!id_ticket %in% ultima_compra$id_ticket]

# 7. Construir la matriz cliente-producto con suma de cantidades
matriz_cp <- tickets_train[, .(
  cantidad = sum(cantidad)
), by = .(id_cli, id_articulo)]

# 8. Convertir a Sparse Matrix (para inspección o uso aparte)
clientes  <- unique(matriz_cp$id_cli)
productos <- unique(matriz_cp$id_articulo)
i <- match(matriz_cp$id_cli, clientes)
j <- match(matriz_cp$id_articulo, productos)
x <- matriz_cp$cantidad

sparse_mat <- sparseMatrix(
  i = i, j = j, x = x,
  dims = c(length(clientes), length(productos)),
  dimnames = list(clientes, productos)
)

# 9. Preparar datos para recosystem (formato tripleta)
train_data <- data_memory(
  user_index = i,
  item_index = j,
  rating     = x
)

# 10. Entrenar modelo ALS con recosystem
reco_model <- Reco()
set.seed(123)
reco_model$train(
  train_data,
  opts = list(
    dim       = 20,     # Factores latentes
    lrate     = 0.1,    # Learning rate
    costp_l2  = 0.01,   # Regularización P
    costq_l2  = 0.01,   # Regularización Q
    nthread   = 4,      # Hilos
    niter     = 20,     # Iteraciones
    verbose   = TRUE
  )
)

# 11. Preparar pares para predecir sólo clientes de Objetivo 4 presentes en train
clientes_pred_i <- match(clientes_obj4, clientes)
clientes_pred_i <- clientes_pred_i[!is.na(clientes_pred_i)]  # Quitar NAs

pred_pairs <- expand.grid(
  user = clientes_pred_i,
  item = seq_along(productos)
)

pred_data <- data_memory(
  user_index = pred_pairs$user,
  item_index = pred_pairs$item
)

# 12. Obtener scores de predicción
scores <- reco_model$predict(pred_data, out_memory())

# 13. Construir tabla de resultados y extraer Top-N
resultados <- data.table(
  id_cli      = clientes[clientes_pred_i][pred_pairs$user],
  id_articulo = productos[pred_pairs$item],
  score       = scores
)

top_n <- 5  # Número de recomendaciones por cliente
recomendaciones <- resultados[
  order(-score),
  head(.SD, top_n),
  by = id_cli
]

# 14. Mostrar y guardar resultados
print(recomendaciones)
# fwrite(recomendaciones, "recomendaciones_obj4.csv")

# ——————————————————————————————————————
# Fin del script
# ——————————————————————————————————————















# === INSTALAR Y CARGAR LIBRERÍAS ===
# install.packages("Matrix")
# remotes::install_github("dselivanov/rsparse")
library(dplyr)
library(tidyr)
library(Matrix)
library(rsparse)
library(lubridate)
library(stringr)

# === CARGAR DATOS ===
maestroestr <- readRDS("maestroestr.RDS")      # Datos maestros de clientes
objetivos   <- readRDS("objetivos.RDS")        # Lista con objetivos, incluyendo objetivo4$obj
tickets <- readRDS("tickets_enc.RDS") 


# Datos objetivo 4
obj4 <- objetivos[[4]]$obj

# Filtrar tickets de clientes objetivo
tickets_filtrados <- tickets %>% filter(id_cliente_enc %in% obj4)

# Convertir fecha
tickets_filtrados <- tickets_filtrados %>% mutate(fecha = ymd(dia))

# Últimos tickets (última compra)
ultimos_tickets <- tickets_filtrados %>%
  group_by(id_cliente_enc) %>%
  filter(fecha == max(fecha)) %>%
  ungroup()

# Historial excluyendo la última compra
historial_tickets <- anti_join(tickets_filtrados, ultimos_tickets, by = "num_ticket")

# Crear matriz de interacciones usuario-producto (conteo)
tickets_matriz_agrupado <- historial_tickets %>%
  group_by(id_cliente_enc, cod_est) %>%
  summarise(N_compras = n(), .groups = "drop")

# Pivotar a matriz wide
df_matriz <- pivot_wider(
  tickets_matriz_agrupado,
  names_from = cod_est,
  values_from = N_compras,
  values_fill = 0,
  names_prefix = "id_"
)

# Crear matriz sparse
matriz_sparse_o4 <- as(as.matrix(df_matriz[,-1]), "dgCMatrix")
rownames(matriz_sparse_o4) <- df_matriz$id_cliente_enc

# Entrenar modelo WRMF
modelo_wrmf_o4 <- WRMF$new(rank = 10L, lambda = 0.1, alpha = 40, feedback = "implicit")
modelo_wrmf_o4$fit_transform(matriz_sparse_o4, n_iter = 1000L, convergence_tol = 1e-6)

# Predecir por cliente excluyendo productos de la última compra
codigos_columnas <- str_remove(colnames(matriz_sparse_o4), "id_")

colnames(matriz_sparse_o4) <- paste0("id_", colnames(matriz_sparse_o4))


recomendaciones_list <- vector("list", length = nrow(matriz_sparse_o4))
codigos_columnas <- str_remove(colnames(matriz_sparse_o4), "id_")

for (i in seq_len(nrow(matriz_sparse_o4))) {
  cliente <- rownames(matriz_sparse_o4)[i]
  
  # Productos de la última compra para excluir
  productos_excluir <- ultimos_tickets %>%
    filter(id_cliente_enc == cliente) %>%
    pull(cod_est)
  
  productos_excluir_idx <- which(codigos_columnas %in% productos_excluir)
  
  # Scores para todos los productos para cliente i
  scores <- modelo_wrmf_o4$predict(user_index = i, item_index = seq_len(ncol(matriz_sparse_o4)))
  
  # Excluir productos ya comprados
  if(length(productos_excluir_idx) > 0) {
    scores[productos_excluir_idx] <- -Inf
  }
  
  mejor_idx <- which.max(scores)
  producto_recomendado <- codigos_columnas[mejor_idx]
  
  recomendaciones_list[[i]] <- data.frame(
    id_cliente_enc = cliente,
    cod_est = producto_recomendado,
    stringsAsFactors = FALSE
  )
}

# Combinar resultados en un solo dataframe
recomendaciones_o4 <- do.call(rbind, recomendaciones_list)

# Añadir descripción
recomendaciones_o4 <- recomendaciones_o4 %>%
  left_join(maestro %>% select(cod_est, descripcion), by = "cod_est")

print(recomendaciones_o4)


print(colnames(matriz_sparse_o4))
print(dim(matriz_sparse_o4))


