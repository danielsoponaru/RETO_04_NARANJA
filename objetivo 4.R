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

# ===========================================
# 🔵 6. Entrenar ALS (WRMF) con recosystem
# ===========================================
train_file <- tempfile()
write.table(rating_df, file = train_file,
            sep = " ", row.names = FALSE, col.names = FALSE)

reco <- Reco()
reco$train(
  data_file(train_file),
  opts = list(dim = 30,               
              niter = 30,             
              costp_l2 = 0.1,         
              costq_l2 = 0.1,         
              verbose = FALSE)
)

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
