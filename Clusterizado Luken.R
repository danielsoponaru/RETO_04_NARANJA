  #================================
  # 1. Librerías necesarias
  # ===============================
library(lubridate)
library(dplyr)
library(naniar)
library(tidyr)
library(ggplot2)
library(purrr)
library(cluster)
library(recosystem)


# ===============================
# 2. Carga y preprocesamiento de datos
# ===============================
maestrostr <- readRDS("maestroestr.RDS")
objetivos  <- readRDS("objetivos.RDS")
tickets_enc <- readRDS("tickets_enc.RDS")

# Formateo inicial
tickets_enc <- tickets_enc %>%
  mutate(
    num_ticket = as.character(num_ticket),
    dia        = ymd(dia),
    num_ticket = paste(num_ticket, id_cliente_enc)
  )

# Verificar NAs
vis_miss(tickets_enc, warn_large_data = FALSE)

# Agregar día de la semana
tickets_enc <- tickets_enc %>%
  mutate(DiaSemana = wday(dia, week_start = 1))

# ===============================
# 3. Ingeniería de características para clustering
# ===============================
datos_clientes <- tickets_enc %>%
  group_by(id_cliente_enc) %>%
  summarise(
    total_productos       = n(),
    productos_distintos    = n_distinct(cod_est),
    dias_activos          = as.numeric(max(dia) - min(dia)),
    compras_por_semana    = ifelse(dias_activos > 0, n() / (dias_activos / 7), n()),
    compras_entre_semana  = sum(DiaSemana %in% 1:5),
    compras_fin_de_semana = sum(DiaSemana %in% 6:7)
  ) %>%
  ungroup()

# Eliminar outliers con IQR
outliers_iqr <- function(x) {
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  which(x < (q1 - 1.5 * iqr) | x > (q3 + 1.5 * iqr))
}
idx_out <- unique(unlist(lapply(datos_clientes[-1], outliers_iqr)))
datos_sin_outliers <- datos_clientes[-idx_out, ]

# Escalado
datos_scaled <- datos_sin_outliers %>% select(-id_cliente_enc) %>% scale()

# ===============================
# 4. Método del codo para K-means
# ===============================
set.seed(123)
wss <- map_dbl(1:15, ~ kmeans(datos_scaled, centers = .x, nstart = 25)$tot.withinss)

elbow_df <- tibble(k = 1:15, wss = wss)

# Graficar método del codo
ggplot(elbow_df, aes(x = k, y = wss)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Método del Codo: WSS vs Número de Clusters",
    x = "Número de Clusters (k)",
    y = "Suma de Cuadrados Intra-cluster (WSS)"
  ) +
  theme_minimal()

# ===============================
# 5. K-means clustering (k = 3)
# ===============================
set.seed(123)
kmeans_result <- kmeans(datos_scaled, centers = 3, nstart = 25)
datos_sin_outliers$kmeans_cluster <- as.factor(kmeans_result$cluster)

# Visualización con PCA
pca_res <- prcomp(datos_scaled)
datos_sin_outliers <- datos_sin_outliers %>%
  mutate(
    pca1 = pca_res$x[,1],
    pca2 = pca_res$x[,2]
  )

ggplot(datos_sin_outliers, aes(x = pca1, y = pca2, color = kmeans_cluster)) +
  geom_point(alpha = 0.7) +
  labs(
    title = "Clusters K-means en 2 Componentes Principales",
    x = "PCA1",
    y = "PCA2",
    color = "Cluster"
  ) +
  theme_minimal()

# ===============================
# 6. Guardar asignación de clusters
# ===============================
clientes_clusterizados <- datos_sin_outliers %>% select(id_cliente_enc, kmeans_cluster)
saveRDS(clientes_clusterizados, "clientes_clusterizados.RDS")

# ===============================
# 7. Crear matriz unificada con clusters
# ===============================
matriz_base <- readRDS("matriz.RDS")
matriz_df   <- matriz_base %>%
  as.data.frame() %>%
  mutate(id_cliente_enc = rownames(.))

matriz_con_cluster <- matriz_df %>%
  inner_join(clientes_clusterizados, by = "id_cliente_enc") %>%
  select(id_cliente_enc, kmeans_cluster, everything())

saveRDS(matriz_con_cluster, "matriz_con_cluster.RDS")

# ===============================
# 8. Tabla de centroides
# ===============================
centroides_clusters <- as.data.frame(kmeans_result$centers)
centroides_clusters$cluster <- paste0("Cluster_", 1:nrow(centroides_clusters)) 

centroide_general <- colMeans(datos_scaled)
centroide_general_df <- as.data.frame(t(centroide_general))
centroide_general_df$cluster <- "Centroide_Global"

tabla_centroides <- bind_rows(centroides_clusters, centroide_general_df) %>%
  select(cluster, everything())

print(tabla_centroides)
