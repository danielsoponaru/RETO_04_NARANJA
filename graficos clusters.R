library(dplyr)
library(ggplot2)
library(lubridate)

clientes_clus=readRDS("clientes_clusterizados.RDS")
tickets_enc<-readRDS("tickets_enc.RDS")
maestroestr<-readRDS("maestroestr.RDS")

datos_clus=left_join(x=clientes_clus, y =tickets_enc, by="id_cliente_enc")
datos_clus=left_join(x=datos_clus, y=maestroestr, by="cod_est")
head(datos_clus)

# Paso 1: Contar artículos por compra y cluster
articulos_por_compra <- datos_clus %>%
  group_by(kmeans_cluster, num_ticket) %>%
  summarise(cantidad_articulos = n(), .groups = 'drop')

# Paso 2: Graficar histograma por cluster
ggplot(articulos_por_compra, aes(x = cantidad_articulos)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "steelblue") +
  facet_wrap(~ kmeans_cluster, scales = "free_y") +
  labs(
    title = "Cantidad de artículos por compra, por cluster",
    x = "Cantidad de artículos por compra",
    y = "Número de compras"
  ) +
  theme_minimal()

# Paso 1: Calcular conteo por artículo y cluster
top_articulos_cluster <- datos_clus %>%
  group_by(kmeans_cluster, descripcion) %>%
  summarise(cantidad = n(), .groups = "drop") %>%
  arrange(kmeans_cluster, desc(cantidad)) %>%
  group_by(kmeans_cluster) %>%
  slice_head(n = 10) %>%
  ungroup()

# Paso 2: Graficar con facet_wrap para ver cada cluster separado
ggplot(top_articulos_cluster, aes(x = reorder(descripcion, cantidad), y = cantidad, fill = kmeans_cluster)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  facet_wrap(~ kmeans_cluster, scales = "free_y") +
  labs(
    title = "Top 10 artículos más comprados por cluster",
    x = "Código de artículo",
    y = "Cantidad de compras"
  ) +
  theme_minimal() +
  theme(legend.position = "none")



# Asegurar que 'dia' esté en formato fecha
datos_clus <- datos_clus %>%
  mutate(dia = as.Date(as.character(dia), format = "%Y%m%d"))

# Calcular duración por cliente y cluster
actividad_por_cluster <- datos_clus %>%
  group_by(id_cliente_enc, kmeans_cluster) %>%
  summarise(
    primera_compra = min(dia),
    ultima_compra = max(dia),
    .groups = "drop"
  ) %>%
  mutate(duracion_dias = as.numeric(ultima_compra - primera_compra)) %>%
  arrange(desc(duracion_dias))

# BOXPLOT por cluster
grafico_box <- ggplot(actividad_por_cluster, aes(x = kmeans_cluster, y = duracion_dias, fill = kmeans_cluster)) +
  geom_boxplot(color = "grey30") +
  labs(
    title = "Distribución del tiempo de actividad de los clientes por cluster",
    x = "Cluster",
    y = "Duración entre primera y última compra (días)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# HISTOGRAMA por cluster (opcional)
grafico_hist <- ggplot(actividad_por_cluster, aes(x = duracion_dias, fill = kmeans_cluster)) +
  geom_histogram(binwidth = 30, color = "grey30") +
  facet_wrap(~kmeans_cluster, scales = "free_y") +
  labs(
    title = "Tiempo de actividad de los clientes por cluster",
    x = "Duración entre primera y última compra (días)",
    y = "Número de clientes"
  ) +
  theme_minimal() +
  theme(legend.position = "none")


# Paso 1: Convertir fecha y extraer día de la semana
datos_dias_semana <- datos_clus %>%
  mutate(
    dia = as.Date(as.character(dia), format = "%Y%m%d"),
    dia_semana = wday(dia, label = TRUE, abbr = FALSE, week_start = 1)  # Lunes a Domingo
  )

# Paso 2: Contar compras por día y cluster
compras_por_dia <- datos_dias_semana %>%
  group_by(kmeans_cluster, dia_semana) %>%
  summarise(cantidad = n(), .groups = "drop")

# Paso 3: Graficar
ggplot(compras_por_dia, aes(x = dia_semana, y = cantidad, fill = kmeans_cluster)) +
  geom_col(position = "dodge") +
  labs(
    title = "Días de la semana con más compras por cluster",
    x = "Día de la semana",
    y = "Cantidad de compras"
  ) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal()



