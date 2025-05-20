# ===============================
# 1. Librerías necesarias
# ===============================
library(dplyr)
library(ggplot2)
library(tidyr)
library(GGally)
library(lubridate)
library(readr)

# ===============================
# 2. Cargar datos
# ===============================
# Datos originales necesarios para reconstruir las variables
tickets_enc <- readRDS("tickets_enc.RDS")
clientes_clusterizados <- readRDS("clientes_clusterizados.RDS")
matriz_base <- readRDS("matriz.RDS")

# ===============================
# 3. Preprocesamiento de tickets
# ===============================
tickets_enc <- tickets_enc %>%
  mutate(
    num_ticket = as.character(num_ticket),
    dia = ymd(dia),
    num_ticket = paste(num_ticket, id_cliente_enc),
    DiaSemana = wday(dia, week_start = 1)
  )

# ===============================
# 4. Ingeniería de características por cliente
# ===============================
# Agregar las variables de comportamiento por cliente
datos_clientes <- tickets_enc %>%
  group_by(id_cliente_enc) %>%
  summarise(
    total_productos        = n(),
    productos_distintos    = n_distinct(cod_est),
    dias_activos           = as.numeric(max(dia) - min(dia)),
    compras_por_semana     = ifelse(dias_activos > 0, n() / (dias_activos / 7), n()),
    compras_entre_semana   = sum(DiaSemana %in% 1:5),
    compras_fin_de_semana  = sum(DiaSemana %in% 6:7)
  ) %>%
  ungroup()

# ===============================
# 5. Reconstruir matriz con clusters + variables
# ===============================
matriz_df <- matriz_base %>%
  as.data.frame() %>%
  mutate(id_cliente_enc = rownames(.))

# Unir los clusters y las variables de comportamiento por cliente
matriz_con_cluster <- matriz_df %>%
  inner_join(clientes_clusterizados, by = "id_cliente_enc") %>%
  left_join(datos_clientes, by = "id_cliente_enc")

# Verifica que las variables de comportamiento estén presentes
names(matriz_con_cluster)

# ===============================
# 6. Análisis descriptivo por cluster
# ===============================
summary_tabla <- matriz_con_cluster %>%
  group_by(kmeans_cluster) %>%
  summarise(
    n_clientes              = n(),
    total_productos_medio   = mean(total_productos, na.rm = TRUE),
    productos_distintos     = mean(productos_distintos, na.rm = TRUE),
    dias_activos_promedio   = mean(dias_activos, na.rm = TRUE),
    compras_por_semana      = mean(compras_por_semana, na.rm = TRUE),
    compras_entre_semana    = mean(compras_entre_semana, na.rm = TRUE),
    compras_fin_de_semana   = mean(compras_fin_de_semana, na.rm = TRUE),
    prop_fin_semana         = mean(compras_fin_de_semana / (compras_entre_semana + compras_fin_de_semana), na.rm = TRUE)
  )

print(summary_tabla)

# ===============================
# 7. Boxplots por variable
# ===============================
variables <- c("total_productos", "productos_distintos", "dias_activos", 
               "compras_por_semana", "compras_entre_semana", "compras_fin_de_semana")

for (var in variables) {
  p <- ggplot(matriz_con_cluster, aes_string(x = "kmeans_cluster", y = var, fill = "kmeans_cluster")) +
    geom_boxplot(alpha = 0.7) +
    labs(
      title = paste("Distribución de", var, "por Cluster"),
      x = "Cluster",
      y = var
    ) +
    theme_minimal() +
    theme(legend.position = "none")
  print(p)
}

# ===============================
# 8. Perfiles promedio por cluster (gráfico resumen)
# ===============================
promedios_long <- matriz_con_cluster %>%
  group_by(kmeans_cluster) %>%
  summarise(across(all_of(variables), ~mean(.x, na.rm = TRUE))) %>%
  pivot_longer(-kmeans_cluster, names_to = "variable", values_to = "valor")

ggplot(promedios_long, aes(x = variable, y = valor, fill = kmeans_cluster)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(
    title = "Promedio de Variables por Cluster",
    x = "Variable",
    y = "Valor Promedio"
  ) +
  theme_minimal()

# ===============================
# 9. Correlaciones dentro de cada cluster
# ===============================
for (i in unique(matriz_con_cluster$kmeans_cluster)) {
  datos_cluster <- matriz_con_cluster %>%
    filter(kmeans_cluster == i) %>%
    select(all_of(variables)) %>%
    drop_na()
  
  print(paste("Correlaciones - Cluster", i))
  print(ggpairs(datos_cluster, title = paste("Correlaciones en Cluster", i)))
}

# ===============================
# 10. Top clientes por total de productos
# ===============================
top_clientes <- matriz_con_cluster %>%
  group_by(kmeans_cluster) %>%
  slice_max(total_productos, n = 5, with_ties = FALSE) %>%
  arrange(kmeans_cluster, desc(total_productos)) %>%
  select(kmeans_cluster, id_cliente_enc, total_productos, compras_por_semana)

print(top_clientes)

# ===============================
# 11. Estadísticas extendidas por cluster
# ===============================
summary_extendido <- matriz_con_cluster %>%
  group_by(kmeans_cluster) %>%
  summarise(across(all_of(variables),
                   list(media = ~mean(.x, na.rm = TRUE),
                        mediana = ~median(.x, na.rm = TRUE),
                        sd = ~sd(.x, na.rm = TRUE)),
                   .names = "{.col}_{.fn}"))

print(summary_extendido)
