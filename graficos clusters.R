library(dplyr)
library(ggplot2)
library(lubridate)

options(scipen=999)

clientes_clus=readRDS("clientes_clusterizados.RDS")
tickets_enc<-readRDS("tickets_enc.RDS")
maestroestr<-readRDS("maestroestr.RDS")

tickets_enc$dia<- ymd(tickets_enc$dia)
tickets_enc$cod_est<- as.numeric(tickets_enc$cod_est)
tickets_enc$id_cliente_enc<- as.character(tickets_enc$id_cliente_enc)
maestroestr$cod_est<- as.numeric(maestroestr$cod_est)

datos_clus=left_join(x=clientes_clus, y =tickets_enc, by="id_cliente_enc")
datos_clus=left_join(x=datos_clus, y=maestroestr, by="cod_est")
head(datos_clus)

cluster1 <- datos_clus %>% filter(kmeans_cluster == "1")
cluster2 <- datos_clus %>% filter(kmeans_cluster == "2")
cluster3 <- datos_clus %>% filter(kmeans_cluster == "3")


##########
# GRAFICO 1
grafico_articulos_por_cluster <- function(datos_cluster, num_cluster, color_fill) {
  datos_cluster %>%
    group_by(num_ticket, id_cliente_enc) %>%
    summarise(ArticulosPorCompra = n(), .groups = "drop") %>%
    ggplot(aes(x = ArticulosPorCompra)) +
    geom_histogram(color = "blue", fill = "lightblue", binwidth = 1) +
    labs(
      title = paste("Artículos por compra - Cluster", num_cluster),
      x = "Artículos por ticket",
      y = "Cantidad de tickets"
    ) +
    theme_minimal()
}

grafico1 <- grafico_articulos_por_cluster(cluster1, 1)
grafico2 <- grafico_articulos_por_cluster(cluster2, 2)
grafico3 <- grafico_articulos_por_cluster(cluster3, 3)


w############
# GRAFICO 2

grafico_top_articulos <- function(datos, titulo = "Top 10 artículos más comprados", fill_color = "lightblue", border_color = "lightblue4") {
  top_articulos <- datos %>%
    group_by(cod_est) %>%
    summarise(Cantidad = n(), .groups = "drop") %>%
    arrange(desc(Cantidad)) %>%
    head(10) %>%
    left_join(maestroestr, by = "cod_est")
  
  ggplot(top_articulos, aes(x = reorder(descripcion, Cantidad), y = Cantidad)) +
    geom_col(fill = "lightblue", color = "blue", size = 1.2) +
    geom_text(aes(label = Cantidad), 
              position = position_stack(vjust = 0.5), 
              color = "white", size = 3) +
    labs(title = titulo,
         x = "Artículo",
         y = "Cantidad de veces comprado") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# Generar gráficos para cada cluster
grafico1 <- grafico_top_articulos(cluster1, titulo = "Top 10 artículos más comprados - Cluster 1")
grafico2 <- grafico_top_articulos(cluster2, titulo = "Top 10 artículos más comprados - Cluster 2")
grafico3 <- grafico_top_articulos(cluster3, titulo = "Top 10 artículos más comprados - Cluster 3")



############
# GRAFICO 3

graficar_duracion_actividad <- function(datos_cluster, titulo_cluster, color_fill = "lightblue", color_border = "lightblue4") {
  
  duracion_cliente <- datos_cluster %>%
    group_by(id_cliente_enc) %>%
    summarise(
      primera_compra = min(dia),
      ultima_compra = max(dia),
      .groups = "drop"
    ) %>%
    mutate(duracion_dias = as.numeric(ultima_compra - primera_compra)) %>%
    arrange(desc(duracion_dias))
  
  # Boxplot
  p_box <- ggplot(duracion_cliente, aes(y = duracion_dias)) +
    geom_boxplot(fill = color_fill, color = color_border, size = 1.2) +
    labs(
      title = paste("Distribución del tiempo de actividad de los clientes -", titulo_cluster),
      y = "Duración entre primera y última compra (días)"
    ) +
    theme_minimal()
  
  # Histograma
  p_hist <- ggplot(duracion_cliente, aes(x = duracion_dias)) +
    geom_histogram(binwidth = 30, fill = color_fill, color = color_border, size = 1.2) +
    labs(
      title = paste("Tiempo de actividad de los clientes -", titulo_cluster),
      x = "Duración entre primera y última compra (días)",
      y = "Número de clientes"
    ) +
    theme_minimal()
  
  list(boxplot = p_box, histograma = p_hist)
}

graficos_cluster1 <- graficar_duracion_actividad(cluster1, "Cluster 1")
graficos_cluster2 <- graficar_duracion_actividad(cluster2, "Cluster 2")
graficos_cluster3 <- graficar_duracion_actividad(cluster3, "Cluster 3")

###########
# GRAFICO 4

graficar_intervalo_compras <- function(datos_cluster, titulo_cluster, color_fill = "lightblue", color_border = "lightblue4") {
  
  intervalos_cliente <- datos_cluster %>%
    arrange(id_cliente_enc, dia) %>%
    group_by(id_cliente_enc) %>%
    mutate(intervalo = as.numeric(difftime(dia, lag(dia), units = "days"))) %>%
    filter(!is.na(intervalo)) %>%
    summarise(media_intervalo = mean(intervalo), .groups = "drop")
  
  # Boxplot
  p_box <- ggplot(intervalos_cliente, aes(y = media_intervalo)) +
    geom_boxplot(color = color_border, fill = color_fill, size = 1.2) +
    labs(
      title = paste("Distribución de la frecuencia de compra -", titulo_cluster),
      y = "Días promedio entre compras"
    ) +
    coord_cartesian(ylim = c(0, 10)) +
    theme_minimal()
  
  # Histograma
  p_hist <- ggplot(intervalos_cliente, aes(x = media_intervalo)) +
    geom_histogram(binwidth = 1, color = color_border, fill = color_fill, size = 1.2) +
    labs(
      title = paste("Media de días entre compras por cliente -", titulo_cluster),
      x = "Media de días entre compras",
      y = "Cantidad de clientes"
    ) +
    coord_cartesian(xlim = c(0, 8)) +
    theme_minimal()
  
  list(boxplot = p_box, histograma = p_hist)
}

graficos_intervalo_c1 <- graficar_intervalo_compras(cluster1, "Cluster 1")
graficos_intervalo_c2 <- graficar_intervalo_compras(cluster2, "Cluster 2")
graficos_intervalo_c3 <- graficar_intervalo_compras(cluster3, "Cluster 3")


graficar_productos_por_dia_semana <- function(datos_cluster, titulo_cluster, color_fill = "lightblue", color_border = "lightblue4") {
  
  datos_dia_semana <- datos_cluster %>%
    mutate(DiaSemana = wday(dia)) %>%
    group_by(DiaSemana) %>%
    summarise(CantidadProductos = n(), .groups = "drop")
  
  datos_dia_semana$DiaSemana <- factor(
    datos_dia_semana$DiaSemana,
    levels = 1:7,
    labels = c("Domingo", "Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado")
  )
  
  # Reordenar el factor manualmente: Lunes a Domingo
  datos_dia_semana$DiaSemana <- factor(
    datos_dia_semana$DiaSemana,
    levels = c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado", "Domingo")
  )
  
  ggplot(datos_dia_semana, aes(x = CantidadProductos, y = DiaSemana)) +
    geom_col(fill = color_fill, color = color_border, size = 1.2) +
    theme_minimal() +
    labs(
      title = paste("Cantidad de productos comprados por día de la semana -", titulo_cluster),
      x = "Cantidad de Productos",
      y = "Día de la Semana"
    )
}

grafico_dia_c1 <- graficar_productos_por_dia_semana(cluster1, "Cluster 1")
grafico_dia_c2 <- graficar_productos_por_dia_semana(cluster2, "Cluster 2")
grafico_dia_c3 <- graficar_productos_por_dia_semana(cluster3, "Cluster 3")

############
# GRAFICO 6

graficar_tickets_por_mes <- function(datos_cluster, titulo_cluster, color_fill = "lightblue", color_border = "steelblue") {
  
  # Extraer el nombre del mes en minúsculas
  tickets_por_mes <- datos_cluster %>%
    mutate(Mes = tolower(month(dia, label = TRUE, abbr = FALSE))) %>%
    group_by(Mes) %>%
    summarise(CantidadTickets = n(), .groups = "drop")
  
  # Orden correcto de los meses
  tickets_por_mes$Mes <- factor(
    tickets_por_mes$Mes,
    levels = c("enero", "febrero", "marzo", "abril", "mayo", "junio",
               "julio", "agosto", "septiembre", "octubre", "noviembre", "diciembre")
  )
  
  # Gráfico
  ggplot(tickets_por_mes, aes(x = Mes, y = CantidadTickets)) +
    geom_col(fill = color_fill, color = color_border, width = 0.7) +
    coord_cartesian(ylim = c(0, 400000)) +   # <--- Escala fija en el eje Y
    theme_minimal() +
    labs(
      title = paste("Cantidad de tickets por mes -", titulo_cluster),
      x = "Mes",
      y = "Cantidad de Tickets"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}


grafico_mes_cluster1 <- graficar_tickets_por_mes(cluster1, "Cluster 1")
grafico_mes_cluster2 <- graficar_tickets_por_mes(cluster2, "Cluster 2")
grafico_mes_cluster3 <- graficar_tickets_por_mes(cluster3, "Cluster 3")


