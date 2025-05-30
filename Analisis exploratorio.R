### ANALISIS EXPLORATORIO RETO 4 ###

#Instalar las librerías necesarias
if(!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}

if(!requireNamespace("lubridate", quietly = TRUE)) {
  install.packages("lubridate")
}

if(!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}

if(!requireNamespace("ggrepel", quietly = TRUE)) {
  install.packages("ggrepel")
}

if(!requireNamespace("tidyr", quietly = TRUE)) {
  install.packages("tidyr")
}

#Cargar librarias necesarias
library(dplyr)
library(lubridate)
library(ggplot2)
library(ggrepel)
library(tidyr)

#Cargar datos
maestroestr<-readRDS("maestroestr.RDS")
objetivos<-readRDS("objetivos.RDS")
tickets_enc<-readRDS("tickets_enc.RDS")

maestroestr #codigos de productos
tickets_enc #productos que ha adquirido cada cliente

options(scipen = 999)

#Examinar datos y tipos de columnas
str(maestroestr)
str(tickets_enc)

summary(maestroestr)
summary(tickets_enc)

colnames(maestroestr)
colnames(tickets_enc)

dim(maestroestr)
dim(tickets_enc)

#Ajustar tipos de columnas
tickets_enc$dia<- ymd(tickets_enc$dia)
tickets_enc$cod_est<- as.numeric(tickets_enc$cod_est)
tickets_enc$id_cliente_enc<- as.character(tickets_enc$id_cliente_enc)
maestroestr$cod_est<- as.numeric(maestroestr$cod_est)

#Comprobar
str(tickets_enc) #todos los tipos de columnas son correctos

#GRAFICO 1: Histograma de cuantos articulos se suelen llevar en cada compra (por ticket)
tickets_enc1<- tickets_enc %>% 
  group_by(num_ticket, id_cliente_enc) %>% 
  summarise(ArticulosPorCompra = n()) %>% 
  arrange(desc(ArticulosPorCompra))

grafico1<- ggplot(tickets_enc1, aes(x = ArticulosPorCompra)) +
  geom_histogram(color = "#E60026", fill = "#E60026", binwidth = 1, alpha = 0.6) +
  labs(title = "Cantidad de articulos que se llevan por compra",
       y = "Cantidad de tickets",
       x = "Articulos por ticket(compra)") +
  theme_minimal()


#GRAFICO 2: Articulos que más veces se compran 
tickets_enc2<- tickets_enc %>% 
  group_by(cod_est) %>% 
  summarise(Cantidad = n()) %>% 
  arrange(desc(Cantidad)) %>% 
  head(10)

tickets_con_nombres<- tickets_enc2 %>%
  left_join(maestroestr, by = "cod_est")

grafico2<- ggplot(tickets_con_nombres, aes(x = Cantidad, y = reorder(descripcion, Cantidad))) +
  geom_col(fill = "#E60026", color = "#E60026", alpha = 0.6) +
  labs(title = "Top 10 artículos más comprados",
       x = "Cantidad de veces comprado",
       y = "Artículo") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#GRAFICO 3: cuanto tiempo ha estado un cliente comprando en Eroski (diferencia entre la primera compra y la ultima)
tickets_enc3<- tickets_enc %>%
  group_by(id_cliente_enc) %>%
  summarise(primera_compra = min(dia),
            ultima_compra = max(dia)) %>%
  mutate(duracion_dias = as.numeric(ultima_compra - primera_compra)) %>%
  arrange(desc(duracion_dias))

grafico3<- ggplot(tickets_enc3, aes(x = duracion_dias)) +
  geom_histogram(binwidth = 10, fill = "#E60026", color = "#E60026", alpha = 0.6) +
  scale_x_continuous(breaks = seq(0, 90, by = 10)) +
  labs(title = "Tiempo de actividad de los clientes",
       x = "Duración entre primera y última compra (días)",
       y = "Cantidad de clientes") +
  theme_minimal()


#GRAFICO 4: Fidelización de los clientes, numero de compras vs. tiempo de actividad
tickets_enc4<- tickets_enc %>%
  group_by(id_cliente_enc) %>%
  summarise(primera = min(dia),
            ultima = max(dia),
            n_compras = n_distinct(num_ticket)) %>%
  mutate(duracion = as.numeric(ultima - primera))

grafico4<- ggplot(tickets_enc4, aes(x = duracion, y = n_compras)) +
  geom_point(alpha = 0.5, color = "#E60026") +
  labs(title = "Compras vs Tiempo de Actividad de los Clientes",
       x = "Días entre primera y última compra",
       y = "Cantidad de compras") +
  theme_minimal()


#GRAFICO 5: Intervalo promedio entre compras por cliente
tickets_enc5<- tickets_enc %>%
  arrange(id_cliente_enc, dia) %>%
  group_by(id_cliente_enc) %>%
  mutate(intervalo = as.numeric(difftime(dia, lag(dia), units = "days"))) %>%
  filter(!is.na(intervalo)) %>%
  summarise(media_intervalo = mean(intervalo)) %>%
  ungroup()

grafico5b<- ggplot(tickets_enc5, aes(y = media_intervalo)) +
  geom_boxplot(color = "#E60026",fill = "#E60026", alpha = 0.6) +
  labs( title = "Distribución de la frecuencia de compra de los clientes",
        y = "Días promedio entre compras") +
  coord_cartesian(ylim = c(0, 10)) + 
  theme_minimal()

grafico5h<- ggplot(tickets_enc5, aes(x = media_intervalo)) +
  geom_histogram(binwidth = 1, color = "#E60026", fill = "#E60026", alpha = 0.6) +
  labs(title = "Media de dias entre compras por cliente",
       x = "Media de dias entre compras",
       y = "Cantidad de clientes") +
  coord_cartesian(xlim = c(0, 8)) +
  theme_minimal()


#GRAFICO 6: Días de la semana que más se compra 
tickets_enc6<- tickets_enc %>% mutate(DiaSemana = wday(dia, label = TRUE, abbr = FALSE, week_start = 1)) %>% group_by(DiaSemana) %>% summarise(CantidadProductos = n())

#Calcular porcentaje y etiqueta
tickets_enc6<- tickets_enc6 %>%
  mutate(
    porcentaje = CantidadProductos / sum(CantidadProductos) * 100,
    etiqueta = paste0(DiaSemana, " (", round(porcentaje, 1), "%)")
  )

grafico6<- ggplot(tickets_enc6, aes(x = "", y = CantidadProductos, fill = DiaSemana)) +
  geom_col(color = "white") +
  coord_polar(theta = "y") +
  theme_void() +
  labs(title = "Proporción de productos comprados por día de la semana") +
  geom_label_repel(aes(label = etiqueta),
                   position = position_stack(vjust = 0.5), 
                   show.legend = FALSE,
                   segment.color = "grey50", size = 4) +
  scale_fill_manual(values = c("#E60026","#0033A0","#80C342","#5B9BD5","#666666","#FFB900","#0072CE"))


#GRAFICO 7: Productos más comprados por meses
tickets_enc7<- tickets_enc %>% mutate(DiaSemana = wday(dia, label = TRUE, abbr = FALSE, week_start = 1)) %>% group_by(DiaSemana) %>% summarise(CantidadProductos = n())

#Calcular porcentaje y etiqueta
tickets_enc7<- tickets_enc %>%
  left_join(maestroestr, by = "cod_est")

productos_mes<- tickets_enc7 %>%
  mutate(Mes = month(dia, label = TRUE)) %>%
  group_by(Mes, descripcion, id_cliente_enc) %>%
  summarise(total = n(), .groups = "drop")

top_productos<- productos_mes %>%
  group_by(Mes) %>%
  slice_max(order_by = total, n = 5)

grafico7<- ggplot(top_productos, aes(x = reorder(descripcion, total), y = total, fill = descripcion)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ Mes, scales = "free_x") +
  scale_fill_manual(values = c("#E60026", "#0033A0", "#80C342", "#5B9BD5", "#666666",
                               "grey", "#FFB900", "#0072CE", "#C8102E", "grey3",
                               "#A1D490", "#FFA07A", "#9ACD32", "#A0522D", "yellow4", "#4B0082")) +
  labs(title = "Top productos más comprados por mes",
       x = "Producto",
       y = "Número de tickets") +
  theme_minimal() +
  coord_flip()

#GRAFICO 8 : Evolucion de la cantidad de compras por mes
tickets_mes<- tickets_enc %>% 
  mutate(mes = floor_date(dia, "day")) %>%
  count(mes)

grafico8<- ggplot(tickets_mes, aes(x = mes, y = n)) +
  geom_line(color = "#E60026", size = 1) +
  labs(title = "Evolución mensual del número de compras",
       x = "Mes", y = "Número de tickets") +
  theme_minimal()


################## GUARDAR LOS GRAFICOS ################## 
ggsave("Graficos/Grafico1.png", plot = grafico1, width = 8, height = 6, dpi = 300, bg = "white")
ggsave("Graficos/Grafico2.png", plot = grafico2, width = 8, height = 6, dpi = 300, bg = "white")
ggsave("Graficos/Grafico3.png", plot = grafico3, width = 8, height = 6, dpi = 300, bg = "white")
ggsave("Graficos/Grafico4.png", plot = grafico4, width = 8, height = 6, dpi = 300, bg = "white")
ggsave("Graficos/Grafico5h.png", plot = grafico5h, width = 8, height = 6, dpi = 300, bg = "white")
ggsave("Graficos/Grafico5b.png", plot = grafico5b, width = 8, height = 6, dpi = 300, bg = "white")
ggsave("Graficos/Grafico6.png", plot = grafico6, width = 8, height = 6, dpi = 300, bg = "white")
ggsave("Graficos/Grafico7.png", plot = grafico7, width = 8, height = 6, dpi = 300, bg = "white")
ggsave("Graficos/Grafico8.png", plot = grafico8, width = 8, height = 6, dpi = 300, bg = "white")

