### ANALISIS EXPLORATORIO RETO 4 CON SHINY ###

#Cargar librarias necesarias
library(dplyr)
library(lubridate)
library(ggplot2)
library(shiny)
library(shinyWidgets)
library(DT)
library(bslib)
library(ggrepel)

#Cargar datos
maestroestr<- readRDS("maestroestr.RDS")
objetivos<- readRDS("objetivos.RDS")
tickets_enc<- readRDS("tickets_enc.RDS")
clientes_clusterizados<- readRDS("clientes_clusterizados.RDS")
matriz<- readRDS("MatrizSuperReducida.RDS")
resultadosO2<- read.csv("Resultados/resultados_objetivo2.csv")

maestroestr #codigos de productos
tickets_enc #productos que ha adquirido cada cliente
clientes_clusterizados #cluster al que pertenece cada cliente
matriz #matriz de recomendacion

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

class(matriz)

#Ajustar tipos de columnas
tickets_enc$dia<- ymd(tickets_enc$dia)
tickets_enc$cod_est<- as.numeric(tickets_enc$cod_est)
tickets_enc$id_cliente_enc<- as.character(tickets_enc$id_cliente_enc)
maestroestr$cod_est<- as.numeric(maestroestr$cod_est)

str(tickets_enc)
str(maestroestr)


#=============
#APP DE SHINNY
#=============

#Definir la ui
ui<- page_navbar(
  title = "APP Eroski - Reto 04",
  nav_panel("Análisis Exploratorio",
            navset_tab(
              tabPanel("Productos por compra", 
                       p("Este histograma refleja la cantidad de articulos que los clientes se suelen llevar por compra."),
                       plotOutput("cantidad_articulos")),
              tabPanel("Articulos más comprados", 
                       p("Este gráfico de barras muestra los 10 articulos más comprados en general."),
                       plotOutput(outputId = "top10_articulos")),
              tabPanel("Tiempo de actividad de los clientes", 
                       p("En este grafico se ve el tiempo de actividad de los clientes en Eroski, es decir la diferencia entre su primera y última compra."),
                       plotOutput(outputId = "actividad_clientes")),
              tabPanel("Frecuencia de compra", 
                       p("Este gráfico muestra con que frecuencia compran los clientes de media. Es decir cuantos días pasan de media entre compra y compra."),
                       plotOutput(outputId = "frecuencia_de_compra")),
              tabPanel("Día de la semana que más se compra", 
                       p("En este gráfico se ven los días de la semana en los que más suelen comprar los clienes de Eroski."),
                       plotOutput(outputId = "dias_semana")),
              tabPanel("Productos más comprados por meses", 
                       p("En este gráfico se puede ver cuales son los productos más frecuentes cada mes. Teniendo en cuenta que la base de datos solo consta de tres meses."),
                       plotOutput(outputId = "productos_mas_frecuentes_mes")),
              tabPanel("Evolucion mensual del número de compras",
                       p("En este gráfico de lineas se ve la evolucion que han tenido las ventas de eroski durante estos tres meses."),
                       plotOutput(outputId = "grafico_evolucion")),
              tabPanel("Fidelización de los clientes",
                       p("Este gráfico mide la fidelización de los clientes, ya que no solo refleja el tiempo de actividad, sino que también la cantidad de veces que han ido a comprar durante ese tiempo."),
                       plotOutput(outputId = "grafico_fidelizacion"))
            )),
  nav_panel("Clústeres",
            selectInput(inputId = "cluster", label = "Selecciona el cluster:", choices = unique(clientes_clusterizados$kmeans_cluster), selected = 1)
            ),
  nav_panel("Modelado",
            navset_tab(
              tabPanel("topNList", 
                       p("Metricas de la evaluacion de algoritmos con topNList."),
                       DTOutput(outputId = "tabla_metricas_topNList")),
              tabPanel("Ratings", 
                       p("Metricas de la evaluacion de algoritmos con ratings."),
                       DTOutput(outputId = "tabla_metricas_ratings"))
            )),
  nav_panel("Resultados",
            radioButtons(inputId = "objetivo", label = "Selecciona el objetivo:", choices = c("Objetivo 1", "Objetivo 2", "Objetivo 3", "Objetivo 4"), selected = "Objetivo 1"),
            DTOutput(outputId = "resultados_objetivo1"),
            DTOutput(outputId = "resultados_objetivo2"),
            DTOutput(outputId = "resultados_objetivo3"),
            DTOutput(outputId = "resultados_objetivo4"))
)

#Server
server<- function(input, output) {
  
  output$cantidad_articulos<- renderPlot({
    tickets_enc %>% 
      group_by(num_ticket, id_cliente_enc) %>% 
      summarise(ArticulosPorCompra = n(), .groups = "drop") %>%
      ggplot(aes(x = ArticulosPorCompra)) +
      geom_histogram(color = "#E60026", fill = "#E60026", binwidth = 1, alpha = 0.6) +
      labs(title = "Cantidad de artículos que se llevan por compra",
           y = "Cantidad de tickets",
           x = "Artículos por ticket") +
      theme_minimal()
  })
  
  output$top10_articulos<- renderPlot({
    top_articulos<- tickets_enc %>% 
      group_by(cod_est) %>% 
      summarise(Cantidad = n(), .groups = "drop") %>%
      arrange(desc(Cantidad)) %>%
      head(10) %>%
      left_join(maestroestr, by = "cod_est")
    
    ggplot(top_articulos, aes(x = Cantidad, y = reorder(descripcion, Cantidad))) +
      geom_col(fill = "#E60026", color = "#E60026", alpha = 0.6) +
      labs(title = "Top 10 artículos más comprados",
           x = "Artículo",
           y = "Cantidad de veces comprado") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$actividad_clientes<- renderPlot({
    tickets_enc3<- tickets_enc %>%
      group_by(id_cliente_enc) %>%
      summarise(primera_compra = min(dia),
                ultima_compra = max(dia)) %>%
      mutate(duracion_dias = as.numeric(ultima_compra - primera_compra)) %>%
      arrange(desc(duracion_dias))
    
    ggplot(tickets_enc3, aes(x = duracion_dias)) +
      geom_histogram(binwidth = 10, fill = "#E60026", color = "#E60026", alpha = 0.6) +
      scale_x_continuous(breaks = seq(0, 90, by = 10)) +
      labs(title = "Tiempo de actividad de los clientes",
           x = "Duración entre primera y última compra (días)",
           y = "Cantidad de clientes") +
      theme_minimal()
  })
  
  output$frecuencia_de_compra<- renderPlot({
    intervalos<- tickets_enc %>%
      arrange(id_cliente_enc, dia) %>%
      group_by(id_cliente_enc) %>%
      mutate(intervalo = as.numeric(difftime(dia, lag(dia), units = "days"))) %>%
      filter(!is.na(intervalo)) %>%
      summarise(media_intervalo = mean(intervalo), .groups = "drop")
    
    ggplot(intervalos, aes(x = media_intervalo)) +
      geom_histogram(binwidth = 1, color = "#E60026", fill = "#E60026", alpha = 0.6) +
      labs(title = "Media de días entre compras por cliente",
           x = "Media de días entre compras",
           y = "Cantidad de clientes") +
      coord_cartesian(xlim = c(0, 8)) +
      theme_minimal()
  })
  
  output$dias_semana<- renderPlot({
    tickets_encF<- tickets_enc %>% mutate(DiaSemana = wday(dia, label = TRUE, abbr = FALSE, week_start = 1)) %>% group_by(DiaSemana) %>% summarise(CantidadProductos = n())
    
    tickets_encF2<- tickets_encF %>%
      mutate(
        porcentaje = CantidadProductos / sum(CantidadProductos) * 100,
        etiqueta = paste0(DiaSemana, " (", round(porcentaje, 1), "%)")
      )
    
    ggplot(tickets_encF2, aes(x = "", y = CantidadProductos, fill = DiaSemana)) +
      geom_col(color = "white") +
      coord_polar(theta = "y") +
      theme_void() +
      labs(title = "Proporción de productos comprados por día de la semana") +
      geom_label_repel(aes(label = etiqueta),
                       position = position_stack(vjust = 0.5), 
                       show.legend = FALSE,
                       segment.color = "grey50", size = 4) +
      scale_fill_manual(values = c( "#E60026","#0033A0","#80C342","#5B9BD5","#666666","#FFB900","#0072CE"))
    
  })
  
  output$productos_mas_frecuentes_mes<- renderPlot({
    tickets_enc6<- tickets_enc %>%
      left_join(maestroestr, by = "cod_est")
    
    productos_mes<- tickets_enc6 %>%
      mutate(Mes = month(dia, label = TRUE)) %>%
      group_by(Mes, descripcion, id_cliente_enc) %>%
      summarise(total = n(), .groups = "drop")
    
    top_productos<- productos_mes %>%
      group_by(Mes) %>%
      slice_max(order_by = total, n = 5)
    
    ggplot(top_productos, aes(x = reorder(descripcion, total), y = total, fill = descripcion)) +
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
  })
  
  output$grafico_evolucion<- renderPlot({
    tickets_mes<- tickets_enc %>% 
      mutate(mes = floor_date(dia, "month")) %>%
      count(mes)
    
    ggplot(tickets_mes, aes(x = mes, y = n)) +
      geom_line(color = "#E60026", size = 1) +
      labs(title = "Evolución mensual del número de compras",
           x = "Mes", y = "Número de tickets") +
      theme_minimal()
    
  })
  
  output$grafico_fidelizacion<- renderPlot({
    tickets_enc4<- tickets_enc %>%
      group_by(id_cliente_enc) %>%
      summarise(primera = min(dia),
                ultima = max(dia),
                n_compras = n_distinct(num_ticket)) %>%
      mutate(duracion = as.numeric(ultima - primera))
    
    ggplot(tickets_enc4, aes(x = duracion, y = n_compras)) +
      geom_point(alpha = 0.5, color = "#E60026") +
      labs(title = "Compras vs Tiempo de Actividad de los Clientes",
           x = "Días entre primera y última compra",
           y = "Cantidad de compras") +
      theme_minimal()
  })
  
  tabla_seleccionada<- reactive({
    if(input$objetivo == "Objetivo 1") {
      df1  # Dataframe para Objetivo 1
    } else if(input$objetivo == "Objetivo 2") {
      resultadosO2
    } else if(input$objetivo == "Objetivo 3") {
      df3
    } else {
      df4
    }
  })
  
  output$resultados_objetivo1<- renderDT({
    tabla_seleccionada()
  })
}

#Ejecutar la app
shinyApp(ui = ui, server = server)

