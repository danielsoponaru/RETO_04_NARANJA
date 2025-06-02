### ANALISIS EXPLORATORIO RETO 4 CON SHINY ###

#Instalar librarias si es necesario
if(!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}

if(!requireNamespace("lubridate", quietly = TRUE)) {
  install.packages("lubridate")
}

if(!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}

if(!requireNamespace("shiny", quietly = TRUE)) {
  install.packages("shiny")
}

if(!requireNamespace("shinyWidgets", quietly = TRUE)) {
  install.packages("shinyWidgets")
}

if(!requireNamespace("DT", quietly = TRUE)) {
  install.packages("DT")
}

if(!requireNamespace("bslib", quietly = TRUE)) {
  install.packages("bslib")
}

if(!requireNamespace("ggrepel", quietly = TRUE)) {
  install.packages("ggrepel")
}

if(!requireNamespace("plotly", quietly = TRUE)) {
  install.packages("plotly")
}

if(!requireNamespace("shinydashboard", quietly = TRUE)) {
  install.packages("shinydashboard")
}

if(!requireNamespace("shinyjs", quietly = TRUE)) {
  install.packages("shinyjs")
}

if(!requireNamespace("viridis", quietly = TRUE)) {
  install.packages("viridis")
}

if(!requireNamespace("waiter", quietly = TRUE)) {
  install.packages("waiter")
}

if(!requireNamespace("tidyr", quietly = TRUE)) {
  install.packages("tidyr")
}

#Cargar librerias necesarias
library(dplyr)
library(lubridate)
library(ggplot2)
library(shiny)
library(shinyWidgets)
library(DT)
library(bslib)
library(ggrepel)
library(plotly)
library(shinydashboard)
library(shinyjs)
library(viridis)
library(waiter)
library(tidyr)

#Cargar datos
maestroestr<- readRDS("Datos/Originales/maestroestr.RDS")
objetivos<- readRDS("Datos/Originales/objetivos.RDS")
tickets_enc<- readRDS("Datos/Originales/tickets_enc.RDS")
clientes_clusterizados<- readRDS("Datos/Transformados/clientes_clusterizados.RDS")
matriz<- readRDS("Datos/Transformados/MatrizSuperReducida.RDS")
resultadosO1<- read.csv("Resultados/resultados_objetivo1.csv")
resultadosO2<- read.csv("Resultados/resultados_objetivo2.csv")
resultadosO3<- read.csv("Resultados/resultados_objetivo3.csv")
resultadosO4<- read.csv("Resultados/resultados_objetivo4.csv")
metricasRatings<- read.csv("Comparacion de algoritmos/metricasRatings.csv")
metricasTopN<- read.csv("Comparacion de algoritmos/metricasTopN.csv")
summary_tabla<- read.csv("Clusterización/estadisticosClusteres.csv")
colnames(summary_tabla)<- c(
  "Cluster",
  "Número de clientes",
  "Productos totales por cliente",
  "Variedad de productos",
  "Días activos promedio",
  "Compras por semana",
  "Compras entre semana",
  "Compras fin de semana",
  "% compras en fin de semana"
)

options(scipen = 999)

#Ajustar tipos de columnas
tickets_enc$dia<- ymd(tickets_enc$dia)
tickets_enc$cod_est<- as.numeric(tickets_enc$cod_est)
tickets_enc$id_cliente_enc<- as.character(tickets_enc$id_cliente_enc)
maestroestr$cod_est<- as.numeric(maestroestr$cod_est)

#Paleta de colores personalizada de Eroski
paleta_eroski<- bs_theme(
  bg = "#FFFFFF",
  fg = "#333333",
  primary = "#E60026",
  secondary = "#0033A0",
  success = "#80C342",
  info = "#5B9BD5",
  warning = "#FFB900",
  danger = "#C8102E",
  font_scale = 1.1
)

#CSS personalizado
css_personalizado<- "
.metric-card {
  background: linear-gradient(135deg, #E60026, #C8102E);
  color: white;
  padding: 20px;
  border-radius: 10px;
  text-align: center;
  margin: 10px;
}
.cluster-info {
  background: linear-gradient(135deg, #0033A0, #5B9BD5);
  color: white;
  padding: 15px;
  border-radius: 8px;
  margin: 10px 0;
}
.info-note {
  background-color: #f8f9fa;
  border-left: 4px solid #E60026;
  padding: 15px;
  margin: 15px 0;
  border-radius: 5px;
  font-style: italic;
  color: #666666;
}
/* Asegurar scroll vertical en toda la aplicación */
body {
  overflow-y: auto !important;
}
/* Contenedor principal con scroll */
.main-container {
  overflow-y: auto !important;
  max-height: none !important;
}
/* Pestañas con altura automática */
.nav-content {
  min-height: auto !important;
  height: auto !important;
}
/* Cards de gráficos con altura automática */
.card-body {
  min-height: 450px !important;
  height: auto !important;
}
/* Contenedor de pestañas con scroll */
.tab-content {
  overflow-y: visible !important;
  height: auto !important;
  min-height: auto !important;
}
.tab-pane {
  height: auto !important;
  min-height: auto !important;
}
"

#Funciones auxiliares para estadisticos principales de la base de datos
calcular_estadisticos_principales<- function() {
  total_clientes<- n_distinct(tickets_enc$id_cliente_enc)
  total_productos<- n_distinct(tickets_enc$cod_est)
  total_tickets<- n_distinct(tickets_enc$num_ticket)
  periodo_analisis<- paste(min(tickets_enc$dia), "a", max(tickets_enc$dia))
  list(
    clientes = total_clientes,
    productos = total_productos,
    tickets = total_tickets,
    periodo = periodo_analisis
  )
}


#Definir la ui
ui<- page_navbar(
  title = strong("Eroski - Reto 04"),
  theme = paleta_eroski,
  useShinyjs(),
  tags$head(tags$style(HTML(css_personalizado))),
  
  #Panel de análisis exploratorio
  nav_panel(
    title = tags$span(icon("magnifying-glass-chart"), "Análisis Exploratorio"),
    #Contenedor principal con scroll
    div(
      class = "main-container",
      style = "padding: 20px; overflow-y: auto;",
      
      #Métricas principales integradas
      fluidRow(
        column(3, div(class = "metric-card", h4("Clientes Únicos"), h2(textOutput("total_clientes")))),
        column(3, div(class = "metric-card", h4("Productos"), h2(textOutput("total_productos")))),
        column(3, div(class = "metric-card", h4("Tickets"), h2(textOutput("total_tickets")))),
        column(3, div(class = "metric-card", h4("Período"), h6(textOutput("periodo_analisis"))))
      ),
      br(),
      
      #Gráficos en layout vertical
      div(
        h3("Análisis de Productos por Compra", style = "color: #E60026; margin-top: 30px;"),
        p("Este histograma refleja la cantidad de articulos que los clientes se suelen llevar por compra."),
        card(
          style = "margin-bottom: 30px;",
          plotOutput("cantidad_articulos", height = "450px")
        )
      ),
      
      div(
        h3("Artículos Más Comprados", style = "color: #E60026; margin-top: 30px;"),
        p("Este gráfico de barras muestra los 10 articulos más comprados en general."),
        card(
          style = "margin-bottom: 30px;",
          plotOutput("top10_articulos", height = "450px")
        )
      ),
      
      div(
        h3("Tiempo de Actividad de los Clientes", style = "color: #E60026; margin-top: 30px;"),
        p("En este grafico se ve el tiempo de actividad de los clientes en Eroski, es decir la diferencia entre su primera y última compra."),
        card(
          style = "margin-bottom: 30px;",
          plotOutput("actividad_clientes", height = "450px")
        )
      ),
      
      div(
        h3("Frecuencia de Compra", style = "color: #E60026; margin-top: 30px;"),
        p("Este boxplot muestra la distribucion de la frecuencia de compra de los clientes. Es decir cuantos días pasan de media entre compra y compra."),
        card(
          style = "margin-bottom: 30px;",
          plotOutput("frecuencia_de_compra", height = "450px")
        )
      ),
      
      div(
        h3("Día de la Semana que Más se Compra", style = "color: #E60026; margin-top: 30px;"),
        p("En este gráfico se ven los días de la semana en los que más suelen comprar los clienes de Eroski."),
        card(
          style = "margin-bottom: 30px;",
          plotOutput("dias_semana", height = "450px")
        )
      ),
      
      div(
        h3("Productos Más Comprados por Meses", style = "color: #E60026; margin-top: 30px;"),
        p("En este gráfico se puede ver cuales son los productos más frecuentes cada mes. Teniendo en cuenta que la base de datos solo consta de tres meses."),
        card(
          style = "margin-bottom: 30px;",
          plotOutput("productos_mas_frecuentes_mes", height = "450px")
        )
      ),
      
      div(
        h3("Evolución Mensual del Número de Compras", style = "color: #E60026; margin-top: 30px;"),
        p("En este gráfico de lineas se ve la evolucion que han tenido las ventas de eroski durante estos tres meses."),
        card(
          style = "margin-bottom: 30px;",
          plotOutput("grafico_evolucion", height = "450px")
        )
      ),
      
      div(
        h3("Fidelización de los Clientes", style = "color: #E60026; margin-top: 30px;"),
        p("Este gráfico mide la fidelización de los clientes, ya que no solo refleja el tiempo de actividad, sino que también la cantidad de veces que han ido a comprar durante ese tiempo."),
        card(
          style = "margin-bottom: 30px;",
          plotOutput("grafico_fidelizacion", height = "450px")
        )
      )
    )
  ),
  
  #Panel de clústeres MODIFICADO
  nav_panel(
    title = tags$span(icon("users"), "Análisis de Clústeres"),
    navset_card_tab(
      
      #Página principal con características generales
      nav_panel(
        title = "Características Generales",
        div(
          style = "padding: 20px;",
          h3("Características Generales de los Clústeres", style = "color: #E60026; margin-bottom: 20px;"),
          p("Este gráfico muestra las principales características que definen cada cluster de clientes."),
          card(
            card_header("Comparación de Características por Cluster"),
            plotOutput("grafico_clusters_general", height = "500px")
          )
        )
      ),
      
      #Pestañas individuales para cada cluster
      nav_panel(
        title = "Cluster 1",
        div(
          style = "padding: 20px;",
          div(
            class = "cluster-info",
            h4("Cluster 1 - Clientes muy activos y fieles"),
            textOutput("descripcion_cluster1")
          ),
          card(
            card_header("Estadísticas Detalladas - Cluster 1"),
            DTOutput("tabla_cluster1")
          )
        )
      ),
      
      nav_panel(
        title = "Cluster 2", 
        div(
          style = "padding: 20px;",
          div(
            class = "cluster-info",
            h4("Cluster 2 - Clientes regulares y equilibrados"),
            textOutput("descripcion_cluster2")
          ),
          card(
            card_header("Estadísticas Detalladas - Cluster 2"),
            DTOutput("tabla_cluster2")
          )
        )
      ),
      
      nav_panel(
        title = "Cluster 3",
        div(
          style = "padding: 20px;",
          div(
            class = "cluster-info", 
            h4("Cluster 3 - Clientes ocasionales pero concentrados"),
            textOutput("descripcion_cluster3")
          ),
          card(
            card_header("Estadísticas Detalladas - Cluster 3"),
            DTOutput("tabla_cluster3")
          )
        )
      )
    )
  ),
  
  #Panel de modelado
  nav_panel(
    title = tags$span(icon("brain"), "Modelado y Métricas"),
    navset_card_tab(
      nav_panel("Evaluación TopNList",
                card(
                  card_header("Métricas de Evaluación - TopNList"),
                  DTOutput("tabla_metricas_topNList"),
                  plotOutput("grafico_metricas_topNList")
                )
      ),
      nav_panel("Evaluación Ratings",
                card(
                  card_header("Métricas de Evaluación - Ratings"),
                  DTOutput("tabla_metricas_ratings"),
                  plotOutput("grafico_metricas_ratings")
                )
      )
    )
  ),
  
  #Panel de resultados - CON NOTA AÑADIDA
  nav_panel(
    title = tags$span(icon("trophy"), "Resultados"),
    sidebarLayout(
      sidebarPanel(
        width = 3,
        radioGroupButtons(
          inputId = "objetivo",
          label = "Selecciona el objetivo:",
          choices = c("Objetivo 1", "Objetivo 2", "Objetivo 3", "Objetivo 4"),
          selected = "Objetivo 1",
          status = "primary",
          direction = "vertical",
          size = "lg"
        ),
        # Nota añadida aquí
        div(
          class = "info-note",
          icon("info-circle"),
          " Consultar el anexo del informe para más detalles de los resultados."
        )
      ),
      mainPanel(
        width = 9,
        card(
          card_header(textOutput("titulo_resultado")),
          DTOutput("tabla_resultados")
        )
      )
    )
  )
)

#Server
server<- function(input, output, session) {
  
  #Cargar página con animación
  waiter_show(html = spin_fading_circles(), color = "#E60026")
  Sys.sleep(1)
  waiter_hide()
  
  
  #Estadisticos del dashboard principal
  metricas<- calcular_estadisticos_principales()
  
  output$total_clientes <- renderText({ format(metricas$clientes, big.mark = ",") })
  output$total_productos <- renderText({ format(metricas$productos, big.mark = ",") })
  output$total_tickets <- renderText({ format(metricas$tickets, big.mark = ",") })
  output$periodo_analisis <- renderText({ metricas$periodo })
  
  #Graficos del analisis explporatorio
  output$cantidad_articulos <- renderPlot({
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
  
  output$top10_articulos <- renderPlot({
    top_articulos <- tickets_enc %>% 
      group_by(cod_est) %>% 
      summarise(Cantidad = n(), .groups = "drop") %>%
      arrange(desc(Cantidad)) %>%
      head(10) %>%
      left_join(maestroestr, by = "cod_est")
    
    ggplot(top_articulos, aes(x = Cantidad, y = reorder(descripcion, Cantidad))) +
      geom_col(fill = "#E60026", color = "#E60026", alpha = 0.6) +
      labs(title = "Top 10 artículos más comprados",
           x = "Cantidad de veces comprado",
           y = "Artículo") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$actividad_clientes <- renderPlot({
    tickets_enc3 <- tickets_enc %>%
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
  
  output$frecuencia_de_compra <- renderPlot({
    intervalos<- tickets_enc %>%
      arrange(id_cliente_enc, dia) %>%
      group_by(id_cliente_enc) %>%
      mutate(intervalo = as.numeric(difftime(dia, lag(dia), units = "days"))) %>%
      filter(!is.na(intervalo)) %>%
      summarise(media_intervalo = mean(intervalo), .groups = "drop")
    
    ggplot(intervalos, aes(y = media_intervalo)) +
      geom_boxplot(color = "#E60026",fill = "#E60026", alpha = 0.6) +
      labs( title = "Distribución de la frecuencia de compra de los clientes",
            y = "Días promedio entre compras") +
      coord_cartesian(ylim = c(0, 10)) + 
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
    tickets_mes <- tickets_enc %>% 
      mutate(mes = floor_date(dia, "day")) %>%
      count(mes)
    
    ggplot(tickets_mes, aes(x = mes, y = n)) +
      geom_line(color = "#E60026", size = 1) +
      labs(title = "Evolución mensual del número de compras",
           x = "Mes", y = "Número de tickets") +
      theme_minimal()
  })
  
  output$grafico_fidelizacion<- renderPlot({
    tickets_enc4 <- tickets_enc %>%
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
  
  
  #Panel de clusteres
  
  #Gráfico general de comparación de clusters
  output$grafico_clusters_general <- renderPlot({
    matriz_df<- matriz %>%
      as.data.frame() %>%
      mutate(id_cliente_enc = rownames(.))
    
    tickets_enc<- tickets_enc %>%
      mutate(
        num_ticket = as.character(num_ticket),
        dia = ymd(dia),
        num_ticket = paste(num_ticket, id_cliente_enc),
        DiaSemana = wday(dia, week_start = 1)
      )
    
    datos_clientes<- tickets_enc %>%
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
    
    matriz_con_cluster<- matriz_df %>%
      inner_join(clientes_clusterizados, by = "id_cliente_enc") %>%
      left_join(datos_clientes, by = "id_cliente_enc")
    
    variables<- c("total_productos", "productos_distintos", "dias_activos", 
                  "compras_por_semana", "compras_entre_semana", "compras_fin_de_semana")
    
    promedios_long<- matriz_con_cluster %>%
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
      scale_fill_manual(values = c("#E60026", "#0033A0", "#80C342")) +
      theme_minimal()
    
  })
  
  #Descripciones de clusters
  output$descripcion_cluster1 <- renderText({
    paste(
      "Este grupo representa a los clientes más intensos, con un alto número de productos comprados y gran diversidad. Compran con mucha frecuencia y son muy activos a lo largo del tiempo, aunque principalmente entre semana. Son clientes fieles y de gran valor para Eroski.",
      "Total de clientes:",summary_tabla[1,2])
  })
  
  output$descripcion_cluster2 <- renderText({
    paste(
      "Formado por consumidores con una actividad moderada, tanto en variedad como en frecuencia de compra. Compran regularmente durante el año y combinan sus compras entre semana y fines de semana. Representan un segmento estable y predecible.",
      "Total de clientes:", summary_tabla[2,2])
  })
  
  output$descripcion_cluster3 <- renderText({
    paste(
      "Este grupo es el más pequeño y menos activo en términos de días de compra, pero con una frecuencia semanal relativamente alta. Compran menos productos y variedad, pero lo hacen de forma más intensiva en periodos cortos. Podrían representar oportunidades puntuales o compradores estacionales.",
      "Total de clientes:", summary_tabla[3,2])
  })
  
  #Tablas detalladas por cluster
  output$tabla_cluster1<- renderDT({
    summary_tabla %>% filter(Cluster == 1)
  })
  
  output$tabla_cluster2<- renderDT({
    summary_tabla %>% filter(Cluster == 2)
  })
  
  output$tabla_cluster3<- renderDT({
    summary_tabla %>% filter(Cluster == 3)
  })
  
  output$titulo_resultado<- renderText({
    titulos <- c(
      "Objetivo 1" = "Resultados - Articulo Promocionado",
      "Objetivo 2" = "Resultados - Otros como tú han comprado",
      "Objetivo 3" = "Resultados - Oferta para ti",
      "Objetivo 4" = "Resultados - Quizás te hayas olvidado"
    )
    titulos[[input$objetivo]]
  })
  
  #Tabla de resultados de objetivos  
  output$tabla_resultados<- renderDT({
    tabla<- switch(input$objetivo,
                   "Objetivo 1" = resultadosO1,
                   "Objetivo 2" = resultadosO2,
                   "Objetivo 3" = resultadosO3,
                   "Objetivo 4" = resultadosO4
    )
    
    datatable(tabla, 
              options = list(pageLength = 10, dom = 'tip'),
              class = 'cell-border stripe') %>%
      formatStyle(columns = colnames(tabla), backgroundColor = '#f8f9fa')
  })
  
  #Tablas de métricas de modelado
  output$tabla_metricas_topNList<- renderDT({
    datatable(metricasTopN, 
              options = list(pageLength = 10, dom = 'tip'),
              class = 'cell-border stripe') %>%
      formatRound(columns = c("Precision", "Recall", "TPR", "FPR"), digits = 4)
  })
  
  output$tabla_metricas_ratings<- renderDT({
    datatable(metricasRatings,
              options = list(pageLength = 10, dom = 'tip'),
              class = 'cell-border stripe') %>%
      formatRound(columns = c("RMSE", "MAE", "MSE"), digits = 4)
  })
  
  output$grafico_metricas_topNList<- renderPlot({
    metricasTopN_larga<- pivot_longer(metricasTopN,
                                      cols = c("Precision", "Recall", "TPR", "FPR"),
                                      names_to = "Metrica",
                                      values_to = "Valor")
    
    ggplot(metricasTopN_larga, aes(x = Modelo, y = Valor, fill = Metrica)) +
      geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
      labs(title = paste("Evaluación de modelos por métricas (Top 10)"),
           y = "Valor de la métrica",
           x = "Modelo",
           fill = "Métrica") +
      theme_minimal(base_size = 14) +
      scale_fill_manual(values = c("Precision" = "#E60026",
                                   "Recall" = "#0033A0",
                                   "TPR" = "#80C342",
                                   "FPR" = "#FFB900")) +
      theme(legend.position = "top")
    
  })
  
  output$grafico_metricas_ratings<- renderPlot({
    metricasRatings_larga<- pivot_longer(metricasRatings,
                                         cols = c("RMSE", "MAE", "MSE"),
                                         names_to = "Metrica",
                                         values_to = "Valor")
    
    ggplot(metricasRatings_larga, aes(x = Modelo, y = Valor, fill = Metrica)) +
      geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
      scale_y_log10() + 
      labs(title = "Comparación de errores por modelo (Ratings)",
           y = "Valor del error (escala logarítmica)",
           x = "Modelo",
           fill = "Métrica") +
      theme_minimal(base_size = 14) +
      scale_fill_manual(values = c("RMSE" = "#E60026", "MAE" = "#0033A0", "MSE" = "#80C342")) +
      theme(legend.position = "top")
  })
  
}

#Ejecutar la aplicación
shinyApp(ui = ui, server = server)

