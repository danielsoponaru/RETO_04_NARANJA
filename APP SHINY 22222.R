### ANALISIS EXPLORATORIO RETO 4 CON SHINY ###

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

#Cargar datos
maestroestr<- readRDS("maestroestr.RDS")
objetivos<- readRDS("objetivos.RDS")
tickets_enc<- readRDS("tickets_enc.RDS")
clientes_clusterizados<- readRDS("clientes_clusterizados.RDS")
matriz<- readRDS("MatrizSuperReducida.RDS")
resultadosO2<- read.csv("Resultados/resultados_objetivo2.csv")
resultadosO3<- read.csv("Resultados/resultados_objetivo3.csv")
resultadosO4<- read.csv("Resultados/resultados_objetivo4.csv")
options(scipen = 999)

#Ajustar tipos de columnas
tickets_enc$dia <- ymd(tickets_enc$dia)
tickets_enc$cod_est <- as.numeric(tickets_enc$cod_est)
tickets_enc$id_cliente_enc <- as.character(tickets_enc$id_cliente_enc)
maestroestr$cod_est <- as.numeric(maestroestr$cod_est)

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
  base_font = font_google("Open Sans"),
  heading_font = font_google("Montserrat"),
  font_scale = 1.1
)

#CSS personalizado
css_personalizado<- "
.navbar-brand {
  font-weight: bold !important;
  font-size: 1.5rem !important;
}
.card {
  box-shadow: 0 4px 8px rgba(0,0,0,0.1);
  border-radius: 10px;
  margin-bottom: 20px;
}
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

#Funciones auxiliares para métricas
calcular_metricas_resumen <- function() {
  total_clientes <- n_distinct(tickets_enc$id_cliente_enc)
  total_productos <- n_distinct(tickets_enc$cod_est)
  total_tickets <- n_distinct(tickets_enc$num_ticket)
  periodo_analisis <- paste(min(tickets_enc$dia), "a", max(tickets_enc$dia))
  
  list(
    clientes = total_clientes,
    productos = total_productos,
    tickets = total_tickets,
    periodo = periodo_analisis
  )
}

#Definir la ui
ui<- page_navbar(
  title = div(
    img(src = "logo_eroski.png", 
        height = "30px", style = "margin-right: 10px;"),
    "Eroski - Reto 04"
  ),
  theme = paleta_eroski,
  useShinyjs(),
  tags$head(tags$style(HTML(css_personalizado))),
  
  # Panel de análisis exploratorio mejorado con scroll
  nav_panel(
    title = tags$span(icon("magnifying-glass-chart"), "Análisis Exploratorio"),
    # Contenedor principal con scroll
    div(
      class = "main-container",
      style = "padding: 20px; overflow-y: auto;",
      
      # Métricas principales integradas
      fluidRow(
        column(3, div(class = "metric-card", h4("Clientes Únicos"), h2(textOutput("total_clientes")))),
        column(3, div(class = "metric-card", h4("Productos"), h2(textOutput("total_productos")))),
        column(3, div(class = "metric-card", h4("Tickets"), h2(textOutput("total_tickets")))),
        column(3, div(class = "metric-card", h4("Período"), h6(textOutput("periodo_analisis"))))
      ),
      br(),
      
      # Gráficos en layout vertical con cards individuales para mejor scroll
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
  
  #Panel de clústeres mejorado
  nav_panel(
    title = tags$span(icon("users"), "Análisis de Clústeres"),
    sidebarLayout(
      sidebarPanel(
        width = 3,
        selectizeInput(
          inputId = "cluster",
          label = "Selecciona el clúster:",
          choices = sort(unique(clientes_clusterizados$kmeans_cluster)),
          selected = 1,
          options = list(
            placeholder = "Elige un clúster...",
            onInitialize = I('function() { this.setValue(""); }')
          )
        ),
        br(),
        div(class = "cluster-info",
            h4("Información del Clúster"),
            textOutput("cluster_size"),
            br(),
            textOutput("cluster_description")
        )
      ),
      mainPanel(
        width = 9,
        conditionalPanel(
          condition = "input.cluster != ''",
          fluidRow(
            column(6, 
                   card(
                     card_header("Productos Favoritos del Clúster"),
                     plotlyOutput("cluster_productos")
                   )
            ),
            column(6,
                   card(
                     card_header("Comportamiento de Compra"),
                     plotlyOutput("cluster_comportamiento")
                   )
            )
          ),
          fluidRow(
            column(12,
                   card(
                     card_header("Clientes del Clúster"),
                     DTOutput("cluster_clientes")
                   )
            )
          )
        ),
        conditionalPanel(
          condition = "input.cluster == ''",
          div(
            style = "text-align: center; margin-top: 100px;",
            icon("info-circle", style = "font-size: 48px; color: #E60026;"),
            h3("Selecciona un clúster para ver el análisis detallado", style = "color: #666;")
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
                  DTOutput("tabla_metricas_topNList")
                )
      ),
      nav_panel("Evaluación Ratings",
                card(
                  card_header("Métricas de Evaluación - Ratings"),
                  DTOutput("tabla_metricas_ratings")
                )
      ),
      nav_panel("Matriz de Recomendación",
                card(
                  card_header("Visualización de la Matriz"),
                  p("Análisis de la matriz de recomendación utilizada en el modelo."),
                  verbatimTextOutput("info_matriz")
                )
      )
    )
  ),
  
  #Panel de resultados
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
        br(),
        div(
          style = "background: #f8f9fa; padding: 15px; border-radius: 8px;",
          h5("Descripción del Objetivo"),
          textOutput("descripcion_objetivo")
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
  
  # Cargar página con animación
  waiter_show(html = spin_fading_circles(), color = "#E60026")
  Sys.sleep(1)
  waiter_hide()
  
  # Métricas del dashboard principal
  metricas <- calcular_metricas_resumen()
  
  output$total_clientes <- renderText({ format(metricas$clientes, big.mark = ",") })
  output$total_productos <- renderText({ format(metricas$productos, big.mark = ",") })
  output$total_tickets <- renderText({ format(metricas$tickets, big.mark = ",") })
  output$periodo_analisis <- renderText({ metricas$periodo })
  
  #Gráficos principales mejorados con plotly
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
  
  output$dias_semana <- renderPlot({
    tickets_encF <- tickets_enc %>% mutate(DiaSemana = wday(dia, label = TRUE, abbr = FALSE, week_start = 1)) %>% group_by(DiaSemana) %>% summarise(CantidadProductos = n())
    
    tickets_encF2 <- tickets_encF %>%
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
  
  output$productos_mas_frecuentes_mes <- renderPlot({
    tickets_enc6 <- tickets_enc %>%
      left_join(maestroestr, by = "cod_est")
    
    productos_mes <- tickets_enc6 %>%
      mutate(Mes = month(dia, label = TRUE)) %>%
      group_by(Mes, descripcion, id_cliente_enc) %>%
      summarise(total = n(), .groups = "drop")
    
    top_productos <- productos_mes %>%
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
  
  output$grafico_evolucion <- renderPlot({
    tickets_mes <- tickets_enc %>% 
      mutate(mes = floor_date(dia, "day")) %>%
      count(mes)
    
    ggplot(tickets_mes, aes(x = mes, y = n)) +
      geom_line(color = "#E60026", size = 1) +
      labs(title = "Evolución mensual del número de compras",
           x = "Mes", y = "Número de tickets") +
      theme_minimal()
  })
  
  output$grafico_fidelizacion <- renderPlot({
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
  
  #Análisis de clústeres
  output$cluster_size <- renderText({
    if(input$cluster != "") {
      size <- clientes_clusterizados %>%
        filter(kmeans_cluster == input$cluster) %>%
        nrow()
      paste("Tamaño del clúster:", format(size, big.mark = ","), "clientes")
    }
  })
  
  output$cluster_description <- renderText({
    descripciones <- c(
      "1" = "Clientes ocasionales con baja frecuencia de compra",
      "2" = "Clientes regulares con comportamiento estándar",
      "3" = "Clientes frecuentes con alta fidelidad",
      "4" = "Clientes premium con alto valor de compra"
    )
    if(input$cluster != "" && input$cluster %in% names(descripciones)) {
      descripciones[[input$cluster]]
    }
  })
  
  #Información de la matriz
  output$info_matriz<- renderText({
    if(exists("matriz")) {
      paste("Dimensiones de la matriz:", paste(dim(matriz), collapse = " x "),
            "\nTipo de objeto:", class(matriz)[1],
            "\nMemoria utilizada:", format(object.size(matriz), units = "Mb"))
    } else {
      "Matriz no disponible"
    }
  })
  
  #Descripciones de objetivos
  output$descripcion_objetivo<- renderText({
    descripciones<- c(
      "obj1" = "Artículo promocionado",
      "obj2" = "Otros como tú han comprado",
      "obj3" = "Oferta para ti",
      "obj4" = "Quizás te hayas olvidado"
    )
    descripciones[[input$objetivo]]
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
  
  #Tabla de resultados
  output$tabla_resultados<- renderDT({
    tabla<- switch(input$objetivo,
                   "Objetivo 1" = data.frame(Métrica = c("Precisión", "Recall", "F1-Score"), 
                                             Valor = c(0.85, 0.78, 0.81)),
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
    datos_ejemplo <- data.frame(
      Algoritmo = c("UBCF", "IBCF", "Popular", "Random"),
      Precision = c(0.15, 0.12, 0.08, 0.02),
      Recall = c(0.25, 0.20, 0.15, 0.05),
      F1 = c(0.19, 0.15, 0.11, 0.03)
    )
    
    datatable(datos_ejemplo, 
              options = list(pageLength = 10, dom = 'tip'),
              class = 'cell-border stripe') %>%
      formatRound(columns = c('Precision', 'Recall', 'F1'), digits = 3)
  })
  
  output$tabla_metricas_ratings <- renderDT({
    datos_ejemplo <- data.frame(
      Algoritmo = c("SVD", "NMF", "Baseline", "KNN"),
      RMSE = c(0.95, 1.02, 1.15, 1.08),
      MAE = c(0.75, 0.82, 0.89, 0.85)
    )
    
    datatable(datos_ejemplo,
              options = list(pageLength = 10, dom = 'tip'),
              class = 'cell-border stripe') %>%
      formatRound(columns = c('RMSE', 'MAE'), digits = 3)
  })
}

#Ejecutar la aplicación
shinyApp(ui = ui, server = server)

