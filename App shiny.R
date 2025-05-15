### APP SHINY ###

#Cargar librerias necesarias
library(dplyr)
library(lubridate)
library(ggplot2)
library(shiny)
library(tidyr)
library(GGally)
library(lubridate)
library(readr)

#Cargar datos
maestroestr<- readRDS("maestroestr.RDS")
objetivos<- readRDS("objetivos.RDS")
tickets_enc<- readRDS("tickets_enc.RDS")
clientes_clusterizados<- readRDS("clientes_clusterizados.RDS")
matriz<- readRDS("matriz.RDS")

maestroestr #codigos de productos
tickets_enc #productos que ha adquirido cada cliente
clientes_clusterizados #cluster al que pertenece cada cliente
matriz #matriz de recomendaciones

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

#Preprocesamiento de tickets
tickets_enc <- tickets_enc %>%
  mutate(
    num_ticket = as.character(num_ticket),
    dia = ymd(dia),
    num_ticket = paste(num_ticket, id_cliente_enc),
    DiaSemana = wday(dia, week_start = 1)
  )

#Agregar las variables de comportamiento por cliente
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

#Reconstruir matriz con clusters + variables
matriz_df <- matriz %>%
  as.data.frame() %>%
  mutate(id_cliente_enc = rownames(.))

#Unir los clusters y las variables de comportamiento por cliente
matriz_con_cluster <- matriz_df %>%
  inner_join(clientes_clusterizados, by = "id_cliente_enc") %>%
  left_join(datos_clientes, by = "id_cliente_enc")



#Definir la ui
ui<- fluidPage(
  titlePanel("App Eroski - Recomendadores"),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Análisis exploratorio",
               h3("Gráficos de análisis exploratorio"),
               plotOutput("grafico1"),
               plotOutput("grafico2"),
               plotOutput("grafico3b"),
               plotOutput("grafico3h"),
               plotOutput("grafico4b"),
               plotOutput("grafico4h"),
               plotOutput("grafico5"),
               plotOutput("grafico6")
      ),
      
      tabPanel("Clústeres",
               h3("Análisis por Clúster"),
               selectInput("cluster_selector", "Selecciona cluster:", choices = NULL),  #Se llenará en el server
               tabsetPanel(
                 tabPanel("Boxplots", plotOutput("boxplots")),
                 tabPanel("Perfiles promedio", plotOutput("perfiles")),
                 tabPanel("Correlaciones", plotOutput("correlaciones")),
                 tabPanel("Top clientes", tableOutput("top_clientes")),
                 tabPanel("Resumen extendido", tableOutput("resumen_extendido"))
               )
      ),
      
      tabPanel("Modelado",
               h3("Métricas de recomendadores"),
               #Aquí puedes agregar tus métricas personalizadas más adelante
               plotOutput("grafico_metricas")  # Placeholder
      ),
      
      tabPanel("Resultados",
               h3("Resultados del modelado"),
               tableOutput("tabla_objetivos")
      )
    )
  )
)


#Server
server<- server <- function(input, output, session) {
  
  #ANALISIS
  #Gráfico 1
  output$grafico1 <- renderPlot({
    tickets_enc %>% 
      group_by(num_ticket, id_cliente_enc) %>% 
      summarise(ArticulosPorCompra = n(), .groups = "drop") %>%
      ggplot(aes(x = ArticulosPorCompra)) +
      geom_histogram(color = "lightblue4", fill = "lightblue", binwidth = 1) +
      labs(title = "Cantidad de artículos que se llevan por compra",
           y = "Cantidad de tickets",
           x = "Artículos por ticket") +
      theme_minimal()
  })
  
  #Gráfico 2
  output$grafico2 <- renderPlot({
    top_articulos <- tickets_enc %>% 
      group_by(cod_est) %>% 
      summarise(Cantidad = n(), .groups = "drop") %>%
      arrange(desc(Cantidad)) %>%
      head(10) %>%
      left_join(maestroestr, by = "cod_est")
    
    ggplot(top_articulos, aes(x = reorder(descripcion, Cantidad), y = Cantidad)) +
      geom_col(fill = "lightblue", color = "lightblue4") +
      labs(title = "Top 10 artículos más comprados",
           x = "Artículo",
           y = "Cantidad de veces comprado") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  #Gráfico 3b y 3h
  output$grafico3b <- renderPlot({
    duraciones <- tickets_enc %>%
      group_by(id_cliente_enc) %>%
      summarise(
        primera_compra = min(dia),
        ultima_compra = max(dia),
        duracion_dias = as.numeric(ultima_compra - primera_compra),
        .groups = "drop"
      )
    
    ggplot(duraciones, aes(y = duracion_dias)) +
      geom_boxplot(fill = "lightblue", color = "lightblue4") +
      labs(title = "Distribución del tiempo de actividad de los clientes",
           y = "Duración entre primera y última compra (días)") +
      theme_minimal()
  })
  
  output$grafico3h <- renderPlot({
    duraciones <- tickets_enc %>%
      group_by(id_cliente_enc) %>%
      summarise(
        primera_compra = min(dia),
        ultima_compra = max(dia),
        duracion_dias = as.numeric(ultima_compra - primera_compra),
        .groups = "drop"
      )
    
    ggplot(duraciones, aes(x = duracion_dias)) +
      geom_histogram(binwidth = 30, fill = "lightblue", color = "lightblue4") +
      labs(title = "Tiempo de actividad de los clientes",
           x = "Duración entre primera y última compra (días)",
           y = "Número de clientes") +
      theme_minimal()
  })
  
  #Gráfico 4b y 4h
  output$grafico4b <- renderPlot({
    intervalos <- tickets_enc %>%
      arrange(id_cliente_enc, dia) %>%
      group_by(id_cliente_enc) %>%
      mutate(intervalo = as.numeric(difftime(dia, lag(dia), units = "days"))) %>%
      filter(!is.na(intervalo)) %>%
      summarise(media_intervalo = mean(intervalo), .groups = "drop")
    
    ggplot(intervalos, aes(y = media_intervalo)) +
      geom_boxplot(color = "lightblue4", fill = "lightblue") +
      labs(title = "Distribución de la frecuencia de compra de los clientes",
           y = "Días promedio entre compras") +
      coord_cartesian(ylim = c(0, 10)) +
      theme_minimal()
  })
  
  output$grafico4h <- renderPlot({
    intervalos <- tickets_enc %>%
      arrange(id_cliente_enc, dia) %>%
      group_by(id_cliente_enc) %>%
      mutate(intervalo = as.numeric(difftime(dia, lag(dia), units = "days"))) %>%
      filter(!is.na(intervalo)) %>%
      summarise(media_intervalo = mean(intervalo), .groups = "drop")
    
    ggplot(intervalos, aes(x = media_intervalo)) +
      geom_histogram(binwidth = 1, color = "lightblue4", fill = "lightblue") +
      labs(title = "Media de días entre compras por cliente",
           x = "Media de días entre compras",
           y = "Cantidad de clientes") +
      coord_cartesian(xlim = c(0, 8)) +
      theme_minimal()
  })
  
  #Gráfico 5
  output$grafico5 <- renderPlot({
    tickets_enc %>%
      mutate(DiaSemana = wday(dia)) %>%
      group_by(DiaSemana) %>%
      summarise(CantidadProductos = n(), .groups = "drop") %>%
      mutate(DiaSemana = factor(DiaSemana, levels = 1:7, labels = c("Domingo", "Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado"))) %>%
      ggplot(aes(x = CantidadProductos, y = reorder(DiaSemana, CantidadProductos))) +
      geom_col(fill = "lightblue", color = "lightblue4") +
      labs(title = "Cantidad de productos comprados por día de la semana",
           x = "Cantidad de Productos",
           y = "Día de la Semana") +
      theme_minimal()
  })
  
  #CLUSTERES
  #Actualiza las opciones del selector de cluster
  observe({
    updateSelectInput(session, "cluster_selector", 
                      choices = unique(matriz_con_cluster$kmeans_cluster))
  })
  
  #Filtra los datos por cluster seleccionado
  datos_cluster <- reactive({
    req(input$cluster_selector)
    matriz_con_cluster %>% filter(kmeans_cluster == input$cluster_selector)
  })
  
  #Boxplots generales por variable
  output$boxplots <- renderPlot({
    req(matriz_con_cluster)
    variables <- c("total_productos", "productos_distintos", "dias_activos", 
                   "compras_por_semana", "compras_entre_semana", "compras_fin_de_semana")
    
    ggplot(matriz_con_cluster, aes(x = as.factor(kmeans_cluster), fill = as.factor(kmeans_cluster))) +
      geom_boxplot(aes_string(y = variables[1])) +  # Solo primer gráfico como ejemplo
      labs(title = paste("Distribución de", variables[1], "por Cluster"), x = "Cluster") +
      theme_minimal()
  })
  
  #Perfiles promedio
  output$perfiles <- renderPlot({
    req(matriz_con_cluster)
    variables <- c("total_productos", "productos_distintos", "dias_activos", 
                   "compras_por_semana", "compras_entre_semana", "compras_fin_de_semana")
    
    promedios_long <- matriz_con_cluster %>%
      group_by(kmeans_cluster) %>%
      summarise(across(all_of(variables), ~mean(.x, na.rm = TRUE))) %>%
      pivot_longer(-kmeans_cluster, names_to = "variable", values_to = "valor")
    
    ggplot(promedios_long, aes(x = variable, y = valor, fill = as.factor(kmeans_cluster))) +
      geom_col(position = "dodge") +
      coord_flip() +
      theme_minimal() +
      labs(title = "Promedio de Variables por Cluster", x = "Variable", y = "Valor Promedio")
  })
  
  #Correlaciones
  output$correlaciones <- renderPlot({
    req(datos_cluster())
    ggpairs(datos_cluster()[, c("total_productos", "productos_distintos", "dias_activos", 
                                "compras_por_semana", "compras_entre_semana", "compras_fin_de_semana")],
            title = paste("Correlaciones en Cluster", input$cluster_selector))
  })
  
  #Top clientes
  output$top_clientes <- renderTable({
    matriz_con_cluster %>%
      filter(kmeans_cluster == input$cluster_selector) %>%
      slice_max(total_productos, n = 5, with_ties = FALSE) %>%
      select(kmeans_cluster, id_cliente_enc, total_productos, compras_por_semana)
  })
  
  #Estadísticas extendidas
  output$resumen_extendido <- renderTable({
    variables <- c("total_productos", "productos_distintos", "dias_activos", 
                   "compras_por_semana", "compras_entre_semana", "compras_fin_de_semana")
    
    matriz_con_cluster %>%
      filter(kmeans_cluster == input$cluster_selector) %>%
      summarise(across(all_of(variables),
                       list(media = ~mean(.x, na.rm = TRUE),
                            mediana = ~median(.x, na.rm = TRUE),
                            sd = ~sd(.x, na.rm = TRUE)),
                       .names = "{.col}_{.fn}"))
  })
  
  #MODELADO
  #Puedes reemplazar este gráfico con tus propias métricas del modelo de recomendación
  output$grafico_metricas <- renderPlot({
    plot(1:10, 10:1, type = "b", col = "steelblue", pch = 19,
         main = "Ejemplo de Métrica de Modelado",
         xlab = "Índice", ylab = "Valor Métrico")
  })
  
  #RESULTADOS
  output$tabla_objetivos <- renderTable({
    objetivos  # Supone que es un dataframe con los resultados a mostrar
  })
}

#Ejecutar la app
shinyApp(ui = ui, server = server)

