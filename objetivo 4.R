# -------------------------------------------
# CARGA DE LIBRERÍAS Y DATOS
# -------------------------------------------
library(tidyverse)
library(lubridate)
library(rsparse)

# Cargar datos
maestro <- readRDS("maestroestr.RDS")
objetivos <- readRDS("objetivos.RDS")
tickets <- readRDS("tickets_enc.RDS")
matriz <- readRDS("matriz.RDS")

# -----------------------------------------
# FILTRADO DE CLIENTES DEL OBJETIVO 4
# -----------------------------------------
obj4 <- objetivos[[4]]$obj  # IDs de los clientes objetivo

# Filtrar los tickets de esos clientes
tickets_filtrados <- tickets %>%
  filter(id_cliente_enc %in% obj4) %>%
  mutate(fecha = ymd(dia))  # convertir fecha

# -----------------------------------------
# SEPARACIÓN DEL HISTORIAL Y ÚLTIMA COMPRA
# -----------------------------------------
ultimos_tickets <- tickets_filtrados %>%
  group_by(id_cliente_enc) %>%
  filter(fecha == max(fecha)) %>%
  ungroup()

historial_tickets <- anti_join(tickets_filtrados, ultimos_tickets, by = "num_ticket")

# -----------------------------------------
# MATRIZ CLIENTE - PRODUCTO
# -----------------------------------------
tickets_matriz_agrupado <- historial_tickets %>%
  group_by(id_cliente_enc, cod_est) %>%
  summarise(N_compras = n(), .groups = "drop")

# Formato ancho sin añadir prefijo "id_"
df_matriz <- pivot_wider(
  tickets_matriz_agrupado,
  names_from = cod_est,
  values_from = N_compras,
  values_fill = 0
)

# Crear matriz dispersa
matriz_sparse_o4 <- as(as.matrix(df_matriz[,-1]), "dgCMatrix")
rownames(matriz_sparse_o4) <- df_matriz$id_cliente_enc
colnames(matriz_sparse_o4) <- colnames(df_matriz)[-1]  # mantener cod_est como nombres de columna

# -----------------------------------------
# ENTRENAR MODELO WRMF
# -----------------------------------------
modelo_wrmf_o4 <- WRMF$new(rank = 10L, lambda = 0.1, feedback = 'implicit')
modelo_wrmf_o4$fit_transform(matriz_sparse_o4, n_iter = 1000L, convergence_tol = 1e-6)

# -----------------------------------------
# GENERAR RECOMENDACIONES
# -----------------------------------------
preds_o4 <- modelo_wrmf_o4$predict(matriz_sparse_o4, k = 1)

clientes <- rownames(matriz_sparse_o4)
productos_predichos <- attr(preds_o4, "ids")

recomendaciones_o4 <- data.frame(
  id_cliente_enc = clientes,
  cod_est = productos_predichos
)

# -----------------------------------------
# UNIR CON MAESTRO PARA DESCRIPCIÓN
# -----------------------------------------
recomendaciones_o4 <- recomendaciones_o4 %>%
  left_join(maestro %>% select(cod_est, descripcion), by = "cod_est")

# -----------------------------------------
# MOSTRAR RESULTADO FINAL
# -----------------------------------------
print(recomendaciones_o4)

















# -----------------------------------------
# CARGA DE LIBRERÍAS Y DATOS
# -----------------------------------------
library(tidyverse)
library(lubridate)
library(rsparse)

# Cargar datos
maestro <- readRDS("maestroestr.RDS")
objetivos <- readRDS("objetivos.RDS")
tickets <- readRDS("tickets_enc.RDS")
matriz <- readRDS("matriz.RDS")

# -----------------------------------------
# FILTRADO DE CLIENTES DEL OBJETIVO 4
# -----------------------------------------
obj4 <- objetivos[[4]]$obj  # IDs de los clientes objetivo

# Filtrar los tickets de esos clientes y convertir fecha
tickets_filtrados <- tickets %>%
  filter(id_cliente_enc %in% obj4) %>%
  mutate(fecha = ymd(dia))

# -----------------------------------------
# SEPARACIÓN DEL HISTORIAL Y ÚLTIMA COMPRA
# -----------------------------------------
ultimos_tickets <- tickets_filtrados %>%
  group_by(id_cliente_enc) %>%
  filter(fecha == max(fecha)) %>%
  ungroup()

historial_tickets <- anti_join(tickets_filtrados, ultimos_tickets, by = "num_ticket")

# -----------------------------------------
# MATRIZ CLIENTE - PRODUCTO
# -----------------------------------------
tickets_matriz_agrupado <- historial_tickets %>%
  group_by(id_cliente_enc, cod_est) %>%
  summarise(N_compras = n(), .groups = "drop")

# Formato ancho sin prefijo
df_matriz <- pivot_wider(
  tickets_matriz_agrupado,
  names_from = cod_est,
  values_from = N_compras,
  values_fill = 0
)

# Crear matriz dispersa
matriz_sparse_o4 <- as(as.matrix(df_matriz[,-1]), "dgCMatrix")
rownames(matriz_sparse_o4) <- df_matriz$id_cliente_enc
colnames(matriz_sparse_o4) <- colnames(df_matriz)[-1]  # mantener cod_est como nombres de columna

# -----------------------------------------
# ENTRENAR MODELO WRMF
# -----------------------------------------
modelo_wrmf_o4 <- WRMF$new(rank = 10L, lambda = 0.1, feedback = 'implicit')
modelo_wrmf_o4$fit_transform(matriz_sparse_o4, n_iter = 1000L, convergence_tol = 1e-6)

# -----------------------------------------
# GENERAR RECOMENDACIONES
# -----------------------------------------
preds_o4 <- modelo_wrmf_o4$predict(matriz_sparse_o4, k = 1)

clientes <- rownames(matriz_sparse_o4)
productos_predichos <- attr(preds_o4, "ids")

recomendaciones_o4 <- data.frame(
  id_cliente_enc = clientes,
  cod_est = productos_predichos
)

# -----------------------------------------
# UNIR CON MAESTRO PARA DESCRIPCIÓN
# -----------------------------------------
recomendaciones_o4 <- recomendaciones_o4 %>%
  left_join(maestro %>% select(cod_est, descripcion), by = "cod_est")

# -----------------------------------------
# MOSTRAR RESULTADO FINAL
# -----------------------------------------
print(recomendaciones_o4)


















# Cargar librerías
library(tidyverse)
library(lubridate)
library(rsparse)

# 1. Cargar datos
maestro <- readRDS("maestroestr.RDS")
objetivos <- readRDS("objetivos.RDS")
tickets <- readRDS("tickets_enc.RDS")

# 2. Obtener los clientes del objetivo 4
obj4 <- objetivos[[4]]$obj

# 3. Filtrar solo tickets de los clientes objetivo
tickets_filtrados <- tickets %>%
  filter(id_cliente_enc %in% obj4) %>%
  mutate(fecha = ymd(dia))

# 4. Obtener última compra por cliente
ultimos_tickets <- tickets_filtrados %>%
  group_by(id_cliente_enc) %>%
  filter(fecha == max(fecha)) %>%
  ungroup()

# 5. Historial anterior (para entrenamiento)
historial_tickets <- anti_join(tickets_filtrados, ultimos_tickets, by = "num_ticket")

# 6. Agrupar historial para crear la matriz
tickets_matriz_agrupado <- historial_tickets %>%
  group_by(id_cliente_enc, cod_est) %>%
  summarise(N_compras = n(), .groups = "drop")

# 7. Crear matriz de cliente-producto
df_matriz <- pivot_wider(
  tickets_matriz_agrupado,
  names_from = "cod_est",
  values_from = "N_compras",
  values_fill = 0,
  names_prefix = "id_"
)

# 8. Convertir a sparseMatrix
matriz_sparse_o4 <- as(as.matrix(df_matriz[,-1]), "dgCMatrix")
rownames(matriz_sparse_o4) <- df_matriz$id_cliente_enc

# 9. ENTRENAMIENTO CON WRMF
modelo_wrmf_o4 <- WRMF$new(rank = 10L, lambda = 0.1, feedback = 'implicit')
modelo_wrmf_o4$fit_transform(matriz_sparse_o4, n_iter = 1000L, convergence_tol = 1e-6)

# 10. Crear matriz de productos NO recomendables (última compra)
ultimos_tickets_bin <- ultimos_tickets %>%
  mutate(cod_est = paste0("id_", cod_est)) %>%
  mutate(valor = 1) %>%
  pivot_wider(names_from = cod_est, values_from = valor, values_fill = 0) %>%
  column_to_rownames("id_cliente_enc")

# 11. Asegurar que tenga mismas columnas y orden que matriz_sparse_o4
columnas_faltantes <- setdiff(colnames(matriz_sparse_o4), colnames(ultimos_tickets_bin))
ultimos_tickets_bin[, columnas_faltantes] <- 0
ultimos_tickets_bin <- ultimos_tickets_bin[, colnames(matriz_sparse_o4)]

# 12. Convertir a matriz sparse para `not_recommend`
not_recommend_matrix <- as(as.matrix(ultimos_tickets_bin), "dgCMatrix")

# 13. Predecir recomendaciones (evitando últimos productos comprados)
preds_o4 <- modelo_wrmf_o4$predict(
  matriz_sparse_o4,
  k = 1,
  not_recommend = not_recommend_matrix
)

# 14. Preparar output
clientes <- rownames(matriz_sparse_o4)
productos_predichos <- attr(preds_o4, "ids") %>% str_remove("id_")

# 15. Armar tabla final con descripciones
recomendaciones_o4 <- data.frame(
  id_cliente_enc = clientes,
  cod_est = productos_predichos
) %>%
  left_join(maestro %>% select(cod_est, descripcion), by = "cod_est") %>%
  select(id_cliente_enc, cod_est, descripcion)

# 16. Ver resultado
print(recomendaciones_o4)















# Cargar librerías
library(tidyverse)
library(lubridate)
library(rsparse)
library(dplyr)

# 1. Cargar datos
maestro <- readRDS("maestroestr.RDS")
objetivos <- readRDS("objetivos.RDS")
tickets <- readRDS("tickets_enc.RDS")
matriz_reducida <- readRDS("matriz.RDS") 
colnames(matriz_reducida) <- sub("^.", "", colnames(matriz_reducida))
matriz_reducida <- as(matriz_reducida, "dgCMatrix" )
# Cargar la matriz reducida preexistente

# 3. Obtener los clientes del objetivo 4 - estos son los 10 clientes objetivo
obj4 <- objetivos[[4]]$obj

# 4. Filtrar solo tickets de los clientes objetivo
tickets_filtrados <- tickets %>%
  filter(id_cliente_enc %in% obj4) %>%
  mutate(fecha = ymd(dia))

# 5. Obtener última compra por cliente
ultimos_tickets <- tickets_filtrados %>%
  group_by(id_cliente_enc) %>%
  filter(fecha == max(fecha)) %>%
  ungroup()

# 8. Asegurar que todos los clientes objetivo estén en nuestra matriz
clientes_en_matriz <- intersect(obj4, rownames(matriz_reducida))
length(clientes_en_matriz)

# 9. 
matriz_sparse_o4 <- matriz_reducida
cat("Dimensiones de la matriz filtrada:", dim(matriz_sparse_o4), "\n")

# 10. ENTRENAMIENTO CON WRMF
modelo_wrmf_o4 <- WRMF$new(rank = 10L, lambda = 0.1, feedback = 'implicit')
modelo_wrmf_o4$fit_transform(matriz_sparse_o4, n_iter = 1000L, convergence_tol = 1e-6)

# 11. Crear matriz de productos NO recomendables (últimas compras)
productos_matriz <- colnames(matriz_sparse_o4)

# Verificar si los productos tienen prefijo "x"
empieza_con_x <- FALSE
if (!tiene_prefijo_id && length(grep("^x", sample_producto)) > 0) {
  empieza_con_x <- TRUE
  cat("Los productos empiezan con 'x':", empieza_con_x, "\n")
}

# 13. Crear una función para convertir formatos de producto
convertir_formato_producto <- function(codigo, de_matriz_a_maestro = TRUE) {
  if (de_matriz_a_maestro) {
    # Convertir de formato en matriz a formato en maestro
    if (tiene_prefijo_id) {
      codigo <- str_remove(codigo, "id_")
    }
    if (empieza_con_x) {
      codigo <- str_remove(codigo, "^x")
    }
  } else {
    # Convertir de formato en maestro a formato en matriz
    if (tiene_prefijo_id) {
      codigo <- paste0("id_", codigo)
    }
    if (empieza_con_x) {
      codigo <- paste0("x", codigo)
    }
  }
  return(codigo)
}

# 14. Crear matriz de exclusión para los clientes objetivo
not_recommend_df <- matrix(0, 
                       nrow = nrow(matriz_sparse_o4), 
                       ncol = ncol(matriz_sparse_o4))
rownames(not_recommend_df) <- rownames(matriz_sparse_o4)
colnames(not_recommend_df) <- productos_matriz

# 15. Para cada cliente objetivo, marcamos con 1 los productos de su último ticket
for (cliente in clientes_en_matriz) {
  # Obtenemos los productos del último ticket del cliente
  productos_ultimo_ticket <- ultimos_tickets %>%
    filter(id_cliente_enc == cliente) %>%
    pull(cod_est)
  
  # Convertimos los productos al formato que tienen en la matriz
  productos_ultimo_ticket <- sapply(productos_ultimo_ticket, 
                                 function(p) convertir_formato_producto(p, FALSE))
  
  # Marcamos estos productos con 1 en la matriz not_recommend
  productos_validos <- intersect(productos_ultimo_ticket, productos_matriz)
  if (length(productos_validos) > 0) {
    not_recommend_df[cliente, productos_validos] <- 1
    cat("Cliente", cliente, "tiene", length(productos_validos), "productos marcados como no recomendables\n")
  } else {
    cat("Cliente", cliente, "no tiene productos para excluir\n")
  }
}

# Convertimos a matriz sparse para 'not_recommend'
not_recommend_matrix <- as(not_recommend_df, "dgCMatrix")

# 16. Predecir recomendaciones (evitando últimos productos comprados)
matriz_sparse_o4 <- matriz_reducida[,obj4]
preds_o4 <- modelo_wrmf_o4$predict(
  matriz_sparse_o4,
  k = 10,  # Aumentamos k para tener más opciones
  not_recommend = not_recommend_matrix
)

# 17. Examinar la estructura de las predicciones
cat("\nEstructura de las predicciones:\n")
str(preds_o4)
print(class(preds_o4))
print(names(attributes(preds_o4)))

# 18. Inicializar dataframe de recomendaciones para los clientes objetivo
recomendaciones_o4 <- data.frame(
  id_cliente_enc = clientes_en_matriz,
  cod_est_matriz = NA_character_,
  cod_est = NA_character_,
  stringsAsFactors = FALSE
)

# 19. Extraer recomendaciones usando el método adecuado según la estructura
if ("ids" %in% names(attributes(preds_o4))) {
  cat("Usando atributo 'ids' para extraer recomendaciones\n")
  for (i in 1:length(clientes_en_matriz)) {
    cliente <- clientes_en_matriz[i]
    indice <- which(rownames(matriz_sparse_o4) == cliente)
    if (length(indice) > 0 && length(attr(preds_o4, "ids")[[indice]]) > 0) {
      # Guardamos el código como aparece en la matriz
      recomendaciones_o4$cod_est_matriz[i] <- attr(preds_o4, "ids")[[indice]][1]
      # Convertimos al formato de maestro
      recomendaciones_o4$cod_est[i] <- convertir_formato_producto(
        attr(preds_o4, "ids")[[indice]][1], TRUE)
      cat("Cliente", cliente, "recomendación:", recomendaciones_o4$cod_est[i], "\n")
    }
  }
} else if ("items" %in% names(attributes(preds_o4))) {
  cat("Usando atributo 'items' para extraer recomendaciones\n")
  for (i in 1:length(clientes_en_matriz)) {
    cliente <- clientes_en_matriz[i]
    indice <- which(rownames(matriz_sparse_o4) == cliente)
    if (length(indice) > 0 && length(attr(preds_o4, "items")[[indice]]) > 0) {
      # Guardamos el código como aparece en la matriz
      recomendaciones_o4$cod_est_matriz[i] <- attr(preds_o4, "items")[[indice]][1]
      # Convertimos al formato de maestro
      recomendaciones_o4$cod_est[i] <- convertir_formato_producto(
        attr(preds_o4, "items")[[indice]][1], TRUE)
      cat("Cliente", cliente, "recomendación:", recomendaciones_o4$cod_est[i], "\n")
    }
  }
} else {
  cat("Intentando extraer recomendaciones directamente\n")
  # Intentamos directamente con los nombres de las columnas y filas
  for (i in 1:length(clientes_en_matriz)) {
    cliente <- clientes_en_matriz[i]
    indice <- match(cliente, rownames(matriz_sparse_o4))
    if (!is.na(indice) && indice <= length(preds_o4) && length(preds_o4[[indice]]) > 0) {
      # Ordenamos por score y tomamos el primero
      prod_scores <- preds_o4[[indice]]
      prod_orden <- order(prod_scores, decreasing = TRUE)
      # Guardamos el código como aparece en la matriz
      recomendaciones_o4$cod_est_matriz[i] <- names(prod_scores)[prod_orden[1]]
      # Convertimos al formato de maestro
      recomendaciones_o4$cod_est[i] <- convertir_formato_producto(
        names(prod_scores)[prod_orden[1]], TRUE)
      cat("Cliente", cliente, "recomendación:", recomendaciones_o4$cod_est[i], "\n")
    }
  }
}

# 20. Estadísticas sobre resultados
cat("\nEstadísticas de recomendaciones:\n")
cat("Total clientes objetivo:", length(clientes_en_matriz), "\n")
cat("Clientes con recomendaciones:", sum(!is.na(recomendaciones_o4$cod_est)), "\n")
cat("Clientes sin recomendaciones:", sum(is.na(recomendaciones_o4$cod_est)), "\n")

# 21. Armar tabla final con descripciones
cat("\nIntentando hacer join con tabla maestro...\n")
recomendaciones_o4 <- recomendaciones_o4 %>%
  left_join(maestro %>% select(cod_est, descripcion), by = "cod_est")

# 22. Mostrar los resultados
cat("\nRecomendaciones finales:\n")
print(recomendaciones_o4)

# 23. Verificar cuántas recomendaciones coinciden con productos del último ticket
verificacion <- recomendaciones_o4 %>%
  filter(!is.na(cod_est)) %>%  # Ignoramos los NA para esta verificación
  left_join(
    ultimos_tickets %>% 
      select(id_cliente_enc, cod_est) %>% 
      mutate(en_ultimo_ticket = TRUE),
    by = c("id_cliente_enc", "cod_est")
  ) %>%
  summarise(
    total_recomendaciones = n(),
    coinciden_con_ultimo_ticket = sum(!is.na(en_ultimo_ticket), na.rm = TRUE),
    porcentaje_coincidencia = coinciden_con_ultimo_ticket / total_recomendaciones * 100
  )

cat("\nVerificación de filtrado de últimas compras:\n")
print(verificacion)

# 24. Obtener el resultado final en el formato adecuado
resultado_final <- recomendaciones_o4 %>%
  select(id_cliente_enc, cod_est, descripcion)

# 25. Guardar resultado
saveRDS(resultado_final, "recomendaciones_o4_sin_ultimas_compras.RDS")
















# Cargar librerías
library(tidyverse)
library(lubridate)
library(rsparse)
library(dplyr)

# 1. Cargar datos
maestro <- readRDS("maestroestr.RDS")
objetivos <- readRDS("objetivos.RDS")
tickets <- readRDS("tickets_enc.RDS")
matriz_reducida <- readRDS("matriz.RDS") 
colnames(matriz_reducida) <- sub("^.", "", colnames(matriz_reducida))
matriz_reducida <- as(matriz_reducida, "dgCMatrix")

# 3. Obtener los clientes del objetivo 4 - estos son los 10 clientes objetivo
obj4 <- objetivos[[4]]$obj

# 4. Filtrar solo tickets de los clientes objetivo
tickets_filtrados <- tickets %>%
  filter(id_cliente_enc %in% obj4) %>%
  mutate(fecha = ymd(dia))

# 5. Obtener última compra por cliente
ultimos_tickets <- tickets_filtrados %>%
  group_by(id_cliente_enc) %>%
  filter(fecha == max(fecha)) %>%
  ungroup()

# 8. Asegurar que todos los clientes objetivo estén en nuestra matriz
clientes_en_matriz <- intersect(obj4, rownames(matriz_reducida))
length(clientes_en_matriz)

# 9. Cambiamos el nombre para la matriz reducida
matriz_sparse_o4 <- matriz_reducida

# 10. ENTRENAMIENTO CON WRMF
modelo_wrmf_o4 <- WRMF$new(rank = 10L, lambda = 0.1, feedback = 'implicit')
modelo_wrmf_o4$fit_transform(matriz_sparse_o4, n_iter = 1000L, convergence_tol = 1e-6)

# 11. Crear matriz de productos NO recomendables (últimas compras)
matriz_sparse_filt <- matriz_sparse_o4[obj4,]
productos_matriz <- colnames(matriz_sparse_o4)

# 14. Crear matriz de exclusión para los clientes objetivo
not_recommend_df <- matrix(0, 
                           nrow = nrow(matriz_sparse_o4), 
                           ncol = ncol(matriz_sparse_o4))
rownames(not_recommend_df) <- rownames(matriz_sparse_o4)
colnames(not_recommend_df) <- productos_matriz

# 15. Para cada cliente objetivo, marcamos con 1 los productos de su último ticket
for (cliente in clientes_en_matriz) {
  # Obtenemos los productos del último ticket del cliente
  productos_ultimo_ticket <- ultimos_tickets %>%
    filter(id_cliente_enc == cliente) %>%
    pull(cod_est)
  
  # Verificamos qué productos del último ticket están en la matriz
  productos_validos <- intersect(productos_ultimo_ticket, productos_matriz)
  
  if (length(productos_validos) > 0) {
    not_recommend_df[cliente, productos_validos] <- 1
    cat("Cliente", cliente, "tiene", length(productos_validos), "productos marcados como no recomendables\n")
  } else {
    cat("Cliente", cliente, "no tiene productos para excluir\n")
  }
}

# Convertimos a matriz sparse para 'not_recommend'
not_recommend_matrix <- as(not_recommend_df, "dgCMatrix")

# 16. Predecir recomendaciones (evitando últimos productos comprados)
matriz_sparse_o4 <- matriz_reducida[clientes_en_matriz,]
preds_o4 <- modelo_wrmf_o4$predict(
  matriz_sparse_o4,
  k = 10,  # Aumentamos k para tener más opciones
  not_recommend = not_recommend_matrix[clientes_en_matriz,]
)

# 17. Examinar la estructura de las predicciones
cat("\nEstructura de las predicciones:\n")
str(preds_o4)
print(class(preds_o4))
print(names(attributes(preds_o4)))

# 18. Inicializar dataframe de recomendaciones para los clientes objetivo
recomendaciones_o4 <- data.frame(
  id_cliente_enc = clientes_en_matriz,
  cod_est = NA_character_,
  stringsAsFactors = FALSE
)

# 19. Extraer recomendaciones usando el método adecuado según la estructura
if ("ids" %in% names(attributes(preds_o4))) {
  cat("Usando atributo 'ids' para extraer recomendaciones\n")
  for (i in 1:length(clientes_en_matriz)) {
    cliente <- clientes_en_matriz[i]
    indice <- which(rownames(matriz_sparse_o4) == cliente)
    if (length(indice) > 0 && length(attr(preds_o4, "ids")[[indice]]) > 0) {
      recomendaciones_o4$cod_est[i] <- attr(preds_o4, "ids")[[indice]][1]
      cat("Cliente", cliente, "recomendación:", recomendaciones_o4$cod_est[i], "\n")
    }
  }
} else if ("items" %in% names(attributes(preds_o4))) {
  cat("Usando atributo 'items' para extraer recomendaciones\n")
  for (i in 1:length(clientes_en_matriz)) {
    cliente <- clientes_en_matriz[i]
    indice <- which(rownames(matriz_sparse_o4) == cliente)
    if (length(indice) > 0 && length(attr(preds_o4, "items")[[indice]]) > 0) {
      recomendaciones_o4$cod_est[i] <- attr(preds_o4, "items")[[indice]][1]
      cat("Cliente", cliente, "recomendación:", recomendaciones_o4$cod_est[i], "\n")
    }
  }
} else {
  cat("Intentando extraer recomendaciones directamente\n")
  # Intentamos directamente con los nombres de las columnas y filas
  for (i in 1:length(clientes_en_matriz)) {
    cliente <- clientes_en_matriz[i]
    indice <- match(cliente, rownames(matriz_sparse_o4))
    if (!is.na(indice) && indice <= length(preds_o4) && length(preds_o4[[indice]]) > 0) {
      # Ordenamos por score y tomamos el primero
      prod_scores <- preds_o4[[indice]]
      prod_orden <- order(prod_scores, decreasing = TRUE)
      recomendaciones_o4$cod_est[i] <- names(prod_scores)[prod_orden[1]]
      cat("Cliente", cliente, "recomendación:", recomendaciones_o4$cod_est[i], "\n")
    }
  }
}

# 20. Estadísticas sobre resultados
cat("\nEstadísticas de recomendaciones:\n")
cat("Total clientes objetivo:", length(clientes_en_matriz), "\n")
cat("Clientes con recomendaciones:", sum(!is.na(recomendaciones_o4$cod_est)), "\n")
cat("Clientes sin recomendaciones:", sum(is.na(recomendaciones_o4$cod_est)), "\n")

# 21. Armar tabla final con descripciones
cat("\nIntentando hacer join con tabla maestro...\n")
recomendaciones_o4 <- recomendaciones_o4 %>%
  left_join(maestro %>% select(cod_est, descripcion), by = "cod_est")

# 22. Mostrar los resultados
cat("\nRecomendaciones finales:\n")
print(recomendaciones_o4)

# 23. Verificar cuántas recomendaciones coinciden con productos del último ticket
verificacion <- recomendaciones_o4 %>%
  filter(!is.na(cod_est)) %>%  # Ignoramos los NA para esta verificación
  left_join(
    ultimos_tickets %>% 
      select(id_cliente_enc, cod_est) %>% 
      mutate(en_ultimo_ticket = TRUE),
    by = c("id_cliente_enc", "cod_est")
  ) %>%
  summarise(
    total_recomendaciones = n(),
    coinciden_con_ultimo_ticket = sum(!is.na(en_ultimo_ticket), na.rm = TRUE),
    porcentaje_coincidencia = coinciden_con_ultimo_ticket / total_recomendaciones * 100
  )

cat("\nVerificación de filtrado de últimas compras:\n")
print(verificacion)

# 24. Obtener el resultado final en el formato adecuado
resultado_final <- recomendaciones_o4 %>%
  select(id_cliente_enc, cod_est, descripcion)

# 25. Guardar resultado
saveRDS(resultado_final, "recomendaciones_o4_sin_ultimas_compras.RDS")