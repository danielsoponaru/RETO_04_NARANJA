# ------------------------------
# Librerías necesarias
# ------------------------------
library(stringr)
library(rsparse)
library(recommenderlab)
library(dplyr)
library(lubridate)
library(tidyr)
library(Matrix)

# ------------------------------
# Cargar datos
# ------------------------------
objetivos <- readRDS("objetivos.RDS")
matriz_final <- readRDS("matriz.rds")  # realRatingMatrix
maestro <- readRDS("maestroestr.RDS")
tickets <- readRDS("tickets_enc.RDS")

# ------------------------------
# Convertir a matriz base
# ------------------------------
matriz <- as(matriz_final, "matrix")
matriz[is.na(matriz)] <- 0

# ------------------------------
# OBJETIVO 1: Recomendar producto específico solo a quienes NO lo han comprado
# ------------------------------

# Construir nombre del producto con prefijo "X"
prod_objetivo1 <- str_c("X", objetivos$objetivo1$obj)

# Transponer la matriz (productos como filas, clientes como columnas)
tmatriz <- t(matriz)

# Validar que el producto esté presente
if (!prod_objetivo1 %in% rownames(tmatriz)) {
  stop(paste("Producto", prod_objetivo1, "no encontrado en la matriz"))
}

# Entrenar modelo WRMF
modelo_wrmf_clientes <- WRMF$new(rank = 10L, lambda = 0.1, feedback = 'implicit')
modelo_wrmf_clientes$fit_transform(tmatriz, n_iter = 1000L, convergence_tol = 1e-6)

# Extraer la fila del producto (interacciones con clientes)
mo1 <- tmatriz[prod_objetivo1, , drop = FALSE]
mo1 <- as(mo1, "sparseMatrix")
colnames(mo1) <- colnames(tmatriz)  # Asegurar alineación de nombres

# Predecir afinidad del producto para todos los clientes
preds_all <- modelo_wrmf_clientes$predict(mo1, k = ncol(tmatriz))

# Obtener IDs de clientes predichos y sus puntuaciones
scores <- as.numeric(preds_all)
names(scores) <- attr(preds_all, "ids")

# Filtrar solo los clientes que NO compraron el producto
compraron <- which(tmatriz[prod_objetivo1, ] > 0)
no_compraron <- setdiff(names(scores), colnames(tmatriz)[compraron])

# Obtener top 10 entre quienes no lo compraron
top_recomendaciones <- sort(scores[no_compraron], decreasing = TRUE)[1:10]

# Mostrar resultados
top_recomendaciones

#========================
#     COMPARACIONES
#========================
# ---------------------------------------------------------------
# 1. Construir data frame con la etiqueta de compra
# ---------------------------------------------------------------

# Vector lógico: TRUE si el cliente compró el producto
flag_compro <- colnames(tmatriz) %in% colnames(tmatriz)[compraron]

df_scores <- data.frame(
  cliente   = names(scores),
  score     = scores,
  compro    = flag_compro
)

# ---------------------------------------------------------------
# 2. Estadísticas descriptivas por grupo con manejo de NA
# ---------------------------------------------------------------
library(dplyr)

resumen <- df_scores %>% 
  group_by(compro) %>% 
  summarise(
    n          = n(),
    media      = mean(score, na.rm = TRUE),
    mediana    = median(score, na.rm = TRUE),
    p25        = quantile(score, 0.25, na.rm = TRUE),
    p75        = quantile(score, 0.75, na.rm = TRUE),
    max_score  = max(score, na.rm = TRUE),
    min_score  = min(score, na.rm = TRUE)
  )

print(resumen)

# ---------------------------------------------------------------
# 3. Prueba t de diferencia de medias (asumiendo varianzas ≠)
# ---------------------------------------------------------------
t_test <- t.test(score ~ compro, data = df_scores, var.equal = FALSE)
print(t_test)

# ---------------------------------------------------------------
# 4. Visualización – Boxplot
# ---------------------------------------------------------------
library(ggplot2)

ggplot(df_scores, aes(x = compro, y = score, fill = compro)) +
  geom_boxplot(outlier.alpha = 0.3) +
  labs(title = "Distribución de scores WRMF",
       x = "¿Compró el producto?", y = "Score de afinidad") +
  theme_minimal() +
  theme(legend.position = "none")

#=================
#    CORREGIR
#=================
# ---------------------------------------------------------------
# 1. Resumen y revisión de valores en scores
# ---------------------------------------------------------------
summary(scores)

na_count <- sum(is.na(scores))
nan_count <- sum(is.nan(scores))
inf_count <- sum(is.infinite(scores))

cat("Valores NA en scores:", na_count, "\n")
cat("Valores NaN en scores:", nan_count, "\n")
cat("Valores Inf en scores:", inf_count, "\n")

# ---------------------------------------------------------------
# 2. Identificar clientes con valores problemáticos en scores
# ---------------------------------------------------------------
clientes_na <- names(scores)[is.na(scores) | is.nan(scores) | is.infinite(scores)]
cat("Número de clientes con valores problemáticos en scores:", length(clientes_na), "\n")
cat("Algunos IDs de clientes con valores problemáticos:\n")
print(head(clientes_na, 20))

# ---------------------------------------------------------------
# 3. Visualizar distribución de scores válidos
# ---------------------------------------------------------------
library(ggplot2)

df_scores_validos <- data.frame(score = scores[!is.na(scores) & !is.nan(scores) & !is.infinite(scores)])

ggplot(df_scores_validos, aes(x = score)) +
  geom_histogram(bins = 50, fill = "steelblue", color = "black") +
  labs(title = "Distribución de scores WRMF (solo valores válidos)", x = "Score", y = "Frecuencia") +
  theme_minimal()

# ---------------------------------------------------------------
# 4. Verificación del producto objetivo en la matriz
# ---------------------------------------------------------------
prod_objetivo_con_prefijo <- paste0("X", objetivos$objetivo1$obj)
cat("Producto objetivo con prefijo:\n")
print(prod_objetivo_con_prefijo)

producto_en_matriz <- prod_objetivo_con_prefijo %in% colnames(matriz)
cat("¿El producto objetivo está en la matriz? ", producto_en_matriz, "\n")

# Mostrar primeros productos de la matriz para inspección visual
cat("Primeros nombres de columnas (productos) en la matriz:\n")
print(head(colnames(matriz), 20))

# ---------------------------------------------------------------
# 5. Revisión de clientes y vectores relacionados
# ---------------------------------------------------------------
cat("Longitud vector 'compraron':", length(compraron), "\n")
cat("Número de columnas en matriz transpuesta (tmatriz):", ncol(tmatriz), "\n")

cat("Primeros valores en 'compraron':\n")
print(head(compraron))

# Verificar que todos los clientes de scores estén en tmatriz
todos_en_tmatriz <- all(names(scores) %in% colnames(tmatriz))
cat("¿Todos los clientes de 'scores' están en tmatriz? ", todos_en_tmatriz, "\n")

# Mostrar primeros nombres de clientes en tmatriz
cat("Primeros nombres de clientes en tmatriz:\n")
print(head(colnames(tmatriz), 20))

#===================
#     AJUSTAR
#===================
# 1. Obtener clientes comunes entre scores y tmatriz
clientes_comunes <- intersect(names(scores), colnames(tmatriz))
cat("Número de clientes comunes:", length(clientes_comunes), "\n")

# 2. Filtrar scores para que solo contenga clientes comunes
scores_filtrados <- scores[clientes_comunes]

# 3. Filtrar tmatriz para que solo contenga clientes comunes (columnas)
tmatriz_filtrada <- tmatriz[, clientes_comunes]

# 4. Verificar si quedan NAs en scores_filtrados
na_scores <- is.na(scores_filtrados)
cat("Números de NA en scores filtrados:", sum(na_scores), "\n")

# 5. Filtrar para eliminar NA en scores (opcional, si prefieres imputar puedes hacer aquí)
scores_validos <- scores_filtrados[!na_scores]
tmatriz_validada <- tmatriz_filtrada[, !na_scores]

cat("Número clientes con scores válidos tras filtrado:", length(scores_validos), "\n")

# Ahora scores_validos y tmatriz_validada están alineados y limpios para análisis

# Ejemplo: crear df para análisis con etiqueta de compra
flag_compro <- colnames(tmatriz_validada) %in% colnames(tmatriz_validada)[compraron]
table(flag_compro)

df_scores <- data.frame(
  cliente = names(scores_validos),
  score   = scores_validos,
  compro  = flag_compro
)
summary(df_scores)
# Puedes continuar con análisis descriptivo, pruebas t, visualizaciones, etc.
