# Librerías
library(stringr)
library(rsparse)
library(recommenderlab)
library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)

set.seed(123)  # (Punto 6) Reproducibilidad

# ----------------------------
# Carga de datos
# ----------------------------
objetivos <- readRDS("objetivos.RDS")
matriz_final <- readRDS("matriz.rds")
maestro <- readRDS("maestroestr.RDS")
tickets <- readRDS("tickets_enc.RDS")

# ----------------------------
# Preparación de matriz
# ----------------------------
matriz0 <- as(matriz_final, "matrix")
matriz[is.na(matriz)] <- 0
saveRDS(matriz0, "matriz0.rds")

# ----------------------------
# Selección del producto objetivo
# ----------------------------
prod_objetivo1 <- str_c("X", objetivos$objetivo1$obj)
tmatriz <- t(matriz)

# ----------------------------
# Modelos con distintos ranks (Puntos 1 y 2)
# ----------------------------
ranks <- c(10, 50, 100)
modelos <- list()
resultados_eval <- list()

for (r in ranks) {
  cat("Entrenando modelo con rank =", r, "\n")
  modelo <- WRMF$new(rank = r, lambda = 0.1, feedback = 'implicit')
  modelo$fit_transform(tmatriz, n_iter = 100, convergence_tol = 1e-6)
  modelos[[as.character(r)]] <- modelo
}

# ----------------------------
# Evaluación del modelo base con métricas top-N (Punto 3, 4)
# ----------------------------
matriz_eval <- as(t(matriz), "realRatingMatrix")
esquema <- evaluationScheme(matriz_eval, method = "split", train = 0.8, given = 10, goodRating = 1)

algos <- list(
  "random"     = list(name = "RANDOM", param = NULL),
  "UBCF_10"    = list(name = "UBCF", param = list(nn = 10)),
  "IBCF_Pear"  = list(name = "IBCF", param = list(method = "Pearson")),
  "SVD_50"     = list(name = "SVDF", param = list(k = 50))
)

eval_result <- evaluate(esquema, algos, type = "topNList", n = c(1, 3, 5, 10))
plot(eval_result, "prec/rec")

# ----------------------------
# Selección de modelo y predicción
# ----------------------------
modelo_wrmf <- modelos[["50"]]  # Puedes cambiar aquí el modelo con mejor desempeño

mo1 <- tmatriz[rownames(tmatriz) %in% prod_objetivo1, , drop = FALSE]
mo1 <- as(mo1, "sparseMatrix")

preds <- modelo_wrmf$predict(mo1, k = ncol(tmatriz))
scores <- as.numeric(preds)
names(scores) <- attr(preds, "ids")

compraron <- which(tmatriz[prod_objetivo1, ] > 0)
no_compraron <- setdiff(names(scores), colnames(tmatriz)[compraron])

top_recomendados <- sort(scores[no_compraron], decreasing = TRUE)[1:10]

# ----------------------------
# Preparación de data.frame para análisis
# ----------------------------
clientes_comunes <- intersect(names(scores), colnames(tmatriz))
scores_filtrados <- scores[clientes_comunes]
tmatriz_filtrada <- tmatriz[, clientes_comunes]

scores_validos <- scores_filtrados[!is.na(scores_filtrados)]
tmatriz_validada <- tmatriz_filtrada[, !is.na(scores_filtrados)]

flag_compro <- colnames(tmatriz_validada) %in% colnames(tmatriz_validada)[compraron]

df_scores <- data.frame(
  cliente = names(scores_validos),
  score   = scores_validos,
  compro  = flag_compro
)

# ----------------------------
# Estadísticos descriptivos
# ----------------------------
resumen <- df_scores %>%
  group_by(compro) %>%
  summarise(
    n        = n(),
    media    = mean(score),
    mediana  = median(score),
    p25      = quantile(score, 0.25),
    p75      = quantile(score, 0.75),
    max      = max(score),
    min      = min(score)
  )

print(resumen)

# ----------------------------
# Prueba t entre grupos
# ----------------------------
if (length(unique(df_scores$compro)) == 2) {
  t_test <- t.test(score ~ compro, data = df_scores)
  print(t_test)
}

# ----------------------------
# Visualización: Boxplot (Punto 11)
# ----------------------------
ggplot(df_scores, aes(x = compro, y = score, fill = compro)) +
  geom_boxplot(outlier.alpha = 0.3) +
  labs(title = "Distribución de Scores WRMF por Grupo",
       x = "¿Compró el producto?", y = "Score de recomendación") +
  theme_minimal()

# ----------------------------
# Activación operativa (Punto 10)
# ----------------------------
threshold <- quantile(scores, 0.95)  # Activar top 5%
recomendados_operativos <- df_scores %>% filter(score >= threshold)

cat("Total de clientes activables:", nrow(recomendados_operativos), "\n")

# ----------------------------
# Simulación A/B Test (Punto 9 - bosquejo)
# ----------------------------
# Grupo control vs tratamiento (simulado)
set.seed(123)
df_scores$grupo <- sample(c("control", "tratamiento"), nrow(df_scores), replace = TRUE)

conversion_rate <- df_scores %>%
  group_by(grupo, compro) %>%
  summarise(n = n()) %>%
  tidyr::pivot_wider(names_from = compro, values_from = n, values_fill = 0) %>%
  mutate(tasa_conversion = TRUE / (TRUE + FALSE))

print(conversion_rate)
