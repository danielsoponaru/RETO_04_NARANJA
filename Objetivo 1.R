library(stringr)
library(rsparse)
library(recommenderlab)
library(dplyr)
library(lubridate)
library(tidyr)
library(stringr)
objetivos <- readRDS("objetivos.RDS")
matriz_reducida <- readRDS("matriz.rds")
maestro <- readRDS("maestroestr.RDS")
tickets <- readRDS("tickets_enc.RDS")

matriz <- as(matriz_reducida, "matrix")
matriz[is.na(matriz)] <- 0

# Convertir a objeto realRatingMatrix para evaluación
matriz_rrm <- as(matriz, "realRatingMatrix")

rmeans <- rowMeans(matriz)
cmeans <- colMeans(matriz)

rcounts <- rowSums(matriz != 0)
ccounts <- colSums(matriz != 0)

hist(rmeans)
hist(cmeans)
hist(rcounts)
hist(ccounts)

min(rcounts)

set.seed(12)
e <- evaluationScheme(matriz_rrm, method = "split", train = 0.8, given = 11, goodRating = 1)
algos <- list(
  "random" = list(name = "RANDOM", param = NULL),
  "ReRecommend" = list(name = "RERECOMMEND", param = NULL),
  "UBCF_10nn" = list(name = "UBCF", param = list(nn = 10)),
  "UBCF_50nn" = list(name = "UBCF", param = list(nn = 50)),
  "IBCF_Pearson" = list(name = "IBCF", param = list(method = "Pearson")),
  "SVDF_k50" = list(name = "SVDF",  param = list(k = 50)),
  "SVDF_k100" = list(name = "SVDF",  param = list(k = 100)),
  "SVDF_k200" = list(name = "SVDF",  param = list(k = 200))
)

# Realizar predicciones --> ratings / topNList
eval_topN <- evaluate(e, algos, type = "topNList", n = c(1, 3, 5))
plot(eval_topN, "prec/rec")

getConfusionMatrix(eval_topN[["random"]])[[1]]
getConfusionMatrix(eval_topN[["UBCF_10nn"]])[[1]]
getConfusionMatrix(eval_topN[["UBCF_50nn"]])[[1]]
getConfusionMatrix(eval_topN[["IBCF_Pearson"]])[[1]]
getConfusionMatrix(eval_topN[["SVDF_k50"]])[[1]]
getConfusionMatrix(eval_topN[["SVDF_k100"]])[[1]]
getConfusionMatrix(eval_topN[["SVDF_k200"]])[[1]]

eval_ratings <- evaluate(e, algos, type = "ratings", n = c(1, 3, 5))
plot(eval_ratings, "prec/rec")

# --- Preparar el producto objetivo ---
prod_objetivo1 <- str_c("X", objetivos$objetivo1$obj) 

# --- Transponer la matriz: productos como filas, clientes como columnas ---
tmatriz <- t(matriz)

# --- Entrenar modelo WRMF sobre productos ---
modelo_wrmf_clientes <- WRMF$new(rank = 10L, lambda = 0.1, feedback = 'implicit')
modelo_wrmf_clientes$fit_transform(tmatriz, n_iter = 1000L, convergence_tol = 0.000001)

# --- Extraer fila del producto objetivo ---
mo1_cli <- tmatriz[rownames(tmatriz) %in% prod_objetivo1, , drop = FALSE]

# Validar que el producto existe en la matriz
if (nrow(mo1_cli) == 0) stop("Producto no encontrado en la matriz.")

# Convertir a sparseMatrix
mo1_cli <- as(mo1_cli, "sparseMatrix")

# --- Predecir clientes con afinidad al producto ---
preds_o1_cli <- modelo_wrmf_clientes$predict(mo1_cli, k = 500)  # Un número amplio

# --- Extraer scores e IDs ---
scores <- attr(preds_o1_cli, "scores")[1, ]
ids <- attr(preds_o1_cli, "ids")[1, ]

# --- Filtro por score mínimo ---
score_min <- 0.9  # Ajusta este umbral según el modelo y dominio
clientes_filtrados <- ids[scores > score_min]
scores_filtrados <- scores[scores > score_min]

# --- Eliminar los que ya compraron el producto ---
clientes_con_producto <- which(tmatriz[prod_objetivo1, ] > 0)
clientes_existentes <- colnames(tmatriz)[clientes_con_producto]

clientes_finales <- clientes_filtrados[!(clientes_filtrados %in% clientes_existentes)]
scores_finales <- scores_filtrados[!(clientes_filtrados %in% clientes_existentes)]

# --- Mostrar resultado final ---
resultado <- data.frame(cliente = clientes_finales, score = scores_finales)
resultado <- resultado[order(-resultado$score), ]  # Ordenar de mayor a menor afinidad

cat("Clientes recomendados (score >", score_min, ") que no compraron el producto:\n")
print(resultado)
