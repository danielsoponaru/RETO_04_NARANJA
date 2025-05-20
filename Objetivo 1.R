library(stringr)
library(rsparse)
library(recommenderlab)
library(dplyr)
library(lubridate)
library(tidyr)

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

# Creamos el modelo WRMF -------------------------------------------------------
modelo_wrmf <- WRMF$new(rank = 10L, lambda = 0.1, feedback = "implicit")
modelo_wrmf$fit_transform(matriz, n_iter = 1000L, convergence_tol = 0.000001)
#-------------------------------------------------------------------------------

# Predecir producto que le falta en la compra
prod_objetivo1 <- str_c("X", objetivos$objetivo1$obj)

mo1_prod <- matriz
mo1_prod[, !(colnames(mo1_prod) %in% prod_objetivo1)] <- 0
mo1_prod <- as(mo1_prod, "sparseMatrix")

preds_o1_prod <- modelo_wrmf$predict(mo1_prod, k = 10, not_recommend = NULL)
preds_o1_prod
attr(preds_o1_prod, "ids")

# OBJETIVO 1 - Recomendación por productos (transpuesta de matriz)
prod_objetivo1 <- paste0("X", objetivos$objetivo1$obj)  # Ajustado al prefijo correcto

tmatriz <- t(matriz)

modelo_wrmf_clientes <- WRMF$new(rank = 10L, lambda = 0.1, feedback = 'implicit')
modelo_wrmf_clientes$fit_transform(tmatriz, n_iter = 1000L, convergence_tol = 0.000001)

mo1_cli <- tmatriz[rownames(tmatriz) %in% prod_objetivo1, , drop = FALSE]
if (nrow(mo1_cli) == 0) stop("no se encontraron productos válidos en 'tmatriz'.")

# Conversión obligatoria a sparseMatrix
library(Matrix)
mo1_cli <- as(mo1_cli, "sparseMatrix")

# Predicción
preds_o1_cli <- modelo_wrmf_clientes$predict(mo1_cli, k = 10)
