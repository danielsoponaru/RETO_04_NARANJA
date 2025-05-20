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