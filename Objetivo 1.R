library(stringr)
library(rsparse)
library(recommenderlab)
library(dplyr)
library(lubridate)
library(tidyr)

objetivos <- readRDS("objetivos.RDS")
matriz_final <- readRDS("matriz.rds")
maestro <- readRDS("maestroestr.RDS")
tickets <- readRDS("tickets_enc.RDS")

matriz <- as(matriz_final, "matrix")
matriz[is.na(matriz)] <- 0
saveRDS(matriz, "matriz.rds")
matriz <- readRDS("matriz.rds")


############################### RECOMENDACIONES ################################
#Estadisticos e histogramas a nivel usuario/item
 max(matriz, na.rm = T); min(matriz, na.rm = T)
 
 rmeans <- rowMeans(matriz_final)
 cmeans <- colMeans(matriz_final)
 
 rcounts <- rowCounts(matriz_final)
 ccounts <- colCounts(matriz_final)
 
 hist(rmeans)
 hist(cmeans)
 hist(rcounts)
 hist(ccounts)
 
 min(rcounts)
 set.seed(12); e <- evaluationScheme(matriz_final, method = "split", train = 0.8, 
                                     given = 11, goodRating = 1)

algos <- list("random" = list(name = "RANDOM", param = NULL),
               "ReRecommend" = list(name = "RERECOMMEND", param= NULL),
               "UBCF_10nn" = list(name = "UBCF", param = list(nn = 10)), # (nn --> Number of Neighbours)
               "UBCF_50nn" = list(name = "UBCF", param = list(nn = 50)),
              "IBCF_Pearson" = list(name = "IBCF", param = list(method = "Pearson")),
               "SVDF_k50" = list(name = "SVDF",  param = list(k = 50)), # k --> Nº Factores Latentes
               "SVDF_k100" = list(name = "SVDF",  param = list(k = 100)),
               "SVDF_k200" = list(name = "SVDF",  param = list(k = 200)))

#Realizar predicciones --> ratings / topNList
algos
eval_topN <- evaluate(e, algos, type = "topNList", n = c(1,3,5))
plot(eval_topN,"prec/rec")
getConfusionMatrix(eval_topN[["random"]])[[1]]
getConfusionMatrix(eval_topN[["UBCF_10nn"]])[[1]]
getConfusionMatrix(eval_topN[["UBCF_50nn"]])[[1]]
getConfusionMatrix(eval_topN[["IBCF_Pearson"]])[[1]]
getConfusionMatrix(eval_topN[["SVDF_k50"]])[[1]]
getConfusionMatrix(eval_topN[["SVDF_k100"]])[[1]]
getConfusionMatrix(eval_topN[["SVDF_k200"]])[[1]]

eval_ratings <- evaluate(e, algos, type = "ratings", n = c(1,3,5))
plot(eval_ratings,"prec/rec")s
getConfusionMatrix(eval_topN[["random"]])[[1]]
getConfusionMatrix(eval_topN[["UBCF_10nn"]])[[1]]
getConfusionMatrix(eval_topN[["UBCF_50nn"]])[[1]]
getConfusionMatrix(eval_topN[["IBCF_Pearson"]])[[1]]
getConfusionMatrix(eval_topN[["SVDF_k50"]])[[1]]
getConfusionMatrix(eval_topN[["SVDF_k100"]])[[1]]
getConfusionMatrix(eval_topN[["SVDF_k200"]])[[1]]

# Creamos el modelo WRMF -------------------------------------------------------
modelo_wrmf <- WRMF$new(rank = 10L, lambda = 0.1, feedback = 'implicit')
modelo_wrmf$fit_transform(matriz, n_iter = 1000L, convergence_tol=0.000001)
# #-------------------------------------------------------------------------------
#Predecir producto que le falta en la compra
prod_objetivo1 <- str_c("id_", objetivos$objetivo1$obj)
mo1 <- matriz
mo1[, !(colnames(mo1) %in% prod_objetivo1)] <- 0
mo1 <- as(mo1, "sparseMatrix")
mo1 <- matriz[,str_extract(colnames(matriz), "\\d+") %in% objetivos$objetivo1$obj, drop = F]
preds_o1 <- modelo_wrmf$predict(mo1, k = 10, not_recommend = NULL)
preds_o1
attr(preds_o1,'ids')




# OBJETIVO 1
prod_objetivo1 <- str_c("id_", objetivos$objetivo1$obj)
tmatriz <- t(matriz)
#tmatriz <- as(tmatriz, "sparseMatrix")

modelo_wrmf_clientes <- WRMF$new(rank = 10L, lambda = 0.1, feedback = 'implicit')
modelo_wrmf_clientes$fit_transform(tmatriz, n_iter = 1000L, convergence_tol=0.000001)

mo1 <- tmatriz[rownames(tmatriz) %in% prod_objetivo1, , drop = F]
mo1 <- as(mo1, "sparseMatrix")

preds_o1 <- modelo_wrmf_clientes$predict(mo1, k = 10)
preds_o1
attr(preds_o1,'ids')