###############
# MATE RETO 4 #
###############

interacciones <- readRDS("Datos/Transformados/MatrizSuperReducida.RDS")

library(Matrix)     
library(tidyverse)


#--------------------------------------------------------#
# Implementación del algoritmo WRMF usando el método ALS #
#--------------------------------------------------------#

# Parámetros
alpha <- 40      # Controla la importancia de las interacciones
lambda <- 0.1    # Regularización
factors <- 20    # Número de factores latentes
iterations <- 10 # Iteraciones de ALS

# Matrices P y C
P <- as(interacciones > 0, "dgCMatrix") # Matriz de preferencias binarias
C <- 1 + alpha * interacciones          # Matriz de confianza

n_users <- nrow(P) 
n_items <- ncol(P)

# Inicialización aleatoria de matrices latentes
set.seed(123)
U <- matrix(rnorm(n_users * factors, 0, 0.01), n_users, factors)
V <- matrix(rnorm(n_items * factors, 0, 0.01), n_items, factors)


# ALS

for (iter in 1:iterations) {
  cat("Iteración:", iter, "\n")
  
  # Actualizar U (usuarios)
  for (u in 1:n_users) {
    idx_items <- which(P[u, ] != 0)
    if (length(idx_items) == 0) next
    
    Cu <- as.numeric(C[u, idx_items])
    Pu <- as.numeric(P[u, idx_items])
    Vu <- V[idx_items, , drop = FALSE]
    Cu_minus1 <- Cu - 1
    
    A <- t(Vu) %*% (diag(Cu_minus1) %*% Vu) + t(Vu) %*% Vu + lambda * diag(factors)
    b <- t(Vu) %*% (Cu * Pu)
    
    U[u, ] <- solve(A, b)
  }
  
  # Actualizar V (productos)
  for (i in 1:n_items) {
    idx_users <- which(P[, i] != 0)
    if (length(idx_users) == 0) next
    
    Ci <- as.numeric(C[idx_users, i])
    Pi <- as.numeric(P[idx_users, i])
    Ui <- U[idx_users, , drop = FALSE]
    Ci_minus1 <- Ci - 1
    
    A <- t(Ui) %*% (diag(Ci_minus1) %*% Ui) + t(Ui) %*% Ui + lambda * diag(factors)
    b <- t(Ui) %*% (Ci * Pi)
    
    V[i, ] <- solve(A, b)
  }
}

# Matriz de predicciones
predicciones <- U %*% t(V)

# Ejemplo: recomendaciones para el usuario 1
recomendaciones_usuario1 <- predicciones[1,]
top_items <- order(recomendaciones_usuario1, decreasing = TRUE)[1:10]
print("Top 10 productos recomendados para el usuario 1:")
print(top_items)


