
# ===============================
# 8. Objetivo 1: Recomendación ALS por clúster
# ===============================
producto_promocionado <- "14351005"  # ID sin prefijo
col_prefijo <- paste0("X", producto_promocionado)

# Leer matriz unificada
df_mat <- readRDS("Datos/matriz_con_cluster.RDS")
res_list <- list()

for (cl in unique(df_mat$kmeans_cluster)) {
  cat("Procesando", cl, "...\n")
  df_k <- df_mat %>% filter(kmeans_cluster == cl)
  
  # Verificar existencia de columna de producto
  if (!(col_prefijo %in% colnames(df_k))) {
    cat("Producto no presente en", cl, "- saltando.\n")
    next
  }
  
  # Preparar datos para recosystem
  df_train <- df_k %>%
    select(id_cliente_enc, starts_with("X")) %>%
    pivot_longer(-id_cliente_enc, names_to = "producto", values_to = "valor") %>%
    filter(valor > 0) %>%
    mutate(producto = gsub("^X", "", producto))
  
  if (nrow(df_train) < 2) {
    cat("Datos insuficientes en", cl, "- saltando entrenamiento.\n")
    next
  }
  
  train_file <- paste0("datos_train_", cl, ".txt")
  write.table(df_train, train_file, sep = " ", row.names = FALSE, col.names = FALSE, quote = FALSE)
  
  # Entrenar ALS
  r <- Reco()
  train_set <- data_file(train_file)
  opts <- r$tune(train_set)
  r$train(train_set, opts = opts)
  
  # Predicción para clientes que no compraron el producto
  clientes_no <- df_k %>% filter(.data[[col_prefijo]] == 0) %>% pull(id_cliente_enc)
  if (length(clientes_no) == 0) next
  
  df_pred <- data.frame(
    id_cliente_enc = clientes_no,
    producto       = producto_promocionado
  )
  pred_file <- paste0("datos_pred_", cl, ".txt")
  write.table(df_pred, pred_file, sep = " ", row.names = FALSE, col.names = FALSE, quote = FALSE)
  
  out_file <- paste0("predicciones_", cl, ".txt")
  r$predict(data_file(pred_file), out_file(out_file))
  scores <- scan(out_file)
  df_pred <- df_pred %>% mutate(score = scores, kmeans_cluster = cl)
  
  res_list[[cl]] <- df_pred
}

# ===============================
# 9. Unir y guardar recomendaciones
# ===============================
res_valid <- res_list[!sapply(res_list, is.null)]
if (length(res_valid) > 0) {
  recomendaciones <- bind_rows(res_valid) %>% arrange(desc(score))
  saveRDS(recomendaciones, "Datos/recomendaciones_14351005_ALS_por_cluster.RDS")
  print(head(recomendaciones, 10))
  print(recomendaciones %>% count(kmeans_cluster))
} else {
  message("No se generaron recomendaciones para ningún cluster.")
}