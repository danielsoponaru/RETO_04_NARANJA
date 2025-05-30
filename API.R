if(!requireNamespace("plumber", quietly = TRUE)) {
  install.packages("plumber")
}

if(!requireNamespace("recommenderlab", quietly = TRUE)) {
  install.packages("recommenderlab")
}

library(plumber)
library(recommenderlab)

# Cargar la matriz desde el archivo
purchase_matrix <- readRDS("matriz.RDS")

## Convertir a matriz si no lo es
if (!is.matrix(purchase_matrix)) {
  purchase_matrix <- as.matrix(purchase_matrix)
}

# Crear realRatingMatrix para recommenderlab
rating_rrm <- as(purchase_matrix, "realRatingMatrix")

# Entrenar modelo colaborativo basado en usuarios
rec_model <- Recommender(rating_rrm, method = "UBCF")

# Definir función del endpoint
#* @get /recommend
#* @param user_id ID del usuario (por ejemplo, "user1")
#* @param n Número de recomendaciones (por defecto 3)
#* @response 200 OK Recomendaciones para el usuario
#* @response 500 Internal Server Error Si el usuario no es encontrado
#* @examples response(200, list(usuario = "user1", recomendaciones = c("item1", "item2", "item3")))
#* @examples response(500, "Usuario no encontrado")
recommend_function <- function(user_id = "user1", n = 3) {
  if (!(user_id %in% rownames(purchase_matrix))) {
    return(list(error = "Usuario no encontrado"))
  }
  
  pred <- predict(rec_model, rating_rrm[user_id], n = as.numeric(n))
  recs <- as(pred, "list")[[1]]
  
  return(list(
    usuario = user_id,
    recomendaciones = recs
  ))
}

# Crear el API
api <- pr()

# Asociar la función al endpoint /recommend
api$handle("GET", "/recommend", recommend_function)

# Definir la especificación OpenAPI para la documentación
api$set_api_spec(list(
  openapi = "3.0.0",
  info = list(
    title = "API de Recomendaciones Personalizadas",
    version = "1.0.0",
    description = "API para generar recomendaciones basadas en un modelo colaborativo de usuarios."
  ),
  paths = list(
    "/recommend" = list(
      get = list(
        summary = "Obtener recomendaciones personalizadas",
        description = "Devuelve una lista de recomendaciones para un usuario",
        parameters = list(
          list(
            name = "user_id",
            in = "query",
            description = "ID del usuario",
            required = FALSE,
            schema = list(type = "string", default = "user1")
          ),
          list(
            name = "n",
            in = "query",
            description = "Número de recomendaciones",
            required = FALSE,
            schema = list(type = "number", format = "double", default = 3)
          )
        ),
        responses = list(
          `200` = list(
            description = "Recomendaciones exitosas",
            content = list(
              "application/json" = list(
                example = list(
                  usuario = "user1",
                  recomendaciones = c("item1", "item2", "item3")
                )
              )
            )
          ),
          `500` = list(
            description = "Error en el servidor",
            content = list(
              "application/json" = list(
                example = "Usuario no encontrado"
              )
            )
          )
        )
      )
    )
  )
))

# Ejecutar el API
api$run(host = "0.0.0.0", port = 8000)

