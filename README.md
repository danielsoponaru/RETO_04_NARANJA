# 🛒 Sistema de Recomendación Personalizado para EROSKI

<div align="center">

![Business Data Analytics](https://img.shields.io/badge/Business-Data%20Analytics-orange)
![R](https://img.shields.io/badge/R-276DC3?logo=r&logoColor=white)
![Machine Learning](https://img.shields.io/badge/ML-Recommendation%20System-blue)

**Equipo Naranja** | **Curso 2024-2025**

</div>

## 👥 Autores

- **Uxue Duran**
- **Luken Larrea**  
- **Daniel Alejandro Soponaru**
- **Jon Basarte**
- **Naia Parte**
- **Helene Urreta**

---

## 📋 Descripción del Proyecto

Este proyecto desarrolla un **sistema de recomendación personalizado para EROSKI** utilizando técnicas avanzadas de filtrado colaborativo y factorización matricial. El sistema aborda cuatro objetivos específicos de recomendación basados en el análisis de patrones de comportamiento de compra de clientes reales.

## 🎯 Objetivos del Sistema

| Objetivo | Descripción |
|----------|-------------|
| **1️⃣** | Identificar los 10 clientes más propensos a comprar un producto específico |
| **2️⃣** | Recomendar productos adicionales al carrito de 10 clientes seleccionados |
| **3️⃣** | Asignar el producto más adecuado de una lista de ofertas a cada cliente |
| **4️⃣** | Sugerir productos posiblemente olvidados, excluyendo los del último ticket |

> **📎 Información Detallada:** Para consultar el proceso completo de los algoritmos, análisis detallados y resultados específicos, revisar el **Anexo Técnico** incluido en la documentación del proyecto.

---

## 🔬 Metodología Técnica

### 🔄 Pipeline de Procesamiento

#### Reducción de Dimensionalidad
- **Preprocesamiento:** Análisis y filtrado de la matriz cliente-producto original
- **Optimización de características:** Selección de productos y clientes más relevantes
- **Reducción de esparsidad:** Eliminación de elementos con baja frecuencia de interacción
- **Matriz reducida:** Transformación a formato optimizado para el modelado

#### Algoritmo Principal
- **WRMF (Weighted Regularized Matrix Factorization)** implementado a través del paquete `rsparse`
- Enfoque de retroalimentación implícita basado en patrones de compra
- Factorización matricial con regularización para prevenir sobreajuste

### ⚙️ Configuración del Modelo
- **Dimensión del espacio latente:** 10
- **Parámetro de regularización (λ):** 0.1  
- **Iteraciones de entrenamiento:** 1000
- **Tolerancia de convergencia:** 1e-6

---

## 📦 Instalación y Dependencias

### 🔧 Instalación Rápida
```r
# Instalar todas las librerías necesarias
install.packages(c(
  # Manipulación y procesamiento de datos
  "dplyr", "tidyr", "tidyverse", "purrr", "stringr", "lubridate",
  
  # Lectura y escritura de archivos
  "readr", "readxl",
  
  # Análisis exploratorio
  "naniar", "GGally",
  
  # Visualización
  "ggplot2", "ggrepel", "gridExtra", "plotly", "viridis",
  
  # Aplicaciones web Shiny
  "shiny", "shinydashboard", "shinyWidgets", "shinyjs", 
  "DT", "bslib", "waiter",
  
  # Machine Learning
  "cluster",
  
  # Sistemas de recomendación
  "recosystem", "recommenderlab",
  
  # Álgebra lineal
  "Matrix", "rsparse",
  
  # APIs
  "plumber"
))

# Instalar rsparse desde GitHub
devtools::install_github("dselivanov/rsparse")
```

<details>
<summary>📚 Ver código completo para cargar librerías</summary>

```r
# =============================================================================
# LIBRERÍAS DE R REQUERIDAS PARA EL PROYECTO
# =============================================================================

# -----------------------------------------------------------------------------
# MANIPULACIÓN Y PROCESAMIENTO DE DATOS
# -----------------------------------------------------------------------------
library(dplyr)          # Manipulación de datos
library(tidyr)          # Reorganización de datos
library(tidyverse)      # Conjunto de paquetes para ciencia de datos
library(purrr)          # Programación funcional
library(stringr)        # Manipulación de strings
library(lubridate)      # Manejo de fechas y horas

# -----------------------------------------------------------------------------
# LECTURA Y ESCRITURA DE ARCHIVOS
# -----------------------------------------------------------------------------
library(readr)          # Lectura de archivos CSV y texto
library(readxl)         # Lectura de archivos Excel

# -----------------------------------------------------------------------------
# ANÁLISIS EXPLORATORIO Y DETECCIÓN DE DATOS FALTANTES
# -----------------------------------------------------------------------------
library(naniar)         # Análisis y visualización de datos faltantes
library(GGally)         # Extensión de ggplot2 para análisis exploratorio

# -----------------------------------------------------------------------------
# VISUALIZACIÓN DE DATOS
# -----------------------------------------------------------------------------
library(ggplot2)        # Gráficos avanzados
library(ggrepel)        # Etiquetas que no se superponen
library(gridExtra)      # Composición de múltiples gráficos
library(plotly)         # Gráficos interactivos
library(viridis)        # Paletas de colores

# -----------------------------------------------------------------------------
# APLICACIONES WEB INTERACTIVAS (SHINY)
# -----------------------------------------------------------------------------
library(shiny)          # Framework para aplicaciones web
library(shinydashboard) # Dashboards con Shiny
library(shinyWidgets)   # Widgets adicionales para Shiny
library(shinyjs)        # JavaScript en Shiny
library(DT)             # Tablas interactivas
library(bslib)          # Temas Bootstrap para Shiny
library(waiter)         # Pantallas de carga para Shiny

# -----------------------------------------------------------------------------
# ANÁLISIS DE CLUSTERS Y MACHINE LEARNING
# -----------------------------------------------------------------------------
library(cluster)        # Análisis de clusters

# -----------------------------------------------------------------------------
# SISTEMAS DE RECOMENDACIÓN
# -----------------------------------------------------------------------------
library(recosystem)     # Sistema de recomendación basado en factorización matricial
library(recommenderlab) # Framework para sistemas de recomendación

# -----------------------------------------------------------------------------
# ÁLGEBRA LINEAL Y MATRICES DISPERSAS
# -----------------------------------------------------------------------------
library(Matrix)         # Matrices dispersas y álgebra lineal
library(rsparse)        # Matrices dispersas eficientes

# -----------------------------------------------------------------------------
# APIS WEB
# -----------------------------------------------------------------------------
library(plumber)        # Creación de APIs REST
```
</details>

---

## 📁 Estructura del Proyecto

```
danielsoponaru/RETO_04_NARANJA/
│
├── 📁 .Rproj.user/                      # Archivos de RStudio Project
├── 📁 Analisis exploratorio/            # Análisis exploratorio de datos
├── 📁 App Shiny/                        # Aplicación Shiny interactiva
├── 📁 Clusterización/                   # Análisis de clustering
├── 📁 Comparacion de algoritmos/        # Comparativa de algoritmos
├── 📁 Datos/                            # Datasets del proyecto
├── 📁 Graficos/                         # Visualizaciones generadas
├── 📁 Objetivos/                        # Implementación de objetivos
├── 📁 Reduccion/                        # Reducción de dimensionalidad
├── 📁 Resultados/                       # Outputs y resultados finales
├── 📁 Informe/                          # Informe del Proyecto (Contiene el Anexo)
│
├── 📄 .gitignore                        # Archivos ignorados por Git
├── 📄 .RDataTmp                         # Datos temporales de R
├── 📄 .Rhistory                         # Historial de comandos R
├── 📄 APL.R                             # Script principal de análisis
├── 📄 clientes_objetivo.rds             # Datos de clientes objetivo
├── 📄 matriz_con_cluster.RDS            # Matriz con clustering
├── 📄 matriz_sparse.rds                 # Matriz dispersa optimizada
├── 📄 modelo_als.rds                    # Modelo ALS entrenado
├── 📄 principal.R                       # Script principal del proyecto
├── 📄 principal.Rproj                   # Archivo de proyecto RStudio
└── 📄 productos.rds                     # Base de datos de productos
```

---

## 🚀 Uso del Sistema

Para ejecutar el sistema completo, simplemente ejecuta el script principal:

```r
source("principal.R")
```

Este script se encarga de:
- ✅ Instalar automáticamente todas las librerías necesarias
- ✅ Cargar y procesar los datos
- ✅ Entrenar los modelos de recomendación  
- ✅ Generar las predicciones para todos los objetivos
- ✅ Exportar los resultados finales
- ✅ Eliminar el environment al ejecutar cada documento R.

---

## ⚡ Características Técnicas

### 🔄 Procesamiento de Datos
- **Reducción de dimensionalidad:** Optimización previa de la matriz cliente-producto
- **Binarización:** Conversión de frecuencias a presencia/ausencia
- **Matrices dispersas:** Optimización de memoria con `dgCMatrix`
- **Filtrado temporal:** Exclusión inteligente basada en último ticket

### 🧠 Algoritmo de Recomendación
- **Factorización matricial:** Descomposición en características latentes
- **Regularización:** Prevención de sobreajuste con parámetro λ
- **Escalabilidad:** Manejo eficiente de matrices de gran dimensión

### ⏳ Coste de Ejecución
- **Tiempo:** 2 horas máximo.
---

## 📄 Licencia

Este proyecto ha sido desarrollado con fines académicos como parte del programa de Business Data Analytics. Los datos utilizados pertenecen a EROSKI y están sujetos a acuerdos de confidencialidad académica.

---

<div align="center">

**¿Te ha gustado este proyecto? ⭐ Dale una estrella!**</div>
