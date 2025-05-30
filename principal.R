### PRINCIPAL, RETO 04, EROSKI ###

if(!requireNamespace("fmsb", quietly = TRUE)) {
  install.packages("fmsb")
}

library(fmsb)


#Limpiar entorno
rm(list=ls())


#-----------------------------------------------------


#Analisis exploratorio

{
  source('Analisis exploratorio/Analisis exploratorio.R')
  rm(list= setdiff(ls(),lsf.str()))
}


#Reduccion de datos

{
  source('Reduccion/reduccion de datos.R')
  rm(list= setdiff(ls(),lsf.str()))
}


#Clusterizacion

{
  source('Clusterización/Clusterizacion.R')
  rm(list= setdiff(ls(),lsf.str()))
}


#Analisis clusters

{
  source('Clusterización/Analisis Clusters.R')
  rm(list= setdiff(ls(),lsf.str()))
}


#Graficos Clusters

{
  source('Clusterización/Graficos Clusters.R')
  rm(list= setdiff(ls(),lsf.str()))
}


#Comparacion de algoritmos

{
  source('Comparacion de algoritmos/Comparacion de algoritmos.R')
  rm(list= setdiff(ls(),lsf.str()))
}


#Objetivo 1

{
  source('Objetivos/Objetivo 1.R')
  rm(list= setdiff(ls(),lsf.str()))
}


#Objetivo 2

{
  source('Objetivos/Objetivo 2.R')
  rm(list= setdiff(ls(),lsf.str()))
}


#Objetivo 3

{
  source('Objetivos/Objetivo 3.R')
  rm(list= setdiff(ls(),lsf.str()))
}


#Objetivo 4

{
  source('Objetivos/Objetivo 4.R')
  rm(list= setdiff(ls(),lsf.str()))
}


#Desarrollo de api en plumber

{
  source('API/API.R')
  rm(list= setdiff(ls(),lsf.str()))
}


#App shiny

{
  source('App Shiny/APP SHINY.R')
  rm(list= setdiff(ls(),lsf.str()))
}


#Limpiar entorno
rm(list=ls())


