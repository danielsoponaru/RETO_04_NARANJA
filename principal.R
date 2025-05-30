### PRINCIPAL, RETO 04, EROSKI ###

if(!requireNamespace("fmsb", quietly = TRUE)) {
  install.packages("fmsb")
}

library(fmsb)

#Limpiar entorno
rm(list=ls())

#-----------------------------------------------------

#Reduccion de datos

{
  source('reduccion de datos.R')
  rm(list= setdiff(ls(),lsf.str()))
}


#Objetivo 1



#Objetivo 2



#Objetivo 3



#Objetivo 4


#Desarrollo de api en plumber

{
  source('API/api.R')
  rm(list= setdiff(ls(),lsf.str()))
}


#Limpiar entorno
rm(list=ls())


