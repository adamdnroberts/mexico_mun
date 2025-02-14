library(dplyr)
library(rdrobust)
library(readxl)

#INEGI definition of rural https://cuentame.inegi.org.mx/poblacion/rur_urb.aspx

#Population of all localities
aguascalientes <- read_excel("raw/ITER_01XLS95.xls")
