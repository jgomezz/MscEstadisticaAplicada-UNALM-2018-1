
# Define path file
FILE_URL <- "https://raw.githubusercontent.com/jgomezz/MsEstadAplic_MetodosEstadisticos/master/data/banco/ansi/Caso_A-Etiq.csv"
FILE_ENCONDING <- "ISO-8859-1"

# Read file
banco.data <- read.csv(FILE_URL, header=TRUE, fileEncoding = FILE_ENCONDING )
#names(banco.data) <- c("Sucursal","Genero","Ingreso","Crédito","NIF","Edad","Deuda")

#
attach(banco.data)
summary(Ingreso)

#media
mean(Ingreso)

#media podada
mean(Ingreso, trim = 0.03)

#Mediana
median(Ingreso)

by(Ingreso, Sucursal, mean)
by(Ingreso, Sucursal, summary)

#cuantil
quantile(Ingreso)
quantile(Ingreso, 0.8, type=6)

#ejercicio1
by(Ingreso, Sucursal, quantile, 0.85, type=6)

#Segmentar
banco.sb.data <- subset(banco.data,Sucursal=="San Borja")

######################
#Medidas de Dispersión
######################
#Rango
r<-range(Ingreso)
r[2]-r[1]
diff(r)

# Rango Intercuartil
IQR(Ingreso, type=6)
by(Ingreso, Sucursal , IQR, type=6)

# Varianza
var(Ingreso)

#Desviacion Estandard
sd(Ingreso)

#Coeficiente de variación
sd(Ingreso)*100/abs(mean(Ingreso))

install.packages("raster")
library(raster) # libreria para calcular el coeficinete de variacion
cv(Ingreso)

# Funcion para calcular el Coeficiente de Variacion
calCoefVariac <- function(pDatos) {
  ret <- sd(pDatos)*100/abs(mean(pDatos))
  return(ret)
}

calCoefVariac(Ingreso)
calCoefVariac(Crédito)

########################
## Medidas de Asimetria
########################

# libreria para calcular asimetria
install.packages("moments")
library(moments) 

skewness(Ingreso)
by(Ingreso, Sucursal, skewness)

# Set una semilla
set.seed(50)
# generar 80 datos con media 1.70 y desviacion 0.05
estatura <- round(rnorm(80, 1.70, 0.05),2)
hist(estatura)
skewness(estatura)

# Coeficiente de Curtosis
kurtosis(Ingreso)

# Coeficiente de simetria de Pearson
calcCoSimPearson <- function(pDatos) {
  ret <- 3*( mean(pDatos) - median(pDatos))/sd(pDatos)
  return(ret)
}

calcCoSimPearson(Ingreso)

bancoCuantitativo <- banco.data[,3:6]
colMeans(bancoCuantitativo)

# Funcion para calcular la asimetria por columnas
colSkewness <- function(pDatos) {
  # obtiene el nro de columnas
  p<-ncol(pDatos) 
  # crea un arreglo
  ret <- rep(0,p) 
  # repetir el 0 la cantidad de "p" veces
  for (i in 1:p)
    ret[i] <- calcCoSimPearson(pDatos[,i])
  return(ret)
}

colSkewness(bancoCuantitativo)

apply(bancoCuantitativo, 2, calcCoSimPearson)


# Calcular la moda

# Si modeest no está instalado y cargado
install.packages("modeest") 
library(modeest)

mlv(estatura, method = "mfv")
apply(bancoCuantitativo, 2, mlv , method = "mfv")
