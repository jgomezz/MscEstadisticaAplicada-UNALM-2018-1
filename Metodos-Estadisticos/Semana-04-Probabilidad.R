## Probabilidad

## FUncion de probabilidad
teo <- c(1,2,3,4,5,6,5,4,3,2,1)/36

## Simulacion del lanzamiento de los dados
valorDado <- 1:6
nroDados <- 2
lanzamiento <- sample(valorDado, nroDados, replace = T) 

## replace = T : para que en el siguiente lanzamiento vuelva
## a considerar el valor obtenido en la siguiente ejecucion


### Calcular la funcion de probabilidad

## se pone una semilla para que todos los resultados sean
## los mismos

## nro de lanzamientos
nroLanzamientos <- 1000
nroDados2 <- 2

######################################################
obtenerFuncionProbabilidad <- function(r,nDado) { 
  ## crear un vector de 0 con r elementos
  result <- rep(0,r)  
  VALOR_DADO <- 1:6
  
  for (i in 1:r){

    result[i] <- sum(sample(VALOR_DADO, nDado, replace = T))
  } 
    
  funcProbabilidad <- table(result)/r
  
  return(funcProbabilidad) 
}

## poner semilla de 40 y calcular la funcion de probabilidad
set.seed(40)
x <- obtenerFuncionProbabilidad(2,2)
x
## poner semilla de 41 y calcular la funcion de probabilidad
set.seed(41)
obtenerFuncionProbabilidad(2000,2)

set.seed(10)
sample(1:6,2)



######################################################
## Distribucion Binomial
######################################################

#   d    density
#   p    probability   
#   q     quantile
#   r     random
#
#        
#
x <- 0:10
Px <- dbinom(0:10, 10 , 1/5)
plot(x, Px, type = "h")

# Probabiidad que responda 3 : P(x=3)
dbinom(3,10,1/5)

# Calculo de probabilidad de que aprueben, que conteste
# mas de 5 preguntas correctas
1-pbinom(5,10,1/5)
pbinom(5,10,1/5, lower.tail = F)


# numero pseudoaleatorios
resu <- rbinom(80, 10,1/5)
mean(resu)
# varianza muestral
var(resu)


######################################################
## Distribucion Hipergeometrica
######################################################
#              3       7
#            (   )   (   )        
#              0       4
# P(x=0) = --------------
#                 10
#               (   )
#                  4
#
#         P(x=0)     A    N-A      n  
#   dhyper( 0,      3,      7,      4)  
#
#
#  p(x=0) : Probabilidad que no lo detecte
#

dhyper(0,3,7,4)


######################################################
## Distribucion de Poisson
######################################################

ppois(0, 0.5, lower.tail = F)


######################################################
## Distribucion Geometrica
######################################################
# calcular el exito de lanzar un cohete con una probabilida de 0.8 por cada lanzamiento
#
dgeom(6-1,0.8)


######################################################
## Distribucion Binomial Negativa
######################################################
# El calculo es dirente en minitab
#
# Se resta r
dnbinom(7-4,4,0.6)


######################################################
## Distribucion UNiforme
######################################################
# el R acumula de izquierda a derecha
# hay que ingresar  el lower.tail = F
punif(2,0,8, lower.tail=F)

# otra forma
1- punif(2,0,8)



######################################################
## Distribucion Exponencial
######################################################

pexp(350, 1/300)

pexp(330, 1/300) - pexp(320, 1/300)

qexp(0.2, 1/300)


######################################################
## Distribucion Normal
######################################################
pnorm(750,850,45)


######################################################
## Distribucion t, Chi y F
######################################################







