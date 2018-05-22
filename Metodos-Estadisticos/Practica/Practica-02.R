###############################################################
#                         Practica No2                        #
###############################################################

### P(X>12.3) 
## 24
##P(x > 12.3) =
pchisq(12.3,24, lower.tail =F )


#Si X se distribuye como una t con 25 grados de libertad, 
#el valor de “c” que cumple P(X>c)=0.80 es: 
#(Utilice aproximación a 4 decimales)
#P(x > 1.9) = pt(1.9, 8, lower.tail = F)
qt(0.2,25)

pt(-0.8562362,8,lower.tail = F)

# PREGUNTA 13
# p 0 0.15%
# N 12
# n 3 

# PREG 9
#Ciertos componentes de un equipo tienen un tiempo de vida que se describe mediante 
# una variable exponencial con media 500 días. La probabilidad de que un componente 
# dure más de 800 días es: 
#. (Utilice aproximación a 4 decimales)
#El tiempo de vida mínimo que debe tener un componente para estar comprendido dentro 
# del 75% de componentes con mayor duración es: 
#  . (Utilice aproximación a 3 decimales)

#P(x >= 800) = 1 - P(x<800)
pexp(800, 1/500, lower.tail = T)
qexp(0.75, 1/500)


## PREG 6
punif(9.2,7.2,9.5)-punif(8.6,7.2,9.5)


## PREG 3
## Un ingeniero de planta ha obtenido una muestra compuesta de 7 artículos
## producidos en la máquina A y 5 artículos producidos en la máquina B. 
## El asistente selecciona al azar 4 de los artículos para que sean inspeccionados
## La distribución teórica que se le puede asignar a la variable: 
## Número de artículos seleccionados producidos por la máquina B es: 
##  La probabilidad de que se seleccionen exactamente 3 artículos producidos
## por la maquina B es: (Utilice aproximación a 4 decimales)

dhyper(3,5,7,4)

#
pnorm(9.05,9,0.03)-pnorm(8.95,9,0.03) 
