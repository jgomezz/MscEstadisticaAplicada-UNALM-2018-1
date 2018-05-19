
################################
##    Consumo de Gasolina
################################
Gasolina2001.data <- read.table(file = "http://tarwi.lamolina.edu.pe/~clopez/Regresion/Gasolina2001.txt", header = T)
attach(Gasolina2001.data)
TasaLic <- 1000*Licencias/Poblacion
TasaComb <- 1000*Combustible/Poblacion
Ingreso <- Ingreso/1000
logMillas <- log2(Millas)
Gasolina2001.m5 <- lm(TasaComb ~ Impuesto + TasaLic + Ingreso + logMillas)
summary(Gasolina2001.m5)

##Predicción y valores estimados
predict(Gasolina2001.m5, data.frame(Impuesto = 20, TasaLic = 900, Ingreso = 30, logMillas = 15),level = 0.98, interval = "confidence")
predict(Gasolina2001.m5, data.frame(Impuesto = 20, TasaLic = 900, Ingreso = 30, logMillas = 15),level = 0.98, interval = "prediction")

detach(Gasolina2001.data)

#######################################
##    Caso de Estudio de Berkeley
#######################################
Berkeley.data <- read.table(file = "http://tarwi.lamolina.edu.pe/~clopez/Regresion/Berkeley.txt", header = T)
attach(Berkeley.data)
head(Berkeley.data)

# construimos matriz de dispersión
pairs(Puntuacion ~ Peso2 + Peso9 + Peso18)

# Construimos el modelo
Berkeley.m1 <- lm(Puntuacion ~ Peso2 + Peso9 + Peso18)
Berkeley.m1

summary(Berkeley.m1)


################################
##    Estudio de ejemplo de Berkeley
################################
PesoG9 <- Peso9 - Peso2
PesoG18 <- Peso18 - Peso9
Berkeley.m2 <- lm(Puntuacion ~ Peso2 + PesoG9 + PesoG18)

# Construir modelo
Berkeley.m2
summary(Berkeley.m2)

# Resumen
pairs(Puntuacion ~ Peso2 + PesoG9 + PesoG18)

# construimos otro modelo sin el peso
Berkeley.m2.1 <- lm(Puntuacion ~ PesoG9 + PesoG18)
summary(Berkeley.m2.1)

#
Berkeley.m3 <- lm(Puntuacion ~ Peso2 + Peso9 + Peso18 + PesoG9 + PesoG18)
Berkeley.m3

########################################
## Coeficiente de determinación parcial
########################################

install.packages("asbio")
library("asbio")

Berkeley.model.1 <- lm(Puntuacion ~ Peso2 + Peso9 + Peso18)
Berkeley.model.2 <- lm(Puntuacion ~ Peso9 + Peso18)

partial.R2(Berkeley.model.2, Berkeley.model.1)
help(partial.R2)

# > partial.R2(Berkeley.mo.2, Berkeley.mo.1)
# [1] 0.05054407

# la variable Peso2 disminuye en 5% la suma 
#  de los cuadrados de los residuales

Berkeley.model.1 <- lm(Puntuacion ~ Peso2 + Peso9 + Peso18)
Berkeley.model.2 <- lm(Puntuacion ~ Peso9 + Peso18)
Berkeley.model.3 <- lm(Puntuacion ~ Peso9)
partial.R2(Berkeley.model.3, Berkeley.model.2)

summary(Berkeley.model.2)







