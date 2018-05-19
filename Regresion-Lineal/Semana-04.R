
## Lectura de datos
Tiempo.data <- read.table(file = "http://tarwi.lamolina.edu.pe/~clopez/Regresion/Tiempo.txt", header = T)
Tiempo.data

attach(Tiempo.data)

# Modelo
Tiempo.m1 <- lm(Tiempo ~ Temperatura)
summary(Tiempo.m1)

#
#
# Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 2184.003     75.022   29.11 1.45e-08 ***
#  Temperatura  -54.459      3.015  -18.06 3.94e-07 ***
#
#####
t <- (2184.003 - 2100)/75.022
1 - pt(t,df=7)

#####
t <- (-54.459 + 55)/3.015
t

pt(t,df=7)

#####
t <- 2184.003/75.022
t

pt(-29.11, df=7) + (1 - pt(29.11, df=7))


Tiempo.m0 <- lm(Tiempo ~ -1 + Temperatura)
Tiempo.m0
summary(Tiempo.m0)

Tiempo.m0 <- lm(Tiempo ~ 0 + Temperatura)
Tiempo.m0
summary(Tiempo.m0)

##########################################################
# VALores Estimados
##########################################################

# para un promedio se usa el intervalo "confidence"
predict(Tiempo.m1, data.frame(Temperatura = 30), level = 0.91 , interval="confidence")


# para un valor en concreto se usa el intervalo "prediction"
predict(Tiempo.m1, data.frame(Temperatura = 25), level = 0.94 , interval="prediction")



par(mfrow = c(2,2))
plot(Tiempo.m1)
residuals(Tiempo.m1)


