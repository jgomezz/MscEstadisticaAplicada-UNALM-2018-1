Forbes.data <- read.table(file = "http://tarwi.lamolina.edu.pe/~clopez/Regresion/Forbes.txt", header = T)
attach(Forbes.data)
Forbes.m1 <- lm(LogPresion ~ Temperatura)
Forbes.m1

summary(Forbes.m1)

Forbes.m0 <- lm(LogPresion  ~ 2)

abline(Forbes.m1)

anova(Forbes.m1)

#
Forbes.m3 <- lm(LogPresion ~ Temperatura - 1)
Forbes.m3


#intevalos de confianza
confint(Forbes.m1, level = 0.90)

##                     5 %        95 %
## (Intercept) -47.9933161 -36.2822424
## Temperatura   0.8666529   0.9243344
##
##      -47.9933161 <= Bo <= -36.2822424  
##        0.8666529 <= B1 <=  0.9243344