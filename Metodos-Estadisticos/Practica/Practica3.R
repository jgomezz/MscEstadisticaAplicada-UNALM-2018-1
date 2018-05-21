######################################
#        Practica No3
######################################

# Define path file
FILE_URL <- "https://raw.githubusercontent.com/jgomezz/MsEstadAplic_MetodosEstadisticos/master/data/practica/Practica3.csv"
FILE_ENCONDING <- "ISO-8859-1"

# Read file
transito.data <- read.csv(FILE_URL, fileEncoding = FILE_ENCONDING)

# Lima Norte
LimaNorte.transito.data <- subset(transito.data, PC=="Lima Norte" )

# Lima Sur
LimaSur.transito.data <- subset(transito.data, PC=="Lima Sur" )

# Cargar libreria
install.packages("DescTools")
library("DescTools")

###############################################################
#                         Preg 1                              #
###############################################################
# El analista estimó un intervalo del 97% de confianza para 
# la diferencia de proporciones de puntos críticos que tuvieron 
# víctimas mortales en Lima Norte (1) y Lima Sur (2), mediante 
# el método de Wald, siendo este igual a:

table(transito.data$PC, transito.data$VM)
#            No Si
# Lima Este  39 11
# Lima Norte 44  6
# Lima Sur   46 14

wa <- BinomDiffCI(6,50,14,60, conf.level=0.97, method="wald")
wa
#         est      lwr.ci      upr.ci 
# 0.11333333 -0.04154305  0.26820971 
# En el método de Wald se cambia el orden del intervalo y el signo
sprintf("[Método Wald] -> IC =[%.15f, %.15f ]",-wa[3],-wa[2] )

# Muestra el ancho del IC
sprintf("[Método Wald] -> Ancho del IC = %.15f",(wa[3]-wa[2]) )

###############################################################
#                         Preg 2                              #
###############################################################
# El analista estimó intervalos del 97% de confianza para la 
# proporción de puntos críticos que presentan una adecuada 
# señalización de tránsito en Lima Norte, mediante los métodos de:

table(transito.data$PC, transito.data$ST)
#            No Si
# Lima Este  19 31
# Lima Norte 27 23
# Lima Sur   21 39

# -----------------------------------------
# Wald
BinomCI(23,27+23, conf.level = 0.97, method="wald")
#     est    lwr.ci    upr.ci
# [1,] 0.46 0.3070433 0.6129567

# Wilson
BinomCI(23,27+23, conf.level = 0.97, method="wilson")
#      est    lwr.ci    upr.ci
# [1,] 0.46 0.3171771 0.6097092

# Exacto
binom.test(23,27+23, conf.level = 0.97 )$conf
# [1] 0.3048832 0.6209326
# attr(,"conf.level")
# [1] 0.97

install.packages("PropCIs")
library("PropCIs")
# Agresti y Caffo
add4ci(23,27+23, conf.level= 0.97)$conf.int
# [1] 0.3157127 0.6102133
# attr(,"conf.level")
# [1] 0.97

#0.612-0.307=0.305 # Wald
#0.609-0.317=0.292 # Wilson --> Mejor
#0.620-0.304=0.316 # Exacto
#0.610-0.315=0.295 # Agresti y Caffo

###############################################################
#                         Preg 3                              #
###############################################################
# -----------------------------------------
# El analista estimó un intervalo del 97% de confianza para 
# la diferencia de medias de los gastos realizados por mantenimiento 
# de los puntos críticos de Lima Este en los años 2015 y 2016.
# Lima Este
LimaEste.transito.data <- subset(transito.data, PC=="Lima Este" )
head(LimaEste.transito.data)

t.test(LimaEste.transito.data$Gasto2015,
       LimaEste.transito.data$Gasto2016, 
       conf.level = 0.97, paired = T)$conf
# [1] -5.285007  2.989007

###############################################################
#                         Preg 4                              #
###############################################################
# El analista estimó un intervalo del 97% de confianza para la  
# razón de varianzas del gasto (en miles de soles) que realizó  
# la municipalidad por mantenimiento de los puntos críticos de  
# Lima Norte(1) y Lima Sur(2) en el año 2015. El intervalo es:

var.test(LimaNorte.transito.data$Gasto2015,
         LimaSur.transito.data$Gasto2015,conf.level = 0.97)$conf
# [1] 0.7903509 2.6260973

# Aprovechó los resultados del anterior intervalo estimado para 
# obtener un intervalo del 97% para la diferencia de medias del 
# gasto (en miles de soles) que realizó la municipalidad por 
# mantenimiento de los puntos críticos de Lima Norte (1) y 
# Lima Sur (2) en el año 2015. El intervalo es:

install.packages("PropCIs")
library("PropCIs")
t.test(LimaNorte.transito.data$Gasto2015, LimaSur.transito.data$Gasto2015, conf.level = 0.97, paired = T)$conf
## la 2da parte no tiene respuesta
# [1] -7.8540992 -0.2431008

# Preg 5
# -----------------------------------------
hist(LimaEste.transito.data$Gasto2015)

#media
t.test(LimaEste.transito.data$Gasto2015,alternative = "t", conf.level = 0.97)$conf
#[1] 28.67003 34.80317
#varianza
install.packages("EnvStats")
library("EnvStats")
varTest(LimaEste.transito.data$Gasto2015,alternative = "t", conf.level = 0.97)$conf
#  63.26833 153.53929 
# coeficiente de variabilidad
install.packages("DescTools")
library("DescTools")
## Function CoefVar
CoefVar(LimaEste.transito.data$Gasto2015,conf.level = 0.97)
# est    low.ci    upr.ci 
# 0.3056859 0.2469101 0.4008661

# Preg.  6
# -----------------------------------------
t.test(LimaEste.transito.data$NA., LimaSur.transito.data$NA., conf.level = 0.98)$conf
# [1] -7.8540992 -0.2431008

# Preg.  7
# -----------------------------------------
# 334
head(LimaEste.transito.data)
sum(LimaSur.transito.data$NA.) # 334
length(LimaSur.transito.data$NA.) # 60
poisson.test(334, conf.level = 0.98)$conf/60
# [1] 4.882671 6.317396