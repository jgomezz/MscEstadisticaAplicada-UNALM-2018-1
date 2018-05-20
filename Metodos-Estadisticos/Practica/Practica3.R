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
library("DescTools")


table(transito.data$PC)
# Lima Este Lima Norte   Lima Sur 
# 50         50         60 

# Preg 1
# -----------------------------------------
# Cálculo del IC para el número de estudiantes
# de la carrera de "Medicina" entre las universidades UNCI y UNP
wa <- BinomDiffCI(50,160,60,160, conf.level=0.97, method="wald")
wa
# -0.05248648  0.17748648

# Preg 2
# -----------------------------------------
BinomCI(50,160, conf.level = 0.97, method="wald")
#[1,] 0.3125 0.2329795 0.3920205
BinomCI(50,160, conf.level = 0.97, method="wilson")
#[1,] 0.3125 0.2393023 0.3964195
binom.test(50,160, conf.level = 0.97 )$conf
#[1] 0.2348004 0.3986672
install.packages("PropCIs")
library("PropCIs")
add4ci(50,160, conf.level= 0.97)$conf.int
#[1] 0.2382194 0.3959270

#0.392-0.233=0.159
#0.396-0.239=0.157
#0.399-0.235=0.164
#0.396-0.238=0.158

# Preg 3
# -----------------------------------------
# Lima Este
LimaEste.transito.data <- subset(transito.data, PC=="Lima Este" )
head(LimaEste.transito.data)

t.test(LimaEste.transito.data$Gasto2015,
       LimaEste.transito.data$Gasto2016, 
       conf.level = 0.97, paired = T)$conf
# [1] -5.285007  2.989007

# Preg 4
var.test(LimaNorte.transito.data$Gasto2015,
         LimaSur.transito.data$Gasto2015,conf.level = 0.97)$conf
# [1] 0.7903509 2.6260973
install.packages("PropCIs")
library("PropCIs")
t.test(LimaNorte.transito.data$Gasto2015, LimaSur.transito.data$Gasto2015, conf.level = 0.97)$conf
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