###############################################################
#                         Practica No1                        #
###############################################################

# Define path file
FILE_URL <- "https://raw.githubusercontent.com/jgomezz/MsEstadAplic_MetodosEstadisticos/master/data/banco/ansi/Caso_1.csv"
FILE_ENCONDING <- "ISO-8859-1"

# Read file
Supermercado.data <- read.csv(FILE_URL, header=TRUE, fileEncoding = FILE_ENCONDING )

attach(Supermercado.data)
head(Supermercado.data)
summary(Monto)

###############################################################
#                         Preg 2                              #
###############################################################
# En el local de Surco, el porcentaje de clientes que pagan al
# contado es ___%. (Considere si es necesario aproximación a 
# dos decimales).

by(Pago, Local, summary)
#Local: Surco
#Contado TCrédito  TDébito 
#17       26       17 
 
percentaje <- 17/(17+26+17) 
percentaje
# [1] 0.2833333

# Rpta --> 28.33%
#

table(Local, Monto)

###############################################################
#                         Preg 3                              #
###############################################################
# En el local de Surco, la forma más frecuente de pago es _______.
# Esto debido a que para esta categoría se obtiene una frecuencia 
# absoluta igual a _____.

by(Pago, Local, summary)
#Local: Surco
#Contado TCrédito  TDébito 
#17       26       17 

## Rpta:
## Forma mas frecuente de pago es TCrédito
## Frec. absoluta 26


# Local de Surco
Supermercado.data.Surco <- subset(Supermercado.data,Local=="Surco")
attach(Supermercado.data.Surco)

#
head(Supermercado.data.Surco)
#   Local    Género     Pago Monto Productos ProducMarca   Opinión
# 1 Surco  femenino TCrédito 205.9         9           3   Regular
# 2 Surco  femenino  Contado 281.5         7           3 Muy Bueno
# 3 Surco  femenino TCrédito 226.9         6           1      Malo
# 6 Surco  femenino  TDébito 314.2         9           3     Buena
# 7 Surco  femenino TCrédito 319.8        10           1 Muy Bueno
# 8 Surco masculino TCrédito 309.8         9           2   Regular

Supermercado.data.Surco[,4]


###############################################################
#                         Preg 6                              #
###############################################################
# En el local de Surco, después de construir la tabla de frecuencia 
# (considere los argumentos que aparecen por defecto en el R), el 
# quinto límite superior de la variable etiquetada como Monto es __
#. (Considere si es necesario aproximación a un decimal).

# tabla de frecuencia
table(Supermercado.data.Surco[,4])

# VARIABLE DE INGRESO
# REGLAS DE sTURGES k = 1 + 3.32 LOG10(N)
# RANGO r = Xman - Xmin
# ANCHO w = r/k

# Tabla de frecuencia para datos continuos
# Get the library.
library(agricolae)
h2 <- with(Supermercado.data.Surco, graph.freq(as.numeric(Monto), plot = FALSE))
print(table.freq(h2), row.names=FALSE)
hist(Monto)

# Lower Upper Main Frequency Percentage CF   CPF
# 200   230  215         6       10.0  6  10.0
# 230   260  245         8       13.3 14  23.3
# 260   290  275        15       25.0 29  48.3
# 290   320  305        14       23.3 43  71.7
# 320   350  335         9       15.0 52  86.7
# 350   380  365         4        6.7 56  93.3
# 380   410  395         4        6.7 60 100.0

# Rpta :  350

###############################################################
#                         Preg 8                              #
###############################################################
# En el local de Surco, el monto promedio de compra es de ____
# soles con un coeficiente de variación de ____%. 
# (Considere aproximación a dos decimales).

# Promedio
mean(Supermercado.data.Surco$Monto) 
# [1] 297.48

#Coeficiente de Variacion
sd(Supermercado.data.Surco$Monto)*100/abs(mean(Supermercado.data.Surco$Monto)) 
# [1] 16.23673

###############################################################
#                         Preg 9                              #
###############################################################
# En el local de Surco, el monto máximo de compra para estar 
# considerado dentro del 80% de registros de venta con menores 
# montos de compra es: ____ soles. (Considere aproximación a dos decimales)
#
# Sugerencia: Utilice la función correspondiente de R y el 
# algoritmo que aparece por defecto (no use type=6).

# Mediana
median(Monto) #[1] 292

# Quantil
quantile(Monto)
quantile(Monto, 0.8) 
# 80% 
# 333.14 

IQR(Monto)
by(Monto,Local,IQR)


###############################################################
#                       Preg 11                              #
###############################################################
# En el local de  se tiene una menor Monto mediano de compra.
#
# Local de San Borja
Supermercado.data.SanBorja <- subset(Supermercado.data,Local=="San Borja")
attach(Supermercado.data.SanBorja)
table(Supermercado.data.SanBorja[,4])

# Promedio
mean(Monto) # [1] 301.592

#Coeficiente de Variacion
sd(Monto)*100/abs(mean(Monto)) # [1] 15.82424

# Mediana
median(Monto) # [1] 295.85

