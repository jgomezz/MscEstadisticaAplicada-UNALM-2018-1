###############################################################
#                         Practica No1                        #
###############################################################

# Define path file
FILE_URL <- "https://raw.githubusercontent.com/jgomezz/MsEstadAplic_MetodosEstadisticos/master/data/banco/ansi/Caso_1.csv"
FILE_ENCONDING <- "ISO-8859-1"

# Read file
Supermercado.data <- read.csv(FILE_URL, header=TRUE, fileEncoding = FILE_ENCONDING )

attach(Supermercado.data)
summary(Monto)

by(Pago, Local, summary)
#Local: Surco
#Contado TCrédito  TDébito 
#17       26       17 
 
percentaje <- 17/(17+26+17) # 28.33%

table(Local, Monto)

################################################################
# Local de Surco
Supermercado.data.Surco <- subset(Supermercado.data,Local=="Surco")
attach(Supermercado.data.Surco)
table(Supermercado.data.Surco[,4])

# Promedio
mean(Monto) # [1] 297.48

#Coeficiente de Variacion
sd(Monto)*100/abs(mean(Monto)) # [1] 16.23673

# Mediana
median(Monto) #[1] 292

#cuantil
quantile(Monto)
quantile(Monto, 0.8) 
# 80% 
# 333.14 
IQR(Monto)
by(Monto,Local,IQR)

################################################################
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

