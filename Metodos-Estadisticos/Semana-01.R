# Define path file
FILE_URL <- "https://raw.githubusercontent.com/jgomezz/MsEstadAplic_MetodosEstadisticos/master/data/banco/ansi/Caso_A.csv"
FILE_ENCONDING <- "ISO-8859-1"
#
Banco.data <- read.csv(FILE_URL, header=TRUE, fileEncoding = FILE_ENCONDING )
attach(Banco.data)
head(Banco.data)
table(Sucursal)
table(Sucursal, Genero)


library(readr)
Banco.data <- read_csv(URL_FILE, locale = locale(encoding = "ISO-8859-1"))
View(Banco.data)

attach(Banco.data)
table(Sucursal)
table(Sucursal, Genero)

######################################################################

FILE_URL <- "https://raw.githubusercontent.com/jgomezz/MsEstadAplic_MetodosEstadisticos/master/data/banco/ansi/Caso_A-Etiq.csv"
FILE_ENCONDING <- "ISO-8859-1"
#
Banco.data <- read.csv(FILE_URL, header=TRUE, fileEncoding = FILE_ENCONDING )
attach(Banco.data)
head(Banco.data)

# Elaboracion de una tabla de frecuencias
tabla1 <- table(Banco.data[,1])

# Otra forma 
table(Sucursal)
table(Sucursal, Genero)

# Graficos de R
pie(tabla1, main = " Gráfico Circular")

# Example - Graficos de R
pie.sales <- c(0.12, 0.3, 0.26, 0.16, 0.04, 0.12)
names(pie.sales) <- c("Blueberry", "Cherry",
                      "Apple", "Boston Cream", "Other", "Vanilla Cream")
pie(pie.sales) # default colours


#####################################
# GRAFICOS PARA VARIABLES DISCRETAS #
#####################################

# tabla de frecuencia de una variable cuantitativa discreta
tabla2<-table(NIF)

# Grafico de lineas
# type h es un histograma
#plot(1:4, tabla2, type="h")
plot(tabla2, type="h")

# type b es un histograma
#plot(1:4, tabla2, type="b")
plot(tabla2, type="b")

###################################################
# GRAFICOS PARA VARIABLES CUANTITATIVAS CONTINUAS #
###################################################
# VARIABLE DE INGRESO
# REGLAS DE sTURGES k = 1 + 3.32 LOG10(N)
# RANGO r = Xman - Xmin
# ANCHO w = r/k

# Tabla de frecuencia para datos continuos

# Get the library.
library(agricolae)
h2 <- with(Banco.data, graph.freq(as.numeric(Ingreso), plot = FALSE))
print(table.freq(h2), row.names=FALSE)
hist(Ingreso)
hist(as.numeric(table(Banco.data[,3])))

