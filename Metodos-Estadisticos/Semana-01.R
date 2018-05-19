# Define path file
URL_FILE <- "https://raw.githubusercontent.com/jgomezz/MsEstadAplic_MetodosEstadisticos/master/data/banco/ansi/Caso_A.csv"

# Read file
#Banco.data <- read.table("https://raw.githubusercontent.com/jgomezz/MsEstadAplic_MetodosEstadisticos/master/Caso_A.txt",header=TRUE)
#library(readr)
#dataset <- read_csv(URL_FILE)
#View(dataset)

Banco.data <- read.csv(URL_FILE, fileEncoding = "ISO-8859-1")
attach(Banco.data)
table(Sucursal)
table(Sucursal, Genero)


library(readr)
Banco.data <- read_csv(URL_FILE, locale = locale(encoding = "ISO-8859-1"))
View(Banco.data)

attach(Banco.data)
table(Sucursal)
table(Sucursal, Genero)


