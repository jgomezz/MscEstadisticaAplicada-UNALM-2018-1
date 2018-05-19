

## Lectura de datos
UN.data <- read.table(file = "http://tarwi.lamolina.edu.pe/~clopez/Regresion/UN.txt", header = T)

##  en caso no se usa attach, se puede usar el $ UN.data$Fertilidad
attach(UN.data)

head(UN.data)

pairs(Fertilidad ~ PBIpp +  Purban)

pairs(Fertilidad ~ PBIpp +  Purban, data = UN.data)

## Se usan logaritmicoas

pairs(log(Fertilidad) ~ log2(PBIpp) +  Purban, data = UN.data)
