300+240+1527+400+1500+1833
celular <- 300
celular
transporte <- 240
comestibles <- 1527
gimnasio <- 400
alquiler <- 1500
otros <- 1833

5800*5
5800*10

abs(10)
abs(-4)
sqrt(9)
log(2)

# Este es un comentario
2+9 # aquí tambien se puede poner un comentario

celular
Celular <- -300
CELULAR <- 800

celular+Celular+CELULAR
Celular-CELULAR

help(abs)
help(mean)
?mean
help.search("absolute")

gastos <- c(celular, transporte, comestibles, gimnasio, alquiler, otros)

gastos

barplot(gastos)

sort(gastos)
gastosOrdenados <- sort(gastos)
barplot(gastosOrdenados)

help(sort)
sort(gastos,decreasing = TRUE)
gastos_ordenados <- sort(gastos,decreasing = TRUE)
barplot(gastos_ordenados)        

help("barplot")
barplot(gastos_ordenados,names.arg = gastos_ordenados)

names(gastos_ordenados) <- c("Otros","Comestibles","Alquiler","Gimnasio","Celular","Transporte")
barplot(gastos_ordenados, 
        main = "Gastos mensules",
        names.arg = names(gastos_ordenados),
        col = "purple")

# Laboratorio terminado
