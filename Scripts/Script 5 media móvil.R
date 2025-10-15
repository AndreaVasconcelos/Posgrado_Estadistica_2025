
# Script 5 media móvil
# Semana 5 (04/09/25)
# Andrea Luna Vasconcelos 1950889


data("iris")
View(Iris)

setosa <- subset(Iris, Species == "setosa")
View(setosa)

 # Sumatoria acumulada de la variable longitud

acum <- cumsum(setosa$Sepal.Length)
acum # Sumatoria acumulada
cont <- seq(1:length(setosa$Sepal.Length)) # Continuidad
cont

# Crear data.frame --------------------------------------------------------

set_mov <- data.frame(cont, acum)
View(set_mov)
set_mov$movil <- round(acum/cont,2)

plot(cont, set_mov$movil, type = "l",
     xlab = "Número de observaciones",
     ylab = "Media móvil")
abline(h=mean(setosa$Sepal.Length),
       col = "indianred")

mean(setosa$Sepal.Length)
