temperatura <- read.csv("C:/Repositorio GitHub/Posgrado_Estadistica_2025/temperatura.csv")
View(temperatura)

head(temperatura) #Primeras 6 filas
dim(temperatura) #Números de filas y columnas
names(temperatura) #Nombre de las columnas
str(temperatura) #Estructura del data frame

summary(temperatura) #Resumen estadístico 
names(temperatura) <- c("Anual", "Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic") #Corregir nombre de columna anual

temperatura$media_anual <- rowMeans(
  temperatura[11:21,])
head(temperatura)
