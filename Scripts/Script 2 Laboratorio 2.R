
# Laboratorio 2: Empezar con R y Rstudio
# Semana 2 (14/08/2025) y Semana 3 (21/08/2025)
# Andrea Michelle Luna Vasconcelos 1950889


# Laboratorio 2-Parte 1 ---------------------------------------------------

# Importar datos web

read.csv("https://repodatos.atdt.gob.mx/api_update/senasica/actividades_\
         inspeccion_movilizacion/29_actividades-inspeccion-movilizacion.csv")

url <- paste0("https://repodatos.atdt.gob.mx/api_update/senasica/",
  "actividades_inspeccion_movilizacion/29_actividades-inspeccion-movilizacion.csv")
browseURL(url)
senasica <- read.csv(url, header = T)

View(senasica)

url2 <- ("https://repodatos.atdt.gob.mx/api_update/senasica/actividades\inspeccion_movilizacion29_actividades-inspeccion\-movilizacion.csv")
browseURL(url)
senasica <- read.csv(url, header = T)
senasica2 <- read.csv(url2, header = T)
head(senasica2[, c(1,3:12)])

# Laboratorio 2-Parte 2 ---------------------------------------------------

temperatura <- read.csv("C:/Repositorio GitHub/Posgrado_Estadistica_2025/temperatura.csv")
View(temperatura)

head(temperatura) #Primeras 6 filas
dim(temperatura) #Números de filas y columnas
names(temperatura) #Nombre de las columnas
str(temperatura) #Estructura del data frame

summary(temperatura) #Resumen estadístico 
names(temperatura) <- c("Anual", "Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic") #Corregir nombre de columna anual

temperatura$media_anual <- rowMeans(
  temperatura[,2:13])
head(temperatura)

rowMeans(temperatura)


# Creación y edición de gráficas ------------------------------------------

# Crear gráficas con los datos

temp <- temperatura[ ,2:13]

boxplot(temp)

write.csv(temperatura, "temp_final.csv")
temp10 <- temperatura[11:21,2:13]
temperatura[2,2]

# Personalizar la gráfica con la temperatura de 20 años de 2000 a 2020.


colores <- c("salmon","navajowhite","skyblue")

boxplot(temp, col = colores,
        main = "Comportamiento temperatura (2000 a 2020)",
        xlab = "Meses", 
        ylab = "Temperatura (°C)")

# Personalizar la gráfica con la temperatura de 10 años de 2010 a 2020.

boxplot(temp10, col = colores, 
        main = "Comportamiento temperatura (2010 a 2020)",
        xlab = "Meses",
        ylab = "Temperatura (°C)")


