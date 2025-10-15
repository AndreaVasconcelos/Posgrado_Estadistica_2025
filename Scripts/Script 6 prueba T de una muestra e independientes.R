
# Script 6 prueba T de una muestra
# Semana 5 (04/09/25)
# Andrea Michelle Luna Vasconcelos-1950889


# Prueba T de una muestra -------------------------------------------------

# Base de datos Iris= Solo necesitamos la especie setosa

setosa <- subset(iris, Species == "setosa")
mean(setosa$Sepal.Width)

# mu = 3.9 = Media que ya me dan/establecida

# H0 = la diferencia entre las medias es cero o menor a cero = 3.9
# H1 = la diferencia entre la media teorética y experimental es diferente a cero = diferente a 3.9

t.test(setosa$Petal.Width, mu = 3.9)

# Aceptamos H1: alternative hypothesis: true mean is not equal to 3.9. (nuestra:3.428, mu:3.9)


# Prueba T de muestras independientes -------------------------------------

data("iris")
boxplot(iris$Sepal.Length ~ iris$Species)


# Para usar 2 bases de datos, (subset solo funciona para 1)

dat_iris <- subset(iris, Species %in% c("versicolor",
                                        "virginica"))

View(dat_iris)  # In:en tiempo real identifica TRUE los comandos que le dimos.

dat_iris$Species <- droplevels(dat_iris$Species) # Para quitar "setosa" de la gráfica de la base de datos original.
boxplot(dat_iris$Sepal.Length ~ dat_iris$Species)

boxplot(setosa$Sepal.Length) # Ejemplo para ver que solo se grafica setosa.

# Ejemplo correcto
# H0 = la media de SL de virginica es mayor que la media de versicolor
# H1 = 

t.test(dat_iris$Sepal.Length ~ dat_iris$Species,
       alternative = "greater", var.equal =T)
5.936-6.588

tapply(dat_iris$Sepal.Length, dat_iris$Species, mean)
tapply(dat_iris$Sepal.Length, dat_iris$Species, sd)

# Ejemplo erroneo, si queremos le decimos lo contrario
# H0 = la media de SL de virginica es mayor que la media de versicolor
# H1 = 

t.test(dat_iris$Sepal.Length ~ dat_iris$Species,
       alternative = "less", var.equal =T)

# Ejemplo erroneo de 2 colas
# H0 = la media de SL de virginica es mayor que la media de versicolor
# H1 = 

t.test(dat_iris$Sepal.Length ~ dat_iris$Species,
       alternative = "two.sided", var.equal =T)

