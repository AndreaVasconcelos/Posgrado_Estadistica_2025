# Ejemplo de prueba de T de una muestra

# Solo necesitamos la especie setosa

setosa <- subset(iris, Species == "setosa")
mean(setosa$Sepal.Width)

# mu = 3.9 = Media que ya me dan/establecida

# H0 = la diferencia entre las medias es cero o menor a cero = 3.9
# H1 = la diferencia entre la media teorética y experimental es diferente a cero = diferente a 3.9

t.test(setosa$Petal.Width, mu = 3.9)

# Aceptamos H1: alternative hypothesis: true mean is not equal to 3.9. (nuestra:3.428, mu:3.9)

