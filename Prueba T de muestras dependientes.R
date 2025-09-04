# Prueba de t de muestras dependientes

# Revisar la producción del kg de semilla para el año 2012 y 2013.

prod <- read.csv("mainproduccion.csv", header = T)
View(prod)

prod$Tiempo <- as.factor(prod$Tiempo)
boxplot(prod$Kgsem ~ prod$Tiempo)
tapply(prod$Kgsem, prod$Tiempo, mean)
10.1066-10.8954


# Hipotesis (Andrea) ---------------------------------------------------------------

# H0: la media de producción del 2012 es mayor que la media de producción de 2013.
# H1: la media de producción del 2012 es menor que la media de producción de 2013.
t.test(prod$Kgsem ~ prod$Tiempo,
       alternative = "greater", var.equal =T)

# Acepta la H1: t = -1.2998, df = 98, p-value = 0.9016: alternative hypothesis: true difference in means between group T2012 and group T2013 is greater than 0

# 2013 - 2012 = 0.7888
10.8954-10.1066


# Ejemplo del Dr. Tagle ---------------------------------------------------


# H0: las medias de ambos años son iguales a cero
# H1: las medias de ambos años son diferentes a cero

# Método alternativo two.sided

var.test(prod$Kgsem ~ prod$Tiempo)
t.test(prod$Kgsem ~ prod$Tiempo,
       alternative = "two.sided",
       var.equal = T,
       paired =T) # No funciona al poner paired

# Acepta la H1:t = -1.2998, df = 98, p-value = 0.1967, alternative hypothesis: true difference in means between group T2012 and group T2013 is not equal to 0
 
# Como no funcionó paired, lo separamos como 2 individuales y ahora si hacer la prueba de t con los individuales.

t2012 <- subset(prod$Kgsem, prod$Tiempo == "T2012")
t2013 <- subset(prod$Kgsem, prod$Tiempo == "T2013")

t.test(t2012, t2013,
       alternative = "two.sided",
       var.equal = T,
       paired =T)
# Aceptas la H0, porque la diferencia entre las medias no es significativa: p-value = 0.2159 alternative hypothesis: true mean difference is not equal to 0

# Corrección Andrea -------------------------------------------------------

# H0: la media de producción del 2012 es mayor que la media de producción de 2013.
# H1: la media de producción del 2012 es menor que la media de producción de 2013.

t.test(t2012, t2013,
       alternative = "greater",
       var.equal = T,
       paired =T)

# No es estadísticamente signficativo.




