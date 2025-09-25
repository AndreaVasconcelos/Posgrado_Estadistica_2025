# Andrea Michelle Luna Vasconcelos 1950889
#**Regresión lineal simple (sigue un patrón lineal entre sus variables)**


# Regresión ---------------------------------------------------------------

# X: variable independiente/explicativa/endógena
# Y: variable dependiente/respuesta/endógena
# El objetivo es tener estimaciones de Y para distintos valores de X a partir 
# de n pares de valores

# Causalidad --------------------------------------------------------------

# Hay una relación en X y Y pero tambien puede haber otra causa de esos resultados.
#**Es saber si esa variable si afecta a la otra.**
# "No todo lo que tiene correlación tiene causalidad"

# Tipos de relación -------------------------------------------------------

# No determinista: Conociendo X, el valor de Y no queda perfectamente establecido.
# Y=f(x)+u

# Lineal: cuando la función F(x) es lineal
# f(x)=B0+B1x
#**B0 es cuando la X vale 0 (intercepto) y B1 es la (pendiente)** (Pendiente/B1= Si aumento X, cuánto se aumenta en Y?)
# Si B0 <0 es lineal positiva.
# Si B0 >0 es lineal negativa.

# No lineal: cuando f(x) no es lineal
# Es una función logarítmica

# Ausencia de relación: f(x)=0 =no hay correlación

# Mientras más llega a 1, es más correlación, 0 es que no hay relación.


# Modelo de regresión lineal simple ---------------------------------------

# (y prima) Yi=B0+B1xi+Ei
# Error=residuo ei= Yi-Yi(prima)


# Supuestos ---------------------------------------------------------------

# 1: Linealidad: relación entre x y y lineal.
# 2: Homogeneidad: valor promedio del error = 0.
# 3: Homocedasticidad: la varianza de los errores es constante.
# 4: Independencia: observaciones independientes.
# 5: Normalidad: errores siguen una distribución lineal.


# Estimadores de mínimos cuadrados ----------------------------------------

#**Gauss 1809**
# B0 y B1
# Minimizar la suma de los residuos al cuadrado = residuales al cuadrado y su sumatoria.

# Ejercicio 1 -------------------------------------------------------------

datos <- data.frame(
  trigo = c(30, 28, 32, 25, 25, 25, 22, 24, 35, 40),
  harina = c(25, 30, 27, 40, 42, 40, 50, 45, 30, 25)
)
datos

sumxi_yi <- sum(datos$trigo*datos$harina)
sumxi_yi # =9734
nxy <- length(datos$trigo)*mean(datos$trigo)*mean(datos$harina)
nxy
sumxi_yi-nxy # =-390.4

xi2 <- sum(datos$trigo^2)
xi2 # =8468

nx2 <- length(datos$trigo)*mean(datos$trigo)^2
nx2 # =81796

b1 <- (sumxi_yi - nxy)/(xi2-nx2)
b1 # =-1.353675

b0 <- mean(datos$harina) - b1*mean(datos$trigo)
b0 # 74.11512

Gráfica # en el modelo lineal primero variable dependiente y luego independiente

fit.lm <- lm(datos$harina ~ datos$trigo)
fit.lm
summary(fit.lm)

# Coefficients: (Intercept) B0 = intercepto  74.1151 =2.85e-05 *** muy significativo 
#      datos$trigo  -1.3537 B1 = pendiente   -1.3537 =0.00198 **   muy significativo

anova(fit.lm)
# p-value: 0.001978
# Homocedasticidad=
# mean sq = 25.99 (varianza entre los residuales)
# suma de cuadrados del error = 207.93


# InterpretacióN de varianza de los errores: (foto) 

install.packages("lmtest")
library(lmtest)

# modelo, es lo mismo a fit.lm
m <- lm(datos$harina ~ datos$trigo)
bptest(m) # Breusch-Pagan = homocedasticidad
bptest(fit.lm) # p-value = 0.5641
# Se acepta la H0 es homocedastica

fit.lm$model
fit.lm$coefficients
fit.lm$residuals
mean(fit.lm$residuals) # media de los residuales es casi 0 = 4.883247e-16

# (En la tesis si vas a hacer una, primero explica que se cumplen todos los supuestos y los valores, de B0 y B1)

sqrt(0.6820) # sqrt de r
# coeficiente de correlación = varianza = 0.8258329 = **linealidad** muy cercano a 1.
# correlación de -1 a 1
# Se acepta que es lineal y tiene una ccorrelación muy alta.

# Fuerza de correlación, 0-0.1 indica que no existe, valor entre 0.7-1 es correlación muy alta.

# SSE = 207.9
mean(fit.lm$residuals)
datos$yprima <- 74.11 - 1.3536 * datos$trigo # b0-b1*datos$trigo
datos$yprima
datos$recta <- fit.lm$fitted.values
datos$recta

datos$residuales <- datos$harina-datos$recta
datos$residuales

sum(datos$residuales^2) # 207.9251 = SSE

SSE <- sum(datos$residuales^2) # suma de cuadrados del error.
SSE/8 # 25.99064 varianza de los errores.
sqrt(SSE/8) # 5.098101 desviación estandar residual.
