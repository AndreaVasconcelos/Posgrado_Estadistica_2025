# Andrea Michelle Luna Vasoncelos 1950889
# Análisis de varianza-Script 8?
# Productividad de los cultivos
# 18/0972025

# Base de datos -----------------------------------------------------------

crop <- read.csv("crop.data.csv")
View(crop)

summary(crop)

# Las variables las marca como integrales (en environment) pero debemos pasarlos a fator:

crop$density <- as.factor(crop$density)
crop$block <- as.factor(crop$block)
crop$fertilizer <- as.factor(crop$fertilizer)

summary(crop) # ya nos da el summary por factor


# Boxplot de factores -----------------------------------------------------

colores <- c("indianred", "skyblue", "navajowhite")

boxplot(crop$yield ~ crop$fertilizer,
        col = colores,
        xlab = "Fertilizante",
        ylab = "Productividad (ton/ha)", 
        main = "Boxplot de 1 factor")

tapply(crop$yield, crop$fertilizer, mean)
tapply(crop$yield, crop$fertilizer, var)

# Análisis estadísticos  --------------------------------------------------


# Aplicar prueba de normalidad de datos

shapiro.test(crop$yield) # se cumple en todos los datos (p value 0.6135)

shapiro.test(subset(crop$yield, crop$fertilizer == "1")) # fertilizante combinando subset y shapiro
shapiro.test(subset(crop$yield, crop$fertilizer == "2"))
shapiro.test(subset(crop$yield, crop$fertilizer == "3")) # todos son normales

# Prueba de Bartlett

bartlett.test(crop$yield ~ crop$fertilizer) # sirve para al menos 3 tratamientos que comparar
var.test(crop$yield ~ crop$fertilizer) # var.test solo funciona para 2 factores


# Tabla de ANOVA ----------------------------------------------------------

# H0 = la M1 = M2 = M3
# H1 = M1 = M2 =/ M3 (en al menos una hay diferencia)

# Grados de libertad = total de mis datos-el número de tratamientos

# Cuando el cuadro medio del error es más grande que el cuadro del tratamiento significa que no hay diferencias.
# si el valor de abajo es más grande que el de arriba no hay diferencia 
# en este caso el de abajo es menor, así que si hay diferencia y se acepta la H1

# Prueba de ANOVA

crop.aov <- aov(crop$yield ~ crop$fertilizer) # 1 factor
summary(crop.aov)

crop.aov <- aov(crop$yield ~ crop$fertilizer + crop$block) # 2 factores
summary(crop.aov)

crop.aov <- aov(crop$yield ~ crop$fertilizer + crop$density) # 2 factores
summary(crop.aov) 
# nos dice que ambos tienen diferencia

# Interacciones (dificil de explicar)

crop.aov <- aov(crop$yield ~ crop$fertilizer * crop$block) # 2 factores
summary(crop.aov) # ver la interacción de los bloques
# nos dice que los bloques no influyen

crop.aov <- aov(crop$yield ~ crop$fertilizer * crop$density) # 2 factores
summary(crop.aov)

# Volvemos a trabajar con lo sencillo:

crop.aov <- aov(crop$yield ~ crop$fertilizer) # 1 factor
summary(crop.aov) # es altamente diferente entre medias 

# Para saber cuales son las diferentes puedes usar LSD o Tukey
# cuadrado medio del error = 0

# Todo esto es con el Fertilizante ----------------------------------------

# Prueba LSD --------------------------------------------------------------

# LSD nos dice cual es el número mínimo que debe existir para que mis muestras sean diferentes
# k(k-1)/2 pares de medias k=3 existen 3 x 2/2 = 3 posibles pares de medias

#**Posibilidad 1**
# H0 = M1 = M2
# H1 = M1 ≠ M2

#**Posibilidad 2**
# H0 = M2 = M3
# H1 = M2 ≠ M3

#**Posibilidad 3**
# H0 = M1 = M3
# H1 = M1 ≠ M3

qt(0.975, 93)

sqrt((2*0.3859)/32)*qt(0.975, 93)
# = **0.3083992** este es el valor mínimo que debe haber entre medias para que sean diferentes


tapply(crop$yield, crop$fertilizer, mean)
# diferencia de medias F1 vs F2
176.757-176.9332 # = -0.1762 = NO hay diferencia entre M1 y M2

# diferencia de medias F2 vs F3
176.9332-177.3562 # = -0.423 = SI hay diferencia entre M2 y M3

# diferencia de medias F1 vs F3
176.7570-177.3562 # = -0.5992 = SI hay diferencia entre M1 y M3

# El único que no tuvo diferencia fue F1

# Prueba de Tukey ---------------------------------------------------------

# qtukey(0.95, means = 3, df = 93)
sqrt((0.3859)/32)*qtukey(0.95, nmeans = 3, df = 93)
# = **0.3699006** este es el valor mínimo que debe haber entre medias para que sean diferentes
# = 500 kg

# Con Tukey me diría que solo el F1 vs F3 tienen diferencia significativa

# Prueba gráfica de Tukey

TukeyHSD(crop.aov) # usamos el objeto de análisis de varianza (crop.aov)
# En los que son positivos, si hay diferencia significativa, en el que tiene negativo (atraviesa el 0) no tiene diferencia significativa
# Cualquier línea que toque el 0 = no hay diferencia

plot(TukeyHSD(crop.aov)) # Los datos varían porque usamos TukeyHSD en esta gráfica


# Hacer lo mismo pero con bloques desde el boxplot y todo lo demás (6 medias) 4+3 = 12 / 2 = 6


# Gráfica de violín -------------------------------------------------------


library(ggplot2)
ggplot(crop, aes(x=fertilizer, y=yield, fill = 
                   fertilizer))+
  geom_violin()+
  geom_jitter()+
  geom_boxplot(width = 0.1, col = "white", alpha = 0.5)+ # se ve feo y aún no lo quiero usar, debo personalizarlo
  theme_light()+
  labs(x = "Fertilizante",
       y = "Rendimiento (ton/ha)")
