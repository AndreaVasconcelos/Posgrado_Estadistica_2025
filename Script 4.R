# Andrea Michelle Luna Vasconcelos 1950889
# Semana 4-Script 4


# Importar ----------------------------------------------------------------

calidad <- read.csv("Calidadplantula.csv", header = T)
View(calidad)
calidad$Tratamiento <- as.factor(calidad$Tratamiento)
class(calidad)
summary(calidad)

mean(calidad$IE)
tapply(calidad$IE, calidad$Tratamiento, mean)
tapply(calidad$IE, calidad$Tratamiento, sd)
tapply(calidad$IE, calidad$Tratamiento, var)


# Boxplot -----------------------------------------------------------------

colores <- c("navajowhite", "skyblue")

boxplot(calidad$IE~ calidad$Tratamiento, col = colores, 
        xlab = "Tratamientos",
        ylab = "Índice de Esbeltez",
        main = "Calidad de Planta en Vivero Forestal",
        ylim = c(0.4,1.2))

# Aplicar subconjunto para cada tratamiento -------------------------------


df_ctrl <- subset(calidad, Tratamiento == "Ctrl")
View(df_ctrl)
df_fert <- subset(calidad, Tratamiento == "Fert")
View(df_fert)

# Agregar el gráfico QQ

par(mfrow=c(1,2))
qqnorm(df_ctrl$IE); qqline(df_ctrl$IE)
qqnorm(df_fert$IE); qqline(df_fert$IE)
par(mfrow=c(1,2))


# Comprobando los 3 supuestos ---------------------------------------------


shapiro.test(df_ctrl$IE) # Normales
shapiro.test(df_fert$IE) # Normales

var.test(calidad$IE ~ calidad$Tratamiento)

t.test(calidad$IE ~ calidad$Tratamiento, alternative = "two.sided", var.equal = T)
 
# Ejemplo erroneo
t.test(calidad$IE ~ calidad$Tratamiento, alternative = "greater", var.equal = T)
# Fin de ejemplo erroneo


# Efecto de Cohen´s -------------------------------------------------------

cohens_efecto <- function(x, y) {
  n1 <- length(x); n2 <- length(y)
  s1 <- sd(x); s2 <- sd(y)
  sp <- sqrt(((n1 - 1) * s1^2 + (n2 - 1) * s2^2) / (n1 + n2 - 2))
  (mean(x) - mean(y)) / sp
}


d1_cal <- cohens_efecto(df_ctrl$IE,df_fert$IE)
d1_cal
