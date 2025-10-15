
# Ejercicio:Comparación de concentraciones de estroncio en cuerpos de agua/anova
# Semana 6 (11/09/2025)
# Andrea Michelle Luna Vasconcelos 1950889


# Comparación de concentraciones de estroncio en cuerpos de agua

# Descripción-Datos a trabajar --------------------------------------------

estroncio <- read.csv("C:/Repositorio GitHub/Posgrado_Estadistica_2025/Tarea 22_09/Estroncio mg_ml.csv")
View(estroncio)

library(knitr)
kable(head(estroncio), caption = "Concentración de estroncio (mg/ml)
      en cinco cuerpos de agua (n=6)")


library(tidyverse)

estroncio_long <- estroncio %>%
  pivot_longer(cols= -Muestra,
               names_to = "Cuerpo_agua",
               values_to = "Concentracion")
view(estroncio_long)

# Convertir en factor el cuerpo de agua

estroncio_long$Cuerpo_agua <- as.factor(estroncio_long$Cuerpo_agua)

kable(head(estroncio_long), caption = "Datos reorganizados de concentraciones
      de estroncio (mg/ml) en cinco cuerpos de agua como factor")

summary(estroncio_long)
tapply(estroncio_long$Concentracion, estroncio_long$Cuerpo_agua, mean)

# Angler.s.Cove   Appletree.Lake    Beaver.Lake  Grayson.s.Pond   Rock.River 
#  44.08333        41.10000         40.23333       32.08333       58.30000

  

# Preguntas ---------------------------------------------------------------

# Hipótesis del ANOVA -----------------------------------------------------

# H0 = La media de concentración de estroncio en todos los cuerpos de agua son 
# iguales.
# H1 = Al menos una media de concentración de estroncio es diferente al resto
# de las medias de los cuerpos de agua.

# Cálculo del ANOVA -------------------------------------------------------

bartlett.test(estroncio_long$Concentracion ~ estroncio_long$Cuerpo_agua)
  # p vale = 0.9586, son homogeneas
 
estroncio_long.aov <- aov(estroncio_long$Concentracion ~ estroncio_long$Cuerpo_agua) 
summary(estroncio_long.aov)
  
library(broom)
library(knitr)  

anova <- tidy(estroncio_long.aov)
kable(anova, caption = "Resultados de análisis de varianza (ANOVA de una vía)")  

# H0 = (medias iguales) = se rechaza
# H1 = (al menos una media distinta) = se acepta

# Prueba LSD --------------------------------------------------------------

library(ggplot2)

ggplot(estroncio_long, aes(x = Cuerpo_agua, y = Concentracion, fill = Cuerpo_agua)) +
  geom_violin(trim = FALSE, alpha = 0.5) +
  geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA) +
  geom_jitter(width = 0.1, size = 2, alpha = 0.7) +
  labs(
    title = "Concentraciones de estroncio en cuerpos de agua",
    x = "Cuerpo de agua",
    y = "Concentración (mg/ml)",
    fill = "Cuerpos de agua",
    caption = "Figura 1. Concentraciones de estroncio en cinco cuerpos de agua"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),   
    plot.subtitle = element_text(hjust = 0.5), 
    plot.caption = element_text(hjust = 0.5)   
  )

# k(k-1)/2 pares de medias > 5(5-1)/2 = 5(4)/2 = 10 posibles pares de medias.


# Posibles pares de medias ------------------------------------------------

#**Posibilidad 1**
# H0 = μ(Grayson’s Pond) = μ(Beaver Lake)
# H1 = μ(Grayson’s Pond) ≠ μ(Beaver Lake)

#**Posibilidad 2**
# H0 = μ(Grayson’s Pond) = μ(Angler’s Cove)
# H1 = μ(Grayson’s Pond) ≠ μ(Angler’s Cove)

#**Posibilidad 3**
# H0 = μ(Grayson’s Pond) = μ(Appletree Lake)
# H1 = μ(Grayson’s Pond) ≠ μ(Appletree Lake)

#**Posibilidad 4**
# H0 = μ(Grayson’s Pond) = μ(Rock River)
# H1 = μ(Grayson’s Pond) ≠ μ(Rock River)

#**Posibilidad 5**
# H0 = μ(Beaver Lake) = μ(Angler’s Cove)
# H1 = μ(Beaver Lake) ≠ μ(Angler’s Cove)

#**Posibilidad 6**
# H0 = μ(Beaver Lake) = μ(Appletree Lake)
# H1 = μ(Beaver Lake) ≠ μ(Appletree Lake)

#**Posibilidad 7**
# H0 = μ(Beaver Lake) = μ(Rock River)
# H1 = μ(Beaver Lake) ≠ μ(Rock River)

#**Posibilidad 8**
# H0 = μ(Angler’s Cove) = μ(Appletree Lake)
# H1 = μ(Angler’s Cove) ≠ μ(Appletree Lake)

#**Posibilidad 9**
# H0 = μ(Angler’s Cove) = μ(Rock River)
# H1 = μ(Angler’s Cove) ≠ μ(Rock River)

#**Posibilidad 10**
# H0 = μ(Appletree Lake) = μ(Rock River)
# H1 = μ(Appletree Lake) ≠ μ(Rock River)

qt(0.975, 25) # 0.975 %confianza, 25(30 resultados-5 factores)
# 2.059539

sqrt((2*9.8)/6)*qt(0.975, 25) # 2(qt),9.8(mean sq)/6(muestras)
#**3.722394** valor mínimo entre medias
# 3.72 mg/ml

tapply(estroncio_long$Concentracion, estroncio_long$Cuerpo_agua, mean)

library(dplyr)
library(knitr)

medias <- tapply(estroncio_long$Concentracion, estroncio_long$Cuerpo_agua, mean)
kable(medias, caption = "Medias de concentración de estroncio (mg/ml) por cuerpo de agua",
      col.names = c("Cuerpo de agua", "Media (mg/ml)"))

LSD <- 3.72

# Diferencia entre medias -------------------------------------------------

Posibilidad1 <- medias["Grayson.s.Pond"] - medias["Beaver.Lake"]
Posibilidad1; ifelse(abs(Posibilidad1) > LSD, "Significativa", "No significativa")
# -8.15 Significativa

Posibilidad2 <- medias["Grayson.s.Pond"] - medias["Angler.s.Cove"]
Posibilidad2; ifelse(abs(Posibilidad2) > LSD, "Significativa", "No significativa")
# -12.0 Significativa

Posibilidad3 <- medias["Grayson.s.Pond"] - medias["Appletree.Lake"]
Posibilidad3; ifelse(abs(Posibilidad3) > LSD, "Significativa", "No significativa")
# -9.017 Significativa

Posibilidad4 <- medias["Grayson.s.Pond"] - medias["Rock.River"]
Posibilidad4; ifelse(abs(Posibilidad4) > LSD, "Significativa", "No significativa")
# -26.217 Significativa

Posibilidad5 <- medias["Beaver.Lake"] - medias["Angler.s.Cove"]
Posibilidad5; ifelse(abs(Posibilidad5) > LSD, "Significativa", "No significativa")
# -3.85 Significativa

Posibilidad6 <- medias["Beaver.Lake"] - medias["Appletree.Lake"]
Posibilidad6; ifelse(abs(Posibilidad6) > LSD, "Significativa", "No significativa")
# -0.867 No significativa

Posibilidad7 <- medias["Beaver.Lake"] - medias["Rock.River"]
Posibilidad7; ifelse(abs(Posibilidad7) > LSD, "Significativa", "No significativa")
# -18.067 Significativa

Posibilidad8 <- medias["Angler.s.Cove"] - medias["Appletree.Lake"]
Posibilidad8; ifelse(abs(Posibilidad8) > LSD, "Significativa", "No significativa")
# 2.983 No significativa

Posibilidad9 <- medias["Angler.s.Cove"] - medias["Rock.River"]
Posibilidad9; ifelse(abs(Posibilidad9) > LSD, "Significativa", "No significativa")
# -14.217 Significativa

Posibilidad10 <- medias["Appletree.Lake"] - medias["Rock.River"]
Posibilidad10; ifelse(abs(Posibilidad10) > LSD, "Significativa", "No significativa")
# -17.2 Significativa

library(knitr)

resultados <- data.frame(
  Comparacion = c(
    "Grayson’s Pond vs Beaver Lake",
    "Grayson’s Pond vs Angler’s Cove",
    "Grayson’s Pond vs Appletree Lake",
    "Grayson’s Pond vs Rock River",
    "Beaver Lake vs Angler’s Cove",
    "Beaver Lake vs Appletree Lake",
    "Beaver Lake vs Rock River",
    "Angler’s Cove vs Appletree Lake",
    "Angler’s Cove vs Rock River",
    "Appletree Lake vs Rock River"
  ),
  Diferencia = c(-8.15, -12.00, -9.017, -26.217, -3.85, -0.867, -18.067, 2.983, -14.217, -17.2),
  Significancia = c("Significativa", "Significativa", "Significativa", "Significativa",
                    "Significativa", "No significativa", "Significativa",
                    "No significativa", "Significativa", "Significativa")
)

kable(resultados, caption = "Resultados LSD de comparación entre pares de medias",
      col.names = c("Pares de medias", "Diferencia (mg/ml)", "Resultado LSD"))

# Tukey HSD ---------------------------------------------------------------

sqrt(9.8/6) * qtukey(0.95, nmeans = 5, df = 25)
# 5.308078

TukeyHSD(estroncio_long.aov)
plot(TukeyHSD(estroncio_long.aov), las = 1, cex.axis = 0.6)

PTukey <- 5.31

Posibilidad1HSD <- medias["Grayson.s.Pond"] - medias["Beaver.Lake"]
Posibilidad1HSD; ifelse(abs(Posibilidad1HSD) > PTukey, "Significativa", "No significativa")
# -8.15 Significativa

Posibilidad2HSD <- medias["Grayson.s.Pond"] - medias["Angler.s.Cove"]
Posibilidad2HSD; ifelse(abs(Posibilidad2HSD) > PTukey, "Significativa", "No significativa")
# -12.0 Significativa

Posibilidad3HSD <- medias["Grayson.s.Pond"] - medias["Appletree.Lake"]
Posibilidad3HSD; ifelse(abs(Posibilidad3HSD) > PTukey, "Significativa", "No significativa")
# -9.017 Significativa

Posibilidad4HSD <- medias["Grayson.s.Pond"] - medias["Rock.River"]
Posibilidad4HSD; ifelse(abs(Posibilidad4HSD) > PTukey, "Significativa", "No significativa")
# -26.217 Significativa

Posibilidad5HSD <- medias["Beaver.Lake"] - medias["Angler.s.Cove"]
Posibilidad5HSD; ifelse(abs(Posibilidad5HSD) > PTukey, "Significativa", "No significativa")
# -3.85 No significativa

Posibilidad6HSD <- medias["Beaver.Lake"] - medias["Appletree.Lake"]
Posibilidad6HSD; ifelse(abs(Posibilidad6HSD) > PTukey, "Significativa", "No significativa")
# -0.867 No significativa

Posibilidad7HSD <- medias["Beaver.Lake"] - medias["Rock.River"]
Posibilidad7HSD; ifelse(abs(Posibilidad7HSD) > PTukey, "Significativa", "No significativa")
# -18.067 Significativa

Posibilidad8HSD <- medias["Angler.s.Cove"] - medias["Appletree.Lake"]
Posibilidad8HSD; ifelse(abs(Posibilidad8HSD) > PTukey, "Significativa", "No significativa")
# 2.983 No significativa

Posibilidad9HSD <- medias["Angler.s.Cove"] - medias["Rock.River"]
Posibilidad9HSD; ifelse(abs(Posibilidad9HSD) > PTukey, "Significativa", "No significativa")
# -14.217 Significativa

Posibilidad10HSD <- medias["Appletree.Lake"] - medias["Rock.River"]
Posibilidad10HSD; ifelse(abs(Posibilidad10HSD) > PTukey, "Significativa", "No significativa")
# -17.2 Significativa

library(knitr)

resultadosHSD <- data.frame(
  Comparacion = c(
    "Grayson’s Pond vs Beaver Lake",
    "Grayson’s Pond vs Angler’s Cove",
    "Grayson’s Pond vs Appletree Lake",
    "Grayson’s Pond vs Rock River",
    "Beaver Lake vs Angler’s Cove",
    "Beaver Lake vs Appletree Lake",
    "Beaver Lake vs Rock River",
    "Angler’s Cove vs Appletree Lake",
    "Angler’s Cove vs Rock River",
    "Appletree Lake vs Rock River"
  ),
  Diferencia = c(-8.15, -12.00, -9.017, -26.217, -3.85, -0.867, -18.067, 2.983, -14.217, -17.2),
  Significancia = c("Significativa", "Significativa", "Significativa", "Significativa",
                    "No significativa", "No significativa", "Significativa",
                    "No significativa", "Significativa", "Significativa")
)

kable(resultadosHSD, caption = "Resultados Tukey HSD de comparación entre pares de medias",
      col.names = c("Pares de medias", "Diferencia (mg/ml)", "Resultado Tukey HSD"))


# Comparación entre LSD vs TukeyHSD ---------------------------------------

library(knitr)

comparaciones <- c(
  "Grayson’s Pond vs Beaver Lake",
  "Grayson’s Pond vs Angler’s Cove",
  "Grayson’s Pond vs Appletree Lake",
  "Grayson’s Pond vs Rock River",
  "Beaver Lake vs Angler’s Cove",
  "Beaver Lake vs Appletree Lake",
  "Beaver Lake vs Rock River",
  "Angler’s Cove vs Appletree Lake",
  "Angler’s Cove vs Rock River",
  "Appletree Lake vs Rock River"
)

diferencias <- c(-8.15, -12.00, -9.017, -26.217, -3.85, -0.867, -18.067, 2.983, -14.217, -17.2)

resultado_LSD <- c("Significativa", "Significativa", "Significativa", "Significativa",
                   "Significativa", "No significativa", "Significativa",
                   "No significativa", "Significativa", "Significativa")

resultado_Tukey <- c("Significativa", "Significativa", "Significativa", "Significativa",
                     "No significativa", "No significativa", "Significativa",
                     "No significativa", "Significativa", "Significativa")

resultadosComparados <- data.frame(
  `Pares de medias` = comparaciones,
  `Diferencia (mg/ml)` = diferencias,
  `Resultado LSD` = resultado_LSD,
  `Resultado Tukey HSD` = resultado_Tukey
)


kable(resultadosComparados,
      caption = "Comparación de resultados LSD vs Tukey HSD entre pares de medias",
      col.names = c("Pares de medias", "Diferencia (mg/ml)", "Resultado LSD", "Resultado Tukey HSD"))


# Interpretación ----------------------------------------------------------

# ¿Qué cuerpo de agua presenta las concentraciones más altas?
# Grayson’s Pond y Beaver Lake.

# ¿Qué sitios no difieren entre sí?
# Según LSD: Beaver Lake vs Appletree Lake, Angler’s Cove vs Appletree Lake.
# Según Tukey HSD: los mismos de LSD, más Beaver Lake vs Angler’s Cove.

# ¿Qué implicaciones podrían tener estas diferencias en la calidad del agua?
# Las diferencias en las concentraciones de estroncio entre cuerpos de agua 
# indican una variabilidad en la calidad. Los sitios con valores más altos podrían
# estar expuestos a contaminación local o condiciones específicas, lo que
# representa un riesgo potencial para la fauna y flora local. Asimismo, estas 
# diferencias pueden tener implicaciones en el uso humano del agua (consumo, 
# riego o recreación), por lo que los sitios con mayores concentraciones 
# requieren mayor monitoreo y gestión ambiental.

