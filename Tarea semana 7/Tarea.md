---
title: "Comparación de concentraciones de estroncio en cuerpos de agua"
author: "Andrea Michelle Luna Vasconcelos – 1950889"
date: "22/09/2025"
output: pdf_document
---

## Descripción de los datos

```{r}
estroncio <- read.csv("C:/Repositorio GitHub/Posgrado_Estadistica_2025/Tarea 22_09/Estroncio mg_ml.csv")
library(knitr)
kable(head(estroncio), caption = "Concentración de estroncio (mg/ml) en 
cinco cuerpos de agua (n = 6)")

library(tidyverse)

estroncio_long <- estroncio %>%
  pivot_longer(cols = -Muestra,
               names_to = "Cuerpo_agua",
               values_to = "Concentracion")

estroncio_long$Cuerpo_agua <- as.factor(estroncio_long$Cuerpo_agua)

kable(head(estroncio_long), caption = "Datos reorganizados de concentraciones de
estroncio (mg/ml) en cinco cuerpos de agua como factor")

medias <- tapply(estroncio_long$Concentracion, estroncio_long$Cuerpo_agua, mean)
kable(medias, caption = "Medias de concentración de estroncio (mg/ml) por cuerpo
de agua",
      col.names = c("Cuerpo de agua", "Media (mg/ml)"))

## Hipótesis del ANOVA

H0: La media de concentración de estroncio en todos los cuerpos de agua es igual.
H1: Al menos una media de concentración de estroncio es diferente.

## Cálculo del ANOVA

bartlett.test(estroncio_long$Concentracion ~ estroncio_long$Cuerpo_agua)

estroncio_long.aov <- aov(estroncio_long$Concentracion ~ estroncio_long$Cuerpo_agua) 
summary(estroncio_long.aov)

library(broom)
anova <- tidy(estroncio_long.aov)
kable(anova, caption = "Resultados de análisis de varianza (ANOVA de una vía)")

## Gráfica de distribución

```{r}
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
  theme(plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5))


## Prueba LSD

LSD <- sqrt((2*9.8)/6) * qt(0.975, 25)
LSD

## Comparación por pares con LSD:

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

## Prueba Tukey HSD

PTukey <- sqrt(9.8/6) * qtukey(0.95, nmeans = 5, df = 25)
PTukey

TukeyHSD(estroncio_long.aov)

## Tabla de comparaciones con Tukey:

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
kable(resultadosHSD, caption = "Resultados Tukey HSD de comparación entre pares 
de medias",
      col.names = c("Pares de medias", "Diferencia (mg/ml)", "Resultado Tukey HSD"))

## Comparación LSD vs Tukey HSD

resultadosComparados <- data.frame(
  `Pares de medias` = comparaciones,
  `Diferencia (mg/ml)` = diferencias,
  `Resultado LSD` = resultado_LSD,
  `Resultado Tukey HSD` = resultado_Tukey
)
kable(resultadosComparados,
      caption = "Comparación de resultados LSD vs Tukey HSD entre pares de medias",
      col.names = c("Pares de medias", "Diferencia (mg/ml)", "Resultado LSD", 
      "Resultado Tukey HSD"))

## Interpretación ambiental

¿Qué cuerpo de agua presenta las concentraciones más altas?
Grayson’s Pond y Beaver Lake.

¿Qué sitios no difieren entre sí?
Según LSD: Beaver Lake vs Appletree Lake, Angler’s Cove vs Appletree Lake.
Según Tukey HSD: los mismos de LSD, más Beaver Lake vs Angler’s Cove.

¿Qué implicaciones podrían tener estas diferencias en la calidad del agua?
Las diferencias en las concentraciones de estroncio entre cuerpos de agua 
indican una variabilidad en la calidad. Los sitios con valores más altos podrían
estar expuestos a contaminación local o condiciones específicas, lo que
representa un riesgo potencial para la fauna y flora local. Asimismo, estas 
diferencias pueden tener implicaciones en el uso humano del agua (consumo, 
riego o recreación), por lo que los sitios con mayores concentraciones 
requieren mayor monitoreo y gestión ambiental.

---