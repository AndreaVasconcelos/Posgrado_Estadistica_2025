# Andrea Luna Vasconcelos 1950889
# Asignación 3: Contraste de medias (03/09/2025)
# Tarea 2

# Base de datos Iris ------------------------------------------------------


data("iris")
head(iris)
str(iris)
summary(iris)
View(iris)

Iris <- iris
knitr::kable(iris[1:10, ], caption = "Cuadro 1: Muestra de las características
de las tres especies de iris")

Iris$Species <- factor(Iris$Species, 
                       levels = c("virginica", "versicolor", "setosa"))

install.packages("ggplot2")
library(ggplot2)


ggplot(Iris, aes(x = Sepal.Length, y = Species, fill = Species)) +
  geom_boxplot() +
  coord_flip() + 
  scale_fill_manual(values = c("tomato", "seagreen", "skyblue"))+
  labs(title = "Distribución del largo de pétalo por especie",
    x = "Longitud (cm)",
    y = "Especies")


ggplot(Iris, aes(x = Sepal.Width, y = Species, fill = Species)) +
  geom_boxplot() +
  coord_flip() + 
  scale_fill_manual(values = c("tomato", "seagreen", "skyblue"))+
  labs(title = "Distribución del ancho de pétalo por especie",
       x = "Diámetro (cm)",
       y = "Especies")

# Datos a trabajar --------------------------------------------------------
# Selección de especies versicolor y virginica

data_sub <- subset(Iris, Species %in% c("versicolor", "virginica"))

head(data_sub)
table(data_sub$Species)

# Explorar la base de datos

tapply(data_sub$Petal.Length, data_sub$Species, summary)
tapply(data_sub$Petal.Length, data_sub$Species, mean)
tapply(data_sub$Petal.Length, data_sub$Species, sd)
tapply(data_sub$Petal.Length, data_sub$Species, var)

## Hipotesis:

# H0 = no hay diferencia significativa con la media de la longitud de 
# pétalo de la especie versicolor y la especie virginica 
# H1 = hay diferencias significativas

# ¿Existe diferencia en la longitud del pétalo entre versicolor y virginica?

df_versicolor <- data.frame(Petal.Length = 
                              data_sub$Petal.Length[data_sub$Species
                                                    == "versicolor"])
View(df_versicolor)

df_virginica <- data.frame(Petal.Length = 
                             data_sub$Petal.Length[data_sub$Species
                                                   == "virginica"])
View(df_virginica)




