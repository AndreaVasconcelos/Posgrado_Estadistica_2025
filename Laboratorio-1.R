# Andrea Michelle Luna Vasconcelos 1950889
# Semana 2 (14/08/2025)

## Laboratorio 1: Empezar con R y RStudio
# Primer contacto con RStudio

# Primer contacto con la consola de R -------------------------------------

# Gastos totales

300+240+1527+400+1500+1833
celular <- 300
celular
transporte <- 240
comestibles <- 1527
gimnasio <- 400
alquiler <- 1500
otros <- 1833

5800*5
5800*10

abs(10)
abs(-4)
sqrt(9)
log(2)

# Este es un comentario
2+9 # aquí tambien se puede poner un comentario

# R disitngue mayúsculas y minúsculas

celular
Celular <- -300
CELULAR <- 800

celular+Celular+CELULAR
Celular-CELULAR

# Aprender a usar help

help(abs)
help(mean)
?mean
help.search("absolute")

gastos <- c(celular, transporte, comestibles, gimnasio, alquiler, otros)

gastos

barplot(gastos)

sort(gastos)
gastosOrdenados <- sort(gastos)
barplot(gastosOrdenados)

help(sort)
sort(gastos,decreasing = TRUE)
gastos_ordenados <- sort(gastos,decreasing = TRUE)
barplot(gastos_ordenados)        

# Obtener ayuda

help("barplot")
barplot(gastos_ordenados,names.arg = gastos_ordenados)

names(gastos_ordenados) <- c("Otros",
                             "Comestibles",
                             "Alquiler",
                             "Gimnasio",
                             "Celular",
                             "Transporte")
barplot(gastos_ordenados, 
        main = "Gastos mensules",
        names.arg = names(gastos_ordenados),
        col = "purple")

# Parte II Variables ------------------------------------------------------

cat("#Problema 1: Identifique el tipo de variable (cualitativa o cuantitativa)\n
    para la lista de preguntas de una encuesta aplicada a estudiantes\n
    universitarios en una clase de estadística.\n")

variables <- c(list(
  nombre_de_estudiante = "Cualitativa",
  fecha_de_nacimiento = "Cuantitativa",
  edad = "Cuantitativa",
  direccion_de_casa = "Cualitativa",
  numero_de_teléfono = "Cualitativa",
  area_principal_de_estudio = "Cualitativa",
  grado_universitario = "Cualitativa",
  puntaje_en_la_prueba = "Cuantitativa",
  calificacion_general = "Cualitativa",
  tiempo_para_completar_la_prueba_de_MCF202 = "Cuantitativa",
  numero_de_hermanos = "Cuantitativa")
)

cat("Problema 2: Considere una variable con una lista de 14 variables:\n
    7 cuantitativas y 7 cualitativas.\n")

Deportista <- data.frame(Cualitativas = c("Nombre",
                   "Deporte",
                   "País de origen",
                   "Equipo/Academia",
                   "Nivel",
                   "Idioma",
                   "Dieta"),
                Cuantitativas = c("Edad",
                    "Peso(kg)",
                    "Altura(m)",
                    "Número de competencias",
                    "Horas de entrenamiento",
                    "Horas de sueño",
                    "Cantidad de comidas al día"))

View(Deportista)
knitr::kable(Deportista, caption = "Variables cuantitativas y cualitativas")


cat("Problema 3: Considere una variable con valores númericos que describen\n
    opiniones personales.\n")


Formas_electronicas_de_opinion <- data.frame(
  Código = c(
    1,
    2,
    3,
    4,5
  ),
  Redes = c(
    "Twitter",
    "Correo electrónico",
    "Mensaje de texto",
    "Facebook",
    "Blog"
    )
)
View(Formas_electronicas_de_opinion)

knitr::kable(Formas_electronicas_de_opinion, caption = "Formas electrónicas de\n
             expresar opiniones")


cat("Respuesta: Variable *cualitativa* ya que, aunque tu le das un valor\n
    númerico en realidad lo que representa son opiniones.\n") 

## Problema 4:Preguntas de investigación.


preguntas <- data.frame(
  Pregunta = c(
    "¿Cuál es la cantidad promedio de horas que los estudiantes de universidades\n
    públicas trabajan cada semana?",
    "¿Qué proporción de todos los estudiantes universitarios de México están\n
    inscritps en una universidad pública?",
    "En las universidades públicas,¿las estudiantes femeninas tienen un promedio\n
    CENEVAL más alto que los estudiantes varones?",
    "¿Es más probable que los atletas universitarios reciban asesoramiento\n
    académico que los atletas no universitarios?"
  ),
  Individuos = c(
    "Estudiantes universitarios",
    "Estudiantes universitarios",
    "Estudiantes femeninas y estudiantes varones",
    "Atletas universitarios y no universitarios"
    ),
  Variable = c(
    "Horas de trabajo",
    "Inscripción en universidad pública",
    "Promedio de CENEVAL",
    "Asesoramiento académico"
  ),
  Tipo_variable = c(
    "Cuantitativo",
    "Categórico",
    "Cuantitativo",
    "Categórico"
  ), stringsAsFactors = F)

View(preguntas)

knitr::kable(preguntas, booktabs = TRUE, caption = "Tabla de preguntas ajustada")



cat("Si reuniéramos datos para responder a las preguntas de la investigación\n
    anterior, ¿qué datos podrían analizarse mediante un historgrama?, ¿cómo\n
    lo sabes?.\n")

cat("Respuesta: Pregunta 1 y 3, ya que son variables cuantitativas medibles y\n
    estas las puedes agrupar por intervalos (Promedios de tiempo y de\n
    calificaciones) que por el contrario las categóricas no funcionan para\n
    historgrama porque como son categorias, no tienes un intervalo o rango para\n
    agrupar, se presentan en barras individuales.\n")
