# Andrea Michelle Luna Vasconcelos
# 21/08/2025
# Semana 3


# Importar datos web ------------------------------------------------------

read.csv("https://repodatos.atdt.gob.mx/api_update/senasica/actividades_inspeccion_movilizacion/29_actividades-inspeccion-movilizacion.csv")

url <- "https://repodatos.atdt.gob.mx/api_update/senasica/actividades_inspeccion_movilizacion/29_actividades-inspeccion-movilizacion.csv"

senasica <- read.csv(url, header = T)

View(senasica)

url2 <- paste0("https://repodatos.atdt.gob.mx/api_update/",
               "senasica/actividades_inspeccion_movilizacion/",
               "29_actividades-inspeccion-movilizacion.csv")

senasica <- read.csv(url, header = T)
senasica2 <- read.csv(url2, header = T)
head(senasica2[, c(1,3:12)])

# Terminado
