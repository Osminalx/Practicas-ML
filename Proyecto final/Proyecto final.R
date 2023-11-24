#Librerías
library(dplyr)
library(caret)
library(randomForest)

#dataset
data <- read.csv("spotify_songs.csv")
head(data)
str(data)

#limpieza de datos
data_cleaned <- data %>%
  select(-track_id,-track_name,-track_album_name,-playlist_id,
         -track_album_id,-duration_ms,-playlist_name,-playlist_subgenre,-track_artist,
         -track_album_release_date,-instrumentalness,-mode)
str(data_cleaned)

# Variables a excluir
exclude_vars <- c("key", "tempo", "track_popularity")

# Variables a transformar
vars_to_transform <- setdiff(names(data_cleaned), exclude_vars)

# Definir intervalos y etiquetas
breaks <- c(0, 0.5, 1)
labels <- c(0, 1)

# Iterar sobre las variables y aplicar la transformación
for (var in vars_to_transform) {
  data_cleaned[[paste0(var, "_category")]] <- cut(data_cleaned[[var]], breaks = breaks, labels = labels, include.lowest = TRUE)
}

# Omitir datos faltantes
datos_entrenamiento <- na.omit(data_cleaned)

# Dividir el conjunto de datos en entrenamiento y prueba
set.seed(123)  
indice_entrenamiento <- createDataPartition(datos_entrenamiento$track_popularity, p = 0.8, list = FALSE)
datos_entrenamiento <- datos_entrenamiento[indice_entrenamiento, ]
datos_prueba <- datos_entrenamiento[-indice_entrenamiento, ]

