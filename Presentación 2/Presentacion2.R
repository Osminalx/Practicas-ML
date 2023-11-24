#librerias
library(caTools)
library(rpart)
library(rpart.plot)
library(dplyr)
library(randomForest)
library(tidyverse)

#Lectura de dataset y limpieza de datos
data <- read.csv("The Hollywood Inider - all data - 2018.csv")
str(data)
# Omitir columnas con solo NA y eliminar la columna "Link"
data_cleaned <- data %>%
  select(-Link,-Film,-Year) %>%
  select_if(~!all(is.na(.)))

# Reemplazar "-" con NA en todas las columnas
data_cleaned <- data_cleaned %>%
  mutate_all(~ifelse(. == "-", NA, .))

# Imputar valores faltantes con la media
data_imputed <- data_cleaned %>%
  mutate_all(~ifelse(is.na(.), mean(., na.rm = TRUE), .))

# Imputar con la media
data_imputed_mean <- na.omit(data_imputed) %>%
  mutate_all(~ifelse(is.na(.), mean(., na.rm = TRUE), .))

# Ver la estructura del nuevo conjunto de datos
str(data_imputed_mean)

# Crear un conjunto de entrenamiento y prueba
set.seed(123)  # Para reproducibilidad
split <- sample.split(data_imputed_mean$Genre, SplitRatio = 0.7)
train_data <- subset(data_imputed_mean, split == TRUE)
test_data <- subset(data_imputed_mean, split == FALSE)

# Entrenar el modelo Random Forest con ajustes en los parámetros
model_rf <- randomForest(factor(Genre) ~ ., data = train_data, ntree = 500, max_depth = 10)

# Imprimir la importancia de las variables
print(model_rf$importance)

# Seleccionar las 5 variables más importantes
top_variables <- head(row.names(model_rf$importance), 5)

# Crear un nuevo conjunto de datos con las 5 variables más importantes y la variable objetivo
data_top_variables <- data_imputed_mean[, c(top_variables, "Genre")]

# Imprimir el conjunto de datos con las 5 variables más importantes
print(data_top_variables)

# Mostrar las 5 variables más importantes en un gráfico
importancia_pred <- data.frame(predictor = row.names(model_rf$importance), importancia = model_rf$importance[, 1])

importancia_pred %>%
  dplyr::arrange(desc(importancia)) %>%
  dplyr::top_n(5) %>%
  ggplot(aes(reorder(predictor, importancia), importancia)) +
  geom_col() +
  coord_flip() +
  ggtitle("Top 5 variables más importantes")

# Crear un conjunto de datos con las 5 variables seleccionadas y la variable objetivo
data_top_variables <- data_imputed[, c(top_variables, "Genre")]

# Dividir el conjunto de datos en entrenamiento y prueba
set.seed(123)
split <- sample.split(data_top_variables$Genre, SplitRatio = 0.7)
train_data <- subset(data_top_variables, split == TRUE)
test_data <- subset(data_top_variables, split == FALSE)

# Entrenar un modelo de árbol de decisión
model_tree <- rpart(Genre ~ ., data = train_data, method = "class", cp = 0.01, minsplit = 10)

# Visualizar el árbol de decisión
rpart.plot(model_tree, main = "Árbol de Decisión con las 5 Mejores Variables", extra = 102, under = TRUE, cex = 0.8)

# Predecir en el conjunto de prueba
predictions_tree <- predict(model_tree, newdata = test_data[, top_variables], type = "class")

# Evaluar el rendimiento del modelo de árbol de decisión
conf_matrix_tree <- table(predictions_tree, test_data$Genre)
accuracy_tree <- sum(diag(conf_matrix_tree)) / sum(conf_matrix_tree)
print(conf_matrix_tree)
print(paste("Precisión del modelo de árbol de decisión: ", accuracy_tree))

