#librerias
library(caret)
library(neuralnet)

#data
data(iris)

# Verificar las primeras filas del conjunto de datos
head(iris)

# Dividir el conjunto de datos en entrenamiento y prueba
set.seed(123)
indices_entrenamiento <- sample(1:nrow(iris), 0.7 * nrow(iris))
datos_entrenamiento <- iris[indices_entrenamiento, ]
datos_prueba <- iris[-indices_entrenamiento, ]
# Crear la fórmula para la red neuronal
formula <- as.formula("Species ~ Sepal.Length + Sepal.Width + Petal.Length +
Petal.Width")
# Crear y entrenar la red neuronal
modelo <- neuralnet(
  formula,
  data = datos_entrenamiento,
  hidden = c(5, 3), # Número de neuronas en cada capa oculta
  linear.output = FALSE # Especifica que la salida no es lineal (para clasificación)
)
# Visualizar la arquitectura de la red neuronal
plot(modelo)

# Hacer predicciones en el conjunto de prueba
predicciones <- predict(modelo, datos_prueba)
# Obtener el índice de la clase con la probabilidad más alta para cada fila
clases_predichas_indices <- max.col(predicciones)
# Convertir los índices a nombres de clases
clases_predichas <- factor(levels(datos_prueba$Species)[clases_predichas_indices],
                           levels = levels(datos_prueba$Species))
# Comparar las predicciones con las etiquetas reales
resultados <- data.frame(
  Real = datos_prueba$Species,
  Predicho = clases_predichas
)
# Imprimir los resultados
print(resultados)

# Crear la matriz de confusión
matriz_confusion <- confusionMatrix(resultados$Predicho, resultados$Real)
# Imprimir la matriz de confusión
print(matriz_confusion)

# Crear y entrenar la red neuronal con diferentes funciones de activación tanh
#Esta red neuronal tiene tres capas: la capa de entrada,
#una capa oculta con función de activación logística ("logistic"),
#otra capa oculta con función de activación tangente hiperbólica ("tanh"),
#y otra capa oculta con función de activación lineal ("linear")
formula2 <- as.formula("Species ~ Sepal.Length + Sepal.Width + Petal.Length +
Petal.Width")

modelo2 <- neuralnet(
  formula2,
  data = datos_entrenamiento,
  hidden = c(5, 3, 2), # Número de neuronas en cada capa oculta
  act.fct = c("logistic", "tanh", "linear"),
  linear.output = FALSE # Especifica que la salida no es lineal (para clasificación)
)
# Visualizar la arquitectura de la red neuronal
plot(modelo2)

# Hacer predicciones en el conjunto de prueba
predicciones2 <- predict(modelo2, datos_prueba)
# Obtener el índice de la clase con la probabilidad más alta para cada fila
clases_predichas_indices2 <- max.col(predicciones2)
# Convertir los índices a nombres de clases
clases_predichas2 <- factor(levels(datos_prueba$Species)[clases_predichas_indices2],
                            levels = levels(datos_prueba$Species))
# Comparar las predicciones con las etiquetas reales
resultados2 <- data.frame(
  Real = datos_prueba$Species,
  Predicho = clases_predichas2)
# Imprimir los resultados
print(resultados2)

# Crear la matriz de confusión
matriz_confusion2 <- confusionMatrix(resultados2$Predicho, resultados2$Real)
# Imprimir la matriz de confusión
print(matriz_confusion2)

# Crear y entrenar la red neuronal con la función de activación tanh
#Una capa oculta con 5 nodos y otra capa oculta de 3 nodos
modelo3 <- neuralnet(
  formula2,
  data = datos_entrenamiento,
  hidden = c(5, 3), # Número de neuronas en cada capa oculta
  act.fct = "tanh", # Especificar las funciones de activación
  linear.output = FALSE # Especifica que la salida no es lineal (para clasificación)
)
# Visualizar la arquitectura de la red neuronal
plot(modelo3)

# Hacer predicciones en el conjunto de prueba
predicciones3 <- predict(modelo3, datos_prueba)
# Obtener el índice de la clase con la probabilidad más alta para cada fila
clases_predichas_indices3 <- max.col(predicciones3)
# Convertir los índices a nombres de clases
clases_predichas3<- factor(levels(datos_prueba$Species)[clases_predichas_indices3],
levels = levels(datos_prueba$Species))
# Comparar las predicciones con las etiquetas reales
resultados3 <- data.frame(
Real = datos_prueba$Species,
Predicho = clases_predichas3
)
# Imprimir los resultados
print(resultados3)
# Crear la matriz de confusión
matriz_confusion3 <- confusionMatrix(resultados3$Predicho, resultados3$Real)
# Imprimir la matriz de confusión
print(matriz_confusion3)