#librerias
library(caTools)
library(rpart)
library(rpart.plot)
#cargar cv
diabetes <- read.csv("diabetes.csv")
#Seed
set.seed(678)

shuffle_idx <- sample(1:nrow(diabetes))
head(shuffle_idx)
diabetes <- diabetes[shuffle_idx, ]
head(diabetes)



# Proporción para dividir los datos (por ejemplo, 80% entrenamiento, 20% prueba)
split_ratio <- 0.8
split_index <- round(nrow(diabetes) * split_ratio)

# Conjunto de entrenamiento
diabetes_train <- diabetes[1:split_index, ]

# Conjunto de prueba
diabetes_test <- diabetes[(split_index + 1):nrow(diabetes), ]

#Modelo
noSugar <- rpart(Outcome ~ ., data= diabetes, method = "class",maxdepth = 5)
print(noSugar)

# Ajustar el tamaño del gráfico
options(repr.plot.width = 10, repr.plot.height = 6)
# Visualizar el árbol
rpart.plot(noSugar)


#predicts del modelo
sugarLess <- predict(noSugar,diabetes_test, Type= "class")
# Establecer un umbral
umbral <- 0.5

# Convertir probabilidades en clases
sugarLess_class <- ifelse(sugarLess[, 2] > umbral, 1, 0)

# Agregar las predicciones a diabetes_test
diabetes_test$Predictions <- sugarLess_class
# Calcular la precisión
precision <- sum(diabetes_test$Outcome == diabetes_test$Predictions) / nrow(diabetes_test)
print(precision)

#**EXAMEN**
#Utilizando regresión logística

#Modelo de regresión lineal
modelo.reg.log = glm(Outcome ~., data = diabetes, family = binomial)
summary(modelo.reg.log)

# Predecir en el conjunto de prueba utilizando el modelo de regresión logística
predict.reg.log <- predict(modelo.reg.log, diabetes_test, type = "response")

# Convertir las probabilidades en clases
reg_log_class <- ifelse(predict.reg.log > umbral, 1, 0)

# Agregar las predicciones a diabetes_test
diabetes_test$RegLogPredictions <- reg_log_class

# Calcular la precisión para el modelo de regresión logística
precision_reg_log <- sum(diabetes_test$Outcome == diabetes_test$RegLogPredictions) / nrow(diabetes_test)
print(precision_reg_log)

