data(cars)
head(cars)

# Ajustar un modelo de regresión lineal
modelo1 <- lm(dist ~ speed, data = cars)

# Resumen del modelo 1
summary(modelo1)

# Ajustar un modelo de regresión polinómica de segundo grado
modelo2 <- lm(dist ~ poly(speed, 2), data = cars)

# Resumen del modelo 2
summary(modelo2)



# Instalar y cargar el paquete Ecdat si no está instalado
if (!requireNamespace("Ecdat", quietly = TRUE)) {
  install.packages("Ecdat")
}

# Cargar el paquete Ecdat
library(Ecdat)

# Cargar los datos del conjunto "Icecream"
data("Icecream")

# Ajustar un modelo de regresión lineal simple
modelo1 <- lm(cons ~ temp, data = Icecream)

# Resumen del modelo 1
summary(modelo1)

# Ajustar un modelo de regresión polinómica de segundo grado
modelo2 <- lm(cons ~ poly(temp, 2), data = Icecream)

# Resumen del modelo 2
summary(modelo2)
# Estimación de la varianza del error para el modelo 1
varianza_error_modelo1 <- sigma(modelo1)

# Estimación de la varianza del error para el modelo 2
varianza_error_modelo2 <- sigma(modelo2)

#Pregunta 2
library(Ecdat)

# Cargar los datos del conjunto "Icecream"
data("Icecream")
# Ajustar un modelo de regresión lineal simple
modelo1 <- lm(cons ~ temp, data = Icecream)

# Resumen del modelo 1
summary(modelo1)

# Ajustar un modelo de regresión polinómica de segundo grado
modelo2 <- lm(cons ~ poly(temp, 2), data = Icecream)

# Resumen del modelo 2
summary(modelo2)
# Estimación de la varianza del error para el modelo 1
varianza_error_modelo1 <- sigma(modelo1)

# Estimación de la varianza del error para el modelo 2
varianza_error_modelo2 <- sigma(modelo2)


#Pregunta 3
# Cargar la biblioteca MASS
library(MASS)

# Cargar los datos del conjunto "cabbages"
data(cabbages)

# Ajustar un modelo de regresión lineal
modelo_regresion <- lm(VitC ~ HeadWt, data = cabbages)

# Resumen del modelo
summary(modelo_regresion)

# Gráfico de dispersión de residuos vs. valores ajustados
plot(modelo_regresion, which = 1)

#Pregunta 4
# Cargar la biblioteca Ecdat
library(Ecdat)

# Cargar los datos del conjunto "Cigar"
data("Cigar")
# Ajustar un modelo de regresión lineal
modelo_regresion <- lm(ventas ~ precio, data = Cigar)

# Resumen del modelo
summary(modelo_regresion)

#pregunta 5
library(wooldridge)

# Cargar los datos del conjunto "happiness"
data("happiness")

# Estimar el modelo de regresión lineal
modelo_regresion_lineal <- lm(vhappy ~ educ + income + female + unem10, data = happiness)

# Resumen del modelo de regresión lineal
summary(modelo_regresion_lineal)
# Estimar el modelo de regresión logística
modelo_regresion_logistica <- glm(vhappy ~ educ + income + female + unem10, data = happiness, family = binomial)

# Resumen del modelo de regresión logística
summary(modelo_regresion_logistica)
# Crear una nueva observación con los valores deseados
nueva_observacion <- data.frame(educ = 18, income = "$25000 or more", female = 1, unem10 = 0)

# Predecir la probabilidad de ser muy feliz utilizando el modelo de regresión lineal
probabilidad_lineal <- predict(modelo_regresion_lineal, newdata = nueva_observacion, type = "response")

# Predecir la probabilidad de ser muy feliz utilizando el modelo de regresión logística
probabilidad_logistica <- predict(modelo_regresion_logistica, newdata = nueva_observacion, type = "response")

probabilidad_lineal
probabilidad_logistica


