
#Cargar librerias
library(randomForest)
library(ranger)
library(tibble)
library(dplyr)
library(MASS)
library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(corrplot)
library(devtools)
library(ggraph)
library(igraph)
library(rpart)
library(ROCR)


# Bosques aleatorios
#Problema 1. Flores
arbol.D <- read.csv ('flores.csv')
str(arbol.D)


set.seed(123)
#Matriz de correlacion
arbol.D %>%
  keep(is.numeric) %>%
  cor() %>%
  corrplot()

# Datos de entrenamiento y de prueba
total <- nrow(arbol.D)
entreno <- round(total *.5)
tamano<-sample(1:total, size = entreno)
train.arbol <- arbol.D[tamano,]
test.arbol = arbol.D[-tamano,]
train.arbol$Especie <- as.character(train.arbol$Especie)
train.arbol$Especie <- as.factor(train.arbol$Especie)

#Construye el modelo de randomForest para datos de entrenamiento
rf <-randomForest(Especie~.,data=train.arbol, mtry=4, ntree=100) 

#Construye el modelo de randomForest para todos los datos
rf2 <-randomForest(Especie~.,data=arbol.D, ntree=500) 


print(rf)
plot(rf)

# Importancia de acuerdo al indice de Gini
rf$importance
importance(rf)
varImpPlot(rf, sort = TRUE, n.var = 5, main = "Importancia de los predictores")

# Error en los set de prueba
oob.error.data <- data.frame(
  Trees=rep(1:nrow(rf$err.rate), times=4),
  Type=rep(c("OOB", "Iris-setosa", "Iris-versicolor","Iris-virginica" ), each=nrow(rf$err.rate)),
  Error=c(rf$err.rate[,"OOB"], 
          rf$err.rate[,"Iris-setosa"], 
          rf$err.rate[,"Iris-versicolor"],
          rf$err.rate[,"Iris-virginica"]))

# Gr?fica de los errores por #aboles
ggplot(
  data = oob.error.data, 
  aes(x = Trees, 
      y = Error, 
      fill = Error)
)+
  labs(x = "arboles", title = "# arboles/ OOBs") +
  geom_col() +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "none")

#Evaluate variable importance
importancia_pred <- rf$importance %>%
  enframe(name = "predictor", value = "importancia")


#Mostrar las 5 variables mas importantes
importancia_pred %>%
  dplyr::arrange(desc(predictor)) %>%
  dplyr::top_n(5) %>%
  ggplot(aes(reorder(predictor, importancia), importancia)) +
  geom_col() +
  coord_flip() +
  ggtitle("Top 5 variables mas importantes")




#EJERCICIO PARA MAYOR DIMENSIONALIDAD DE DATOS
# Problema 2. Set de datos diabetes

# Bosques aleatorios
arbol.D <- read.csv ('diabetes.csv')
str(arbol.D)


set.seed(123)
#Matriz de correlacion
arbol.D %>%
  keep(is.numeric) %>%
  cor() %>%
  corrplot()

# Datos de entrenamiento y de prueba
total <- nrow(arbol.D)
entreno <- round(total *.5)
tamano<-sample(1:total, size = entreno)
train.arbol <- arbol.D[tamano,]
test.arbol = arbol.D[-tamano,]
train.arbol$Outcome <- as.character(train.arbol$Outcome)
train.arbol$Outcome <- as.factor(train.arbol$Outcome)

#Corro el randomForest para todos los datos
rf <-randomForest(Outcome~.,data=train.arbol, mtry=7, ntree=100) 
rf <-randomForest(Outcome~.,data=arbol.D, ntree=100) 
print(rf)
plot(rf)

# Importancia de acuerdo al indice de Gini
rf$importance
importance(rf)
varImpPlot(rf, sort = TRUE, n.var = 5, main = "Importancia de los predictores")

# Error en los set de prueba
oob.error.data <- data.frame(
  Trees=rep(1:nrow(rf$err.rate), times=4),
  Type=rep(c("OOB", "1", "0" ), each=nrow(rf$err.rate)),
  Error=c(rf$err.rate[,"OOB"], 
          rf$err.rate[,"1"], 
          rf$err.rate[,"0"]))

ggplot(
  data = oob.error.data, 
  aes(x = Trees, 
      y = Error, 
      fill = Error)
)+
  labs(x = "arboles", title = "# arboles/ OOBs") +
  geom_col() +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "none")

#Evaluate variable importance
importancia_pred <- rf$importance %>%
  enframe(name = "predictor", value = "importancia")


#Mostrar las 5 variables mas importantes
importancia_pred %>%
  dplyr::arrange(desc(predictor)) %>%
  dplyr::top_n(5) %>%
  ggplot(aes(reorder(predictor, importancia), importancia)) +
  geom_col() +
  coord_flip() +
  ggtitle("Top 5 variables mas importantes")


#mtry es el numero de variables que considera en cada paso del total 
#Se prueba con la raiz cuadrada del total de variables del set de datos
inicio<-floor(sqrt(ncol(arbol.D) - 1))

#Selecciona el valor de mtry con el m?nimo error of bag(OOB) error.

mtry <- tuneRF(arbol.D[-1],arbol.D$Outcome,mtryStart = inicio, ntreeTry=50,
               stepFactor=1,improve=0.005, trace=FALSE, plot=TRUE, doBest =FALSE )
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)

#Encontrando los mejores parametros con la libreria ranger
#Produce una salida ordenada (variando la primera columna como la m?s lenta, en lugar de la m?s r?pida).
param_grid = expand_grid(
  'num_trees' = c(50, 100, 300, 500, 1000),
  'mtry'      = c(3, 5, 7, ncol(train.arbol)-1),
  'max_depth' = c(1, 3, 10, 20))

oob_error<-matrix(nrow=nrow(param_grid), ncol=1)
menorError<-9999
#Ciclo para evaluar cada grupo de par?metros
for(i in 1:nrow(param_grid)){
  
  modelo <- ranger(
    formula   = Outcome ~ .,
    data      = train.arbol, 
    num.trees = param_grid$num_trees[i],
    mtry      = param_grid$mtry[i],
    max.depth = param_grid$max_depth[i],
    importance = "impurity",
    seed      = 123
  )
  # Si el error es menor lo sustituye como el mejor
  
  if (menorError>modelo$prediction.error )
  {
    menorError<- modelo$prediction.error
    mejormodelo<-modelo
    posicion<-i
    arboles<-modelo$num.trees
    maxvariables<-modelo$mtry
    nodos<-modelo$num.independent.variables
    importantes<-modelo$variable.importance
  }
  oob_error[i] <- modelo$prediction.error
  
}
#Historgrama con los OOB ERROR 
hist(oob_error, breaks = 20)
mejormodelo
#oob_error
menorError
posicion
arboles
maxvariables
nodos

#Se construye el modelo con los mejores parametros
model <-randomForest(Outcome~.,data=train.arbol, mtry = maxvariables,importance=TRUE, 
                     ntree=arboles, max_nodes =depth )

model
model$importance
importance(model)
varImpPlot(model, sort = TRUE, n.var = 5, main = "Importancia de los predictores")

#Teniendo las variables mas importantes se construye un arbol
#de clasificacion para identificar las reglas

fit <- rpart(Outcome~Glucose + BMI + Age + SkinThickness + Pregnancies, data=train.arbol, method = 'class')
fit2 <- rpart(Outcome~ Glucose + BMI +  DiabetesPedigreeFunction + Age + 
               BloodPressure , data=train.arbol, method = 'class')
# summarize the fit
print(fit)
print(fit2)

# Habilitar la libreria Rattle
# plot tree
library(rattle)
fancyRpartPlot(fit)
fancyRpartPlot(fit2)

## Choose different colours.
fancyRpartPlot(fit, palettes=c("Greys", "Oranges"))

## Add a main title to the plot.
fancyRpartPlot(fit, main="Personas que desarrollaran diabetes")


# Predicciones

predict_modelo<- predict(fit, test.arbol, type = 'class')
predict_modelo2<- predict(fit2, test.arbol, type = 'class')

#Matriz de confusion Modelo 1
table_conf <- table(test.arbol$Outcome, predict_modelo)
table_conf
#M?tricas de desempe?o
accuracy_Test <- sum(diag(table_conf)) / sum(table_conf)
print(paste('Accuracy para el set de prueba', accuracy_Test))

#Matriz de confusion Modelo 2
table_conf <- table(test.arbol$Outcome, predict_modelo2)
table_conf
#M?tricas de desempe?o
accuracy_Test <- sum(diag(table_conf)) / sum(table_conf)
print(paste('Accuracy para el set de prueba', accuracy_Test))




# Problema #3. Heart 
#Janosi,Andras, Steinbrunn,William, Pfisterer,Matthias, and Detrano,Robert.(1988). 
#Heart Disease. UCI Machine Learning Repository. https://doi.org/10.24432/C52P4X.
library(cowplot)
data <-read.csv("heart.csv")
colnames(data)
head(data)
str(data)

#LIMPIEZA DE DATOS
#cambiar a categoria ca y thal
data$ca<- as.factor(data$ca)
data$thal<-as.factor(data$thal)

#cambiar los simbolo ? por NA
data[data =="?"]<- NA

#convertir los valores de sexo 
data [data$sex == 0,]$sex<- "F" # 0 por F 
data [data$sex == 1,]$sex<- "M" # 1 por M

#Convertir a categorias algunos datos
data$sex <-as.factor(data$sex)  #genero
data$cp <- as.factor(data$cp)   # tipo de dolor de pecho
#-- Value 1: typical angina
#-- Value 2: atypical angina
#-- Value 3: non-anginal pain
#-- Value 4: asymptomatic

data$fbs <- as.factor(data$fbs) # (glucosa en ayunas > 120 mg/dl)  (1 = true; 0 = false)
data$restecg <- as.factor(data$restecg) #resultados electrocardiogr?ficos en reposo
data$exang <- as.factor(data$exang) #angina inducida por el ejercicio (1 = s?; 0 = no)
data$slope <- as.factor(data$slope) #la pendiente del segmento ST del ejercicio m?ximo
#-- Valor 1: pendiente ascendente
#-- Valor 2: plano
#-- Valor 3: pendiente descendente
data$ca <-as.integer(data$ca) # n?mero de vasos principales (0-3) coloreados por fluoroscopia
data$ca <-as.factor(data$ca)  # categoria

data$thal <- as.integer(data$thal) #3-normal , 6-defecto fijo; 7 = defecto reversible
data$thal <-as.factor(data$thal)  # categoria

data$hd <- ifelse(test=data$hd == 0, yes = "Saludable", no= "No saludable")
data$hd <- as.factor(data$hd) #categor?a

set.seed(42)
data.imputed <- rfImpute(hd ~.,data = data, iter=6) #iter randomforest construir? para estimar los datos ausentes

#construimos el modelo
model <- randomForest(hd ~., data=data, proximity=TRUE)
model
# el tipo de RF es de clasificacion
# Si se quisiera predecir un n?mero dir?a: regresi?n
# si se omitiera lo que se busca predecir dir?a: no supervisado

#Plot de los error rate
# 1. data frame con los OOB
oob.error.data <- data.frame(
  Trees=rep(1:nrow(model$err.rate), times=3),
  Type=rep(c("OOB", "Saludable", "No saludable" ), each=nrow(model$err.rate)),
  Error=c(model$err.rate[,"OOB"], 
          model$err.rate[,"Saludable"], 
          model$err.rate[,"No saludable"]))

#2. plot
ggplot(data = oob.error.data, aes(x = Trees,y = Error)) + 
  geom_line (aes(color=Type))

# Usamos menos ?rboles
#construimos el modelo
model <- randomForest(hd ~., data=data.imputed, ntree =70, proximity=TRUE)
model


#Plot de los error rate
# 1. data frame con los OOB
oob.error.data <- data.frame(
  Trees=rep(1:nrow(model$err.rate), times=3),
  Type=rep(c("OOB", "Saludable", "No saludable" ), each=nrow(model$err.rate)),
  Error=c(model$err.rate[,"OOB"], 
          model$err.rate[,"Saludable"], 
          model$err.rate[,"No saludable"]))

#2. plot
ggplot(data = oob.error.data, aes(x = Trees,y = Error)) + 
  geom_line (aes(color=Type))


#Variables principales
#Evaluate variable importance
importancia_pred <-model$importance %>%
  enframe(name = "predictor", value = "importancia")


#Mostrar las 5 variables mas importantes
importancia_pred %>%
  dplyr::arrange(desc(predictor)) %>%
  dplyr::top_n(5) %>%
  ggplot(aes(reorder(predictor, importancia), importancia)) +
  geom_col() +
  coord_flip() +
  ggtitle("Top 5 variables mas importantes")