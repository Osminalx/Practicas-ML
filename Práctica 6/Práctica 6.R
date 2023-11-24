#Importar librerias
library(caTools)
library(rpart)
library(rpart.plot)

# Variables
#dataFrame original
arbol <- read.csv("stevens.csv")
#Data set de entrenamiento
set.seed(3000)
split = sample.split(arbol$Reverse, SplitRatio = .7)
train.arbol = subset(arbol,split==T)
test.arbol = subset(arbol,split==F)
#Modelos de arbol
Modelo.arbol1 = rpart(Reverse ~ Circuit + Issue + Petitioner + LowerCourt + Unconst, data = train.arbol, 
                     method = 'class', minbucket = 25)
#Interpretación del árbol
prp(Modelo.arbol1)
#predicciones
pred.arbol1 <- predict(Modelo.arbol1, newdata=test.arbol, type='class')
#Tabla para verificar la precisión
table(test.arbol$Reverse,pred.arbol1)
#Precisión del modelo según la tabla
precision = ((51+66) / (51+26+27+66))
precision

#EJERCICIO
#Data set
collageDT <- read.csv("College.csv")
collageDT$TOTAL <- NULL
#Data set de entrenamiento
set.seed(3000)
collageSplit = sample.split(collageDT$Reverse, SplitRatio = .7)
train.collage = subset(collageDT,split==T)
test.collage = subset(collageDT,split==F)
#Modelo arbol
