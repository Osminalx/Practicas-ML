#libraries
library(caTools)
library(rpart)
library(rpart.plot)

#Lectura de datos
lin.reg <- read.csv("wine.csv")
#set de datos
str(lin.reg)
#Gráfico del set de datos
plot(lin.reg)
#leer datos de prueba
lin.reg.test = read.csv("wine_test.csv")
#set de datos de prueba
str(lin.reg.test)


#Modelos
modelo.reg.lin.0 = lm(Price ~ AGST, data=lin.reg)
modelo.reg.lin.1 = lm(Price ~ AGST + HarvestRain + WinterRain + Age + FrancePop,data = lin.reg)
modelo.reg.lin.2 = lm(Price ~ AGST + HarvestRain, data = lin.reg)
modelo.reg.lin.3 = lm(Price ~ AGST + HarvestRain + Age,data = lin.reg)

summary(modelo.reg.lin.0)
summary(modelo.reg.lin.1)
summary(modelo.reg.lin.2)
summary(modelo.reg.lin.3)

#Grafica de correlación
plot(lin.reg$AGST,lin.reg$Price)

plot(lin.reg$HarvestRain,lin.reg$Price)

#Predicciones
predicciones0 = predict(modelo.reg.lin.0, newdata = lin.reg.test)
predicciones1 = predict(modelo.reg.lin.1, newdata = lin.reg.test)
predicciones2 = predict(modelo.reg.lin.2, newdata = lin.reg.test)
predicciones3 = predict(modelo.reg.lin.3)
predicciones0
predicciones1
predicciones2

#Correlaciones
cor(predicciones0,lin.reg.test$Price)
cor(predicciones1,lin.reg.test$Price)
cor(predicciones2,lin.reg.test$Price)

#grafica de la relacion entre valores
plot(predicciones0,lin.reg.test$Price)


#Sumas cuadradas de los errores
SSE0 = sum((lin.reg.test$Price - predicciones0) ^ 2)
SSE1 = sum((lin.reg.test$Price - predicciones1) ^ 2)
SSE2 = sum((lin.reg.test$Price - predicciones2) ^ 2)
SSE0
SSE1
SSE2

#Suma total de cuadrados
SST = sum((lin.reg.test$Price - mean(lin.reg$Price)) ^ 2)
SST

#R² de todos los modelos
r2.0 = 1-(SSE0/SST)
r2.1 = 1-(SSE1/SST)
r2.2 = 1-(SSE2/SST)
r2.0
r2.1
r2.2

#2da parte de la práctica: Regresión lógistica
log.reg <- read.csv("quality.csv")
str(log.reg)

#Regresión lógistica
set.seed(88)
split = sample.split(log.reg$PoorCare,SplitRatio = .75)
log.train = subset(log.reg,split==T)
log.test = subset(log.reg,split==F)

#modelo
modelo.reg.log.1 = glm(PoorCare ~ OfficeVisits + Narcotics,data = log.train,family = binomial)
summary(modelo.reg.log.1)

#Predicciones
predict.train.log = predict(modelo.reg.log.1,newdata = log.train,type = 'response')
tapply(predict.train.log,log.train$PoorCare,mean)

predict.log= predict(modelo.reg.log.1, newdata = log.test,type='response')
table(log.test$PoorCare,predict.log > .5)

