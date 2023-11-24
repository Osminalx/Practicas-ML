#librerias
library(ggplot2)
library(rpart)
library(randomForest)
library(dplyr)
library(tibble)


#Data set
data <- read.csv("heart.csv")
colnames(data)
head(data)
str(data)

#Limpieza de datos
data$ca <-as.factor(data$ca)
datathal <- as.factor(data$thal)

#cambiar los ?
data[data == '?']<-NA

#convertir valores de sexo
data[data$sex == 0,]$sex <- "F"
data[data$sex == 1,]$sex <- "M"

#convertir a categorias más datos
data$fbs <- as.factor(data$fbs)
data$restecg <-as.factor(data$restecg)
data$exang <- as.factor(data$exang)
data$lope <- as.factor(data$slope)

data$ca <- as.integer(data$ca)
data$ca <- as.factor(data$ca)

data$thal <- as.integer(data$thal)
data$thal <- as.factor(data$thal)

data$hd <-ifelse(test = data$hd == 0, yes = "saludable", no = "no saludable")
data$hd <- as.factor(data$hd)

set.seed(42)
#data.imputed <- rfImpute(hd ~.,data = data,iter=6)

#modelo
model <- randomForest(hd ~.,data = data, proximity =TRUE)
model


#dataframe de prueba
#Esta opción no me dejaba hacerla
#oob.error.data <- data.frame(
 # Trees=rep(1:nrow(model$err.rate), times=3),
  #Type=rep(c("OOB", "Saludable", "No saludable"), each=nrow(model$err.rate)),
  #Error=c(model$err.rate[,"OOB"],
   #       model$err.rate[,"Saludable"],
    #      model$err.rate[,"No saludable"]
    #      )
#)

oob.error.data <- data.frame(
  Trees = rep(1:nrow(model$err.rate), times = 3),
  Type = rep(c("OOB", "Saludable", "No saludable"), each = nrow(model$err.rate)),
  Error = c(model$err.rate[, "OOB"],
            model$err.rate[, 2],  # Índice 2 para "Saludable"
            model$err.rate[, 3]   # Índice 3 para "No saludable"
  )
)

#Plot
ggplot(data = oob.error.data, aes(x = Trees,y= Error))+
  geom_line(aes(color=Type))

#segundo modelo
importancia_pred <- model$importance %>%
  enframe(name = "predictor", value ="importancia")

#Mostrar las 5 variables másimportantes
# Mostrar las 5 variables más importantes
importancia_pred %>%
  dplyr::arrange(desc(importancia)) %>%
  dplyr::top_n(5) %>%
  ggplot(aes(reorder(predictor, importancia), importancia)) +
  geom_col() +
  coord_flip() +
  ggtitle("Top 5 variables más importantes")
