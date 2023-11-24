library(caTools)
library(rpart)
library(rpart.plot)
library(dplyr)
library(caret)
library(rattle)

titanic <- read.csv("C:/Users/oscar/OneDrive/Escritorio/r_scripts/datos_titanic.csv")
set.seed(678)

shuffle_idx <- sample(1:nrow(titanic))
head(shuffle_idx)
titanic <- titanic[shuffle_idx, ]
head(titanic)

# clean data
clean_titanic <- titanic %>%
  select(-c(home.dest, cabin, name, x, ticket)) %>%
  # convert to factor level
  mutate(pclass = factor(pclass, levels = c(1,2,3), labels = c('Upper', 'Middle', 'Lower')),
         survived = factor(survived, levels = c(0, 1), labels = c('No', 'Si'))) %>%
  na.omit()
glimpse(clean_titanic)
#dividir el set de datos
data_traub <- clean_titanic %>% dplyc:: sample_frac(.8)
data_test <- dplyr::anti_join(clean_titanic, data_train,  by ='x')
#se debe tener un id
data_train<- dplyr::select(data_train, -x)
data_test <- dplyr::select(data_test, -x)
head(data_train)

#como se dividieron las instancias en varios sets
prop.table(table(data_train$survived))
prop.table(table(data_test$survived))

# find pattern within the data
fit <- rpart(survived ~ ., data = clean_titanic, method = 'class')

#Predicts
predict_modelo <- predict(fit,data_test, Type= 'class')

#Matriz de confusion
table_conf <- table(data_test$survived,predict_modelo)
table_conf
#Accuracy
accuracy_Test <- sum(diag(table_conf))/ sum(table_conf)
print("accuracy para el test de prueba"+ accuracy_Test)



print(fit)
fancyRpartPlot(fit)
fancyRpartPlot(fit, palettes = c("Greys", "Oranges"))
fancyRpartPlot(fit, main = "Pasajeros del Titanic")


table(arbol$RV-arbol$RV+arbol$RM+arbol$EDAD+arbol$PROMEDIO)

predict(modelo,h)

predict(modelo,)