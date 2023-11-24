#librerias
library(naivebayes)
#Set de datos
tabla1<-data.frame(hora=c(8,16,24,8,16,24,8,16,24,8,16,24,8,16,24,24,24,24))
tabla1$lugar<-c("casa","universidad","casa",
                "entrenando","universidad","casa",
                "entrenando","restaurant","casa",
                "entrenando","universidad","casa",
                "entrenando","universidad","casa",
                "casa","casa","casa"
                )
tabla1$finde<-c(F,F,F,
                F,F,F,
                T,T,T,
                F,F,T,
                T,F,T,
                T,T,T
                )
str(tabla1)

#convertir la variable continua a hora,en factor discreto
tabla1$hora<-as.factor(tabla1$hora)
str(tabla1)

#Crear modelo de pronostico
m<-naive_bayes(lugar ~ hora+finde,data = tabla1)

#representación gráfica
plot(m)

#Predicciones
tabla1$p<-predict(m)
head(tabla1)

#Nueva tabla para comparar
h<-data.frame(hora="24", finde=F)
table(tabla1$lugar,tabla1$hora+tabla1$finde)

#predecir m y h
predict(m,h)
#predecir en probabilidad
predict(m,h, type="prob")
