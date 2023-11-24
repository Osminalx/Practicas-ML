#librerias
library(gridExtra)
library(ggplot2)

#Set de Datos
data("USArrests")
head(USArrests,10)

#calcular la varianza de cada variable
apply(USArrests,2,var)
#Crear un nuevo marco de datos con variables centradas
scaled_df <- apply(USArrests,2,scale)
head(scaled_df)

#Calcular valores propios y vectores propios
arrests.cov <-cov(scaled_df)
arrests.eigen <- eigen(arrests.cov)
str(arrests.eigen)
#Tomar los primeros conjuntos 
(phi <- arrests.eigen$vectors[,1:2])
phi<-phi
#El conjunto de cargas para el primer componente
row.names(phi)<- c("murder","Assault","UbanPop","Rape")
colnames(phi)<- c("PC1","PC2")
phi
#Calcular puntuaciones
PC1 <- as.matrix(scaled_df) %%phi[,1]
PC2 <- as.matrix(scaled_df) %%phi[,2]
#Crear marco de datos con puntuaciones de componentes
PC <- data.frame(State= row.names(USArrests),PC1,PC2)
head(PC)

#Plot
ggplot(PC,aes(PC1,PC2))+
  modelr::geom_ref_line(h=0)+
  modelr::geom_ref_line(v=0)+
  geom_text(aes(label = State), size=3)+
  xlab("Primer componente principal")+
  ylab("Segundo componente Principal")+
  ggtitle("Primeros dos componentes primcipales de USArrests Date")
#Se calcula
PVE<- arrests.eigen$values / sum(arrests.eigen$values)
round(PVE,2)
#Tramo PVE(También conocido como Scree)
PVEplot <- gplot(c(1:4),PVE)+
  geom_line()+
  xlab("Componente principal")+
  ylab("PVE")+
  ggtitle("scree plot")+
  ylim(0,1)

#Gráfico PVE acumulativo 
  cumPVE <- qplot(c(1:4), cumsum(PVE))+
    geom_line()+
    xlab("Principal component")+
    ylab(NULL)+
    ggtitle("Cumulative scree plot")+
    yliim(0,1)
  
  grid.arrange(PVEplot, cumPVE,ncol=2)
  
  #Funciones PCA integradas
  #prcomp:centra las variables para que tengan media 0
  #Se coloca la escala en TRUE para que las desviación estandar de las variables sea 1
  pca_result<-prpcomop(USArrests, scale= TRUE)
  names(pca_result)
  #Los componentes del centro
  pca_result$center
  #Desviación estandar
  pca_result$scale
  
  #La Matriz de rotación proporciona las cargas de los componentes principales
  pca_result$rotation 
  #Los vectores propios en R apuntan en la dirección negativa para ajustar esto con un simple cambio
  pca_result$rotation <- pca_result$rotation
  pca_result$rotation
  #Se obtienen los componentes principales de nuestros resultados
  pca_result$x<- -pca_result$x
  head(pca_result$x)
  #Se trazan los dos primeros componentes principales usando biplot
  biplot(pca_result,scale=0)
  #Si desea trazar los componentes principales 3 frente a 4 se puede hacer así:
  biplot(pca_result,scale=0,choices=3:4)
  #Genera la desviación estandar
  pca_result$dev
  #La varianza explicada por cada componente principal
  (VE<-pca_result$sdev^2)
  #se dividen las dos varianzas
  PVE <- VE /sum(VE)
  round(PVE,2)
