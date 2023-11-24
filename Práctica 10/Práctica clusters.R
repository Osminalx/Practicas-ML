#cargan las librerias de clusters
# para visualizar resultados de análisis de clusters
library(factoextra)
#para realizar análisis de clustering
library(cluster)
# manipulación de datos
library(tidyverse)

#Carga el set de datos
data<-USArrests
#eliminar registros con valores ausentes
data<-na.omit(data)
#Identificar los datos
head(data)

#escalar cada variable para que tenga una media de 0 y una desviación estándar de 1
#Esto es importante para que las variables tengan el mismo peso en el análisis de
#clustering.
data<- scale.default(data)
#get_dist: fpara calcular una matriz de distancia entre las filas de una matriz de datos.
distance <- get_dist(data)
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

#La medida de distancia predeterminada es la distancia euclidiana
d <- dist(data, method = "euclidean")
# Agrupación jerárquica mediante vinculación completa
hc1 <- hclust(d, method = "complete" )
# Visualizar el dendrograma obtenido.
plot(hc1, cex = 0.6, hang = -1)

# Calcular con Agnes
hc2 <- agnes(data, method = "complete")
# Coeficiente de aglomeración
hc2$ac
#análisis de agrupamiento jerárquico: average, single, complete y ward.
m <- c("average", "single", "complete", "ward")
names(m)<-c("average","single", "complete", "ward" )

#función para calcular el coeficiente aglomerativo (agnes() leyendo el valor de $ac)
#calcula el coeficiente aglomerativo para cada método de vinculación.
ac <- function(x){ agnes(data,method = x)$ac}
#calcular el coeficiente de aglomeración para cada método de vinculación de
#agrupaciones
sapply(m, ac)

#realizar agrupamiento jerárquico utilizando la varianza mínima de Ward (varianza
#mínima)
clust <- agnes(data, method = "ward")
#Salida de un dendrograma
pltree(clust, cex = 0.6, hang = -1, main = "Dendrograma de Agnes")
#Agregando eqtiquetas a los clusters
rect.hclust(clust, k = 4, border = "red")
text(rect.hclust(clust, k = 4), labels = c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4"), srt
     = 0, adj = c(0, 0.5))

# calcular agrupamiento jerárquico divisivo
hc4 <- diana(data)
# Coeficiente de división; cantidad de estructura de agrupamiento encontrada
hc4$dc
# plot dendrogram
pltree(hc4, cex = 0.6, hang = -1, main = "Dendrograma de Diana")

#Para identificar subgrupos (es decir, grupos), podemos cortar el dendrograma con
cutree
#realizar agrupamiento jerárquico utilizando el método de Ward
final_clust <- hclust(d, method = "ward.D2" )
#cortar el dendrograma en 4 grupos
#utilizando cutree y se almacena en "groups".
groups <- cutree(final_clust, k=4)
# Número de miembros en cada grupo
table(groups)
#importa librería
library(dplyr)
#agregar etiquetas de clúster a los datos originales
USArrests %>%
  mutate(cluster = groups) %>%
  head
#Dendograma
plot(final_clust, cex = 0.6)
rect.hclust(final_clust, k = 4, border = 2:5)

# Elbow method
fviz_nbclust(data, FUN = hcut, method = "wss")
# Average Silhouette Method
fviz_nbclust(data, FUN = hcut, method = "silhouette")

#Gap Statistic Method
gap_stat <- clusGap(data, FUN = hcut, nstart = 25, K.max = 10, B = 50)
fviz_gap_stat(gap_stat)

#Segunda parte
# libreria de algoritmos de agrupamiento y visualización
library(factoextra)
# libreria de algoritmos de agrupamiento
library(cluster)
# libreria de manipulación de datos
library(tidyverse)
#Cargan los datos
data <- USArrests
#Se eliminan datos ausentes
data <- na.omit(data)
# Se escalan los datos numéricos
data <- scale(data)
#el número óptimo de grupos versus el total dentro de la suma de cuadrados
fviz_nbclust(data, kmeans, method = "wss")

# calcular la brecha estadística en función del número de grupos
gap_stat <- clusGap(data,
                    FUN = kmeans,
                    nstart = 25,
                    K.max = 10,
                    B = 50)
#grafica el número de conglomerados frente a la estadística de brecha
fviz_gap_stat(gap_stat)

#Se hace reproducible
set.seed(123)
#Realizar agrupaciones de k-means con k = 4 agrupaciones
km <- kmeans(data, centers = 4, nstart = 25)
#Visualiza los resultados
km

#grafica los resultados del modelo final de k-means
fviz_cluster(km, data = data)

#encuentra la media de cada cluster
aggregate(USArrests, by=list(cluster=km$cluster), mean)

#agrega el cluster al archivo original
final_data <- cbind(USArrests, cluster = km$cluster)
#Datos finales
head(final_data)

k2 <- kmeans(data, centers = 2, nstart = 25)
k3 <- kmeans(data, centers = 3, nstart = 25)
k4 <- kmeans(data, centers = 4, nstart = 25)
k5 <- kmeans(data, centers = 5, nstart = 25)
# plots para comparar
p1 <- fviz_cluster(k2, geom = "point", data = data) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point", data = data) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point", data = data) + ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom = "point", data = data) + ggtitle("k = 5")
library(gridExtra)
grid.arrange(p1, p2, p3, p4, nrow = 2)