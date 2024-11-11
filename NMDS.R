
# NMDS
# Cargar tabla: FrecuenciaNMDS

library(vegan)
library(tidyverse)

str(FrecuenciaNMDS)
FrecuenciaNMDS$Tipo <- as.factor(FrecuenciaNMDS$Tipo)
str(FrecuenciaNMDS)

Respuesta <- subset(FrecuenciaNMDS, select = -c(Tipo, Actor) )
View(Respuesta)

# Matriz de distancias
# Rango de valores de los datos
range(Respuesta)
# Como regla "suave" llevarlos a que varían entre más o menos
# entre 0 y 10.
# En este caso no se debe hacer una transformación 

DistRespuesta <- vegdist(Respuesta, method = "euclidean")
max(DistRespuesta)

# NMDS (non-metric multidimensional scaling)
# Es una técnica que permite visualizar el nivel 
# de similitud de las observaciones de un conjunto 
# de datos
set.seed(73)
?metaMDS
NMDA <- metaMDS(DistRespuesta)
NMDA

stressplot(NMDA)

plot(NMDA, type="t")

levels(FrecuenciaNMDS$Tipo)
length(levels(FrecuenciaNMDS$Tipo))
# Hay 2 niveles, entonces vamos
# a utilizar 2 colores para
# representarlos

# solo los ejes:
op <- ordiplot(NMDA, type = "n")

# Los colores de los niveles
cols = c("blue", "green")

# Los puntos:
points(NMDA, cex =1, pch= 16, col = cols[FrecuenciaNMDS$Tipo])

# Unimos los puntos al centroide:
ordispider(NMDA, groups = FrecuenciaNMDS$Tipo, 
           label = T,col = cols )

# Construimos un polígono alrededor de los grupos (niveles):
ordihull(NMDA, groups = FrecuenciaNMDS$Tipo, lty = "dotted", col = cols)


