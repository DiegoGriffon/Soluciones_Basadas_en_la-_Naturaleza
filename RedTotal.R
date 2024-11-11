
# ///////////Análisis de Redes/////////////////
# ///////Resultados de Tams Analyzer///////////

# Una vez efectuada la codificación de los textos.

# Ir a la pestaña de Reports y buscar: 
"Graph/Chart co-coding frequency"
# que es la primera opción.

# Una vez obtenida la tabla, buscar la pestaña de File y luego buscar
# Save To. 
# Escoger el formato (file format) tabbed text.

# Abrir el archivo de texto y copiar todo, luego pegarlo en 
# un archivo excel. 
# Eliminar la primera columna y llamar al achivo Red
# Listo

# Para las frecuencias, hacer una búsqueda ilimitada
# hacer click en el icono "Compare data". En  el área de
# "Data Elements" seleccionar "Count only", en el área
# "include data:" seleccionar "_code"
# Hacer click en el botón "generate report"
# hacer click en "Open in browser", hacer control a, luego
# control c, llevar a un archivo excel y hacer control v


library(igraph)
# Cargar matriz excel (aquí llamada red)
# Cargar matriz excel (aquí llamada frecuencia)

# Grafo red total

Matriz <- Red
Matriz
class(Matriz)
Matriz <-as.matrix(Matriz)
class(Matriz)
Nombres <- colnames(Matriz)
Nombres
colnames(Matriz)<-NULL
Matriz

?graph.adjacency

g <- graph.adjacency(Matriz, mode="undirected", weighted= T)

V(g)$name  <- Nombres
V(g)$name 

plot(g, 
     vertex.color="skyblue")

# OJO: confirmar en Tams, con una búsqueda no limitada
# que todos los códigos se utilizan. Para esto utilizar
# los nombres de los nodos aislados (si existen) en el
# grafo.

# Si hay códigos no utilizados y se considera necesario,
# eliminarlos y repetir todo el proceso hasta aquí.


# Hacer que los vínculos tengan un tamaño
# proporcional a sus pesos
plot(g, edge.width=E(g)$weight, 
     vertex.color="skyblue")


# Hacer que los nodos tenga un tamaño
# proporcional a su frecuencia

Frecuencias <-as.matrix(Frecuencia)
colnames(Frecuencias)<-NULL
Frecuencias

V(g)$Frecuencia <- Frecuencias  

V(g)$Frecuencia

V(g)$size <- V(g)$Frecuencia*2

plot(g, 
     edge.width=E(g)$weight, 
     vertex.color="skyblue")


# Diferentes formas de los grafos
??layouts
l1 <- layout_on_sphere(g)
l2 <- layout_in_circle(g)
l3 <- layout_with_fr(g)
l4 <- layout_with_kk(g)
l5 <- layout_on_grid(g)

plot(g, 
     edge.width=E(g)$weight*0.2, 
     vertex.color="skyblue",
     layout=l1)

plot(g, 
     edge.width=E(g)$weight*0.8, 
     vertex.color="skyblue",
     layout=l2,
     vertex.label.cex=1.6)

plot(g, 
     edge.width=E(g)$weight*0.2, 
     vertex.color="skyblue",
     layout=l3)

plot(g, 
     edge.width=E(g)$weight*0.2, 
     vertex.color="skyblue",
     layout=l4)

plot(g, 
     edge.width=E(g)$weight*0.2, 
     vertex.color="skyblue",
     layout=l5)

# Tamaño del grafo
l3 <- norm_coords(l3, ymin=-1, ymax=1, xmin=-1, xmax=1)

plot(g, 
     edge.width=E(g)$weight*0.2, 
     vertex.color="skyblue",
     rescale=F,
     layout=l3*2)

l2 <- norm_coords(l2, ymin=-1, ymax=1, xmin=-1, xmax=1)

plot(g, 
     edge.width=E(g)$weight*0.2, 
     vertex.color="skyblue",
     rescale=F,
     layout=l2*1.5,
     vertex.label.color="black")


# Índices varios

edge_density(g, loops=F) 

transitivity(g, type="global")

reciprocity(g)

mean_distance(g, directed=T)

diameter(g, directed=F, weights=NA)

betweenness(g, directed=F, weights=NA)

degree(g)

# Distribución de grados

deg <- degree(g, mode="all")

hist(deg, breaks=1:vcount(g)-1, main="Histograma de los grados")

# Graficar Hubs 

hs <- hub_score(g, weights=NA)$vector

plot(g, 
     vertex.size=hs*50, 
     main="Hubs",
     rescale=F,
     layout=l2*1.5, 
     vertex.color="seagreen",
     vertex.label.color="black")

iii <- cut(hs, 
           breaks = seq(min(hs),
                        max(hs), 
                        len = 100), 
           include.lowest = TRUE) 

Colores <- colorRampPalette(c("lightblue", "blue" ))(99)[iii] 

V(g)$Hubs<- Colores

plot(g, 
     vertex.size=hs*50, 
     main="Hubs",
     layout=l2*1.5, 
     vertex.color=V(g)$Hubs,
     vertex.label.color= "#FFA07A",
     vertex.label.cex=1.9)

# Graficar Cliques

net.sym <- as.undirected(g, mode= "collapse")
vcol <- rep("grey80", vcount(net.sym))
vcol[unlist(largest_cliques(net.sym))] <- "gold"

plot(as.undirected(net.sym), 
     vertex.label=V(net.sym)$name, 
     vertex.color=vcol,
     layout=l2,
     rescale=F,
     layout=l2*1.5,
     vertex.label.color="black")

# Identificar grupos basados en la intermediación

ceb <- cluster_edge_betweenness(g)
dendPlot(ceb, mode="hclust") # Dendrograma

plot(ceb, g,
     rescale=F,
     layout=l3*1.5,
     vertex.label.color="black")

# Ver grupos
ceb [ 1:40 ]



