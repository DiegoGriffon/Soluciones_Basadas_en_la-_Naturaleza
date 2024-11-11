
# ///////////Análisis de Redes /////////////////
# ///////Resultados de Tams Analizar////////////
# ////////////////Los PRO///////////////////////
# Una vez efectuada la codificación de los textos.

# Ir a la pestaña de Reports y buscar: 
"Graph/Chart co-coding frequency"
# que es la primera opción.

# Una vez obtenida la tabla, buscar la pestaña de File y buscar
# Save To. 
# Escoger el formato (file format) tabbed text.

# Abrir el archivo de texto y copiar todo, luego pegarlo en 
# un archivo excel. 
# Eliminar la primera columna y llamar al achivo Red
# Listo

library(igraph)
# Cargar matriz excel (aqui llamada RedPro)


# Grafo red total

MatrizPro <- RedPro
MatrizPro
class(MatrizPro)
MatrizPro<-as.matrix(MatrizPro)
class(MatrizPro)
NombresPro <- colnames(MatrizPro)
NombresPro
colnames(MatrizPro)<-NULL
MatrizPro

?graph.adjacency

gPro <- graph.adjacency(MatrizPro, mode="undirected", weighted= T)

V(gPro)$name  <- NombresPro
V(gPro)$name 

plot(gPro, 
     vertex.color="green")


# Hacer que los vínculos tengan un tamaño
# proporcional a sus pesos
plot(gPro, edge.width=E(gPro)$weight, 
     vertex.color="green")


# Formas de los grafos
??layouts
l1 <- layout_on_sphere(gPro)
l2 <- layout_in_circle(gPro)
l3 <- layout_with_fr(gPro)
l4 <- layout_with_kk(gPro)
l5 <- layout_on_grid(gPro)

plot(gPro, 
     edge.width=E(gPro)$weight*0.2, 
     vertex.color="green",
     layout=l1)

plot(gPro, 
     edge.width=E(gPro)$weight*0.2, 
     vertex.color="green",
     layout=l2)

plot(gPro, 
     edge.width=E(gPro)$weight*0.8, 
     vertex.color="green",
     layout=l3)

plot(gPro, 
     edge.width=E(gPro)$weight*0.8, 
     vertex.color="green",
     layout=l4)

plot(gPro, 
     edge.width=E(gPro)$weight*0.2, 
     vertex.color="green",
     layout=l5)

# Tamaño del grafo
l3 <- norm_coords(l3, ymin=-1, ymax=1, xmin=-1, xmax=1)

plot(gPro, 
     edge.width=E(gPro)$weight*0.2, 
     vertex.color="skyblue",
     rescale=F,
     layout=l3*2)

l2 <- norm_coords(l2, ymin=-1, ymax=1, xmin=-1, xmax=1)

plot(gPro, 
     edge.width=E(gPro)$weight*0.2, 
     vertex.color="skyblue",
     rescale=F,
     layout=l2*1.5,
     vertex.label.color="black")


# Índices varios

edge_density(gPro, loops=F) 

transitivity(gPro, type="global")

reciprocity(gPro)

mean_distance(gPro, directed=T)

diameter(gPro, directed=F, weights=NA)

betweenness(gPro, directed=F, weights=NA)

degree(gPro)

# Distribución de grados

deg <- degree(gPro, mode="all")

hist(deg, breaks=1:vcount(gPro)-1, main="Histograma de los grados")

# Graficar Hubs 

hs <- hub_score(gPro, weights=NA)$vector
hs
plot(gPro, 
     vertex.size=hs*50, 
     main="Hubs",
     rescale=F,
     layout=l2*1.5, 
     vertex.color="skyblue",
     vertex.label.color="black")

# Graficar Cliques

net.sym <- as.undirected(gPro, mode= "collapse")
vcol <- rep("grey80", vcount(net.sym))
vcol[unlist(largest_cliques(net.sym))] <- "gold"

plot(as.undirected(net.sym), 
     vertex.label=V(net.sym)$name, 
     vertex.color=vcol,
     rescale=F,
     layout=l1*1.5,
     vertex.label.color="black")

largest_cliques(net.sym)

# Identificar grupos basado en la intermediación

ceb <- cluster_edge_betweenness(gPro)
dendPlot(ceb, mode="hclust") # Dendrograma

plot(ceb, gPro,
     rescale=F,
     layout=l3*1.5,
     vertex.label.color="black") # En el grafo

# Grupos:
ceb [ 1:20 ]


