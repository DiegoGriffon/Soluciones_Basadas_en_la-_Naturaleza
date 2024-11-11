
# ///////////Análisis de Redes /////////////////
# ///////Resultados de Tams Analizar////////////
# ////////////////Los CONTRA///////////////////////
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
# Cargar matriz excel (aquí llamada RedContra)


# Grafo red total

MatrizContra <- RedContra
MatrizContra
class(MatrizContra)
MatrizContra<-as.matrix(MatrizContra)
class(MatrizContra)
NombresContra <- colnames(MatrizContra)
NombresContra
colnames(MatrizContra)<-NULL
MatrizContra

?graph.adjacency

gContra <- graph.adjacency(MatrizContra, mode="undirected", weighted= T)

V(gContra)$name  <- NombresContra
V(gContra)$name 

plot(gContra, 
     vertex.color="blue")


# Hacer que los vínculos tengan un tamaño
# proporcional a sus pesos
plot(gContra, edge.width=E(gContra)$weight, 
     vertex.color="blue")


# Formas de los grafos
??layouts
l1 <- layout_on_sphere(gContra)
l2 <- layout_in_circle(gContra)
l3 <- layout_with_fr(gContra)
l4 <- layout_with_kk(gContra)
l5 <- layout_on_grid(gContra)

plot(gContra, 
     edge.width=E(gContra)$weight*0.2, 
     vertex.color="blue",
     layout=l1)

plot(gContra, 
     edge.width=E(gContra)$weight*0.2, 
     vertex.color="blue",
     layout=l2)

plot(gContra, 
     edge.width=E(gContra)$weight*0.8, 
     vertex.color="pink",
     layout=l3)

plot(gContra, 
     edge.width=E(gContra)$weight*0.8, 
     vertex.color="green",
     layout=l4)

plot(gContra, 
     edge.width=E(gContra)$weight*0.2, 
     vertex.color="green",
     layout=l5)

# Tamaño del grafo
l3 <- norm_coords(l3, ymin=-1, ymax=1, xmin=-1, xmax=1)

plot(gContra, 
     edge.width=E(gContra)$weight*0.2, 
     vertex.color="pink",
     rescale=F,
     layout=l3*2)

l2 <- norm_coords(l2, ymin=-1, ymax=1, xmin=-1, xmax=1)

plot(gContra, 
     edge.width=E(gContra)$weight*0.2, 
     vertex.color="pink",
     rescale=F,
     layout=l2*1.5,
     vertex.label.color="black")


# Índices varios

edge_density(gContra, loops=F) 

transitivity(gContra, type="global")

reciprocity(gContra)

mean_distance(gContra, directed=T)

diameter(gContra, directed=F, weights=NA)

betweenness(gContra, directed=F, weights=NA)

degree(gContra)

# Distribución de grados

deg <- degree(gContra, mode="all")

hist(deg, breaks=1:vcount(gContra)-1, main="Histograma de los grados")

# Graficar Hubs 

hs <- hub_score(gContra, weights=NA)$vector
hs
plot(gContra, 
     vertex.size=hs*50, 
     main="Hubs",
     rescale=F,
     layout=l2*1.5, 
     vertex.color="skyblue",
     vertex.label.color="black")

# Graficar Cliques

net.sym <- as.undirected(gContra, mode= "collapse")
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

ceb <- cluster_edge_betweenness(gContra)
dendPlot(ceb, mode="hclust") # Dendrograma

plot(ceb, gContra,
     rescale=F,
     layout=l3*1.5,
     vertex.label.color="black") # En el grafo

# Grupos:
ceb [ 1:20 ]


