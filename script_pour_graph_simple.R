#Nouveau script pour cartographier la participation des acteurs en prenant en compte uniquement les organisation. Utilisation de
#Igraph. Lien organisation --  comités

rm(list=ls())
setwd("/0_script_R/")

#chargement des packages
library(questionr)
library(igraph)
library(RColorBrewer)
library(Matrix)
library(ggplot2)
#library(network)
#library(sna)
#library(ndtv) #A utiliser avec network
#library(visNetwork) #A utiliser avec Igraph
#library(animation)
#library(statnet)

#chargement des données
links <- read.csv("jeu_acteur_participatory-research/PSR_edge.csv",
                  header=T,as.is=T,sep=";")
nodes <- read.csv("jeu_acteur_participatory-research/PSR_nodes.csv",
                  header=T,as.is=T,sep=";")


nrow(nodes); length(unique(nodes$id))
nrow(links); nrow(unique(links[,c("from", "to")]))

#Création du graph
net <- graph_from_data_frame(d=links, vertices=nodes, directed=F) #construction de l'objet igraph
class(net)

net

#dessin du graph
plot(net,vertex.label=V(net)$Nom)


V(net)$color <- c("steelblue", "steelblue","steelblue",
                  "chartreuse3")[V(net)$type3] #chartreuse (vert)=article de ref#Steelblue= conf, etc.;
                                                            

V(net)$color
l <- layout_with_kk(net)
plot(net,vertex.size=10,vertex.label=NA,layout=l)
deg <- degree(net)
deg.acteur <- degree(net,V(net)[type==F])
deg.event <- degree(net,V(net)[type==T])
hist(deg)
freq(deg.acteur,total=T)
freq(deg.event,total=T)
summary(deg)

#### Sous-graphe avec sommet de degré égal ou supérieur à 2

cut.off <- quantile(degree(net),prob=seq(0.25,0.25))
net2 <- delete_vertices(net,V(net)[deg<2])
net2
net2 <- delete_vertices(net2,V(net2)[Nom=="Dana Committee"])
net2
clu <- components(net2)
print(clu$no)
l <- layout_with_kk(net2)
plot(net2,vertex.label=V(net2)$Nom,layout=l)
deg2 <- degree(net2)
deg2.acteur <- degree(net2,V(net2)[type==F])
deg2.event <- degree(net2,V(net2)[type==T])
hist(deg2)
freq(deg2.acteur,total=T)
freq(deg2.event,total=T)

V(net2)$deg2 <- NA
V(net2)$deg2 <- deg2
V(net2)$deg2
V(net2)[type==F & deg2>2]


V(net2)$name <- V(net2)$Nom
net3m <- as_incidence_matrix(net2,names=T,sparse = F)


palf <- colorRampPalette(c("white", "black"))
heatmap(net3m, Rowv = T, Colv = T, col = palf(100),
        scale="none", margins=c(8,8))
nom.fichier <- paste("matrice_incidence","tiff",sep=".")
nom.fichier

dev.print(tiff, nom.fichier, res=600, height=10, width=10, units="in",
          compression = c("lzw"))






#Sous-graphes des événements et des acteurs
subnet <- bipartite.projection(net2)
subnet
subnet.a<- subnet$proj1 #"Réseau des acteurs
subnet.a$name <- "Réseau des acteurs"
subnet.a
subnet.b <- subnet$proj2#Réseau des événements
subnet.b$name <- "Réseau des événements"
subnet.b

freq(degree(subnet.a))
summary(degree(subnet.a))
output <- list(subnet.a,subnet.b)
nom_output<-c("Acteurs","Events")
class(output)

for(i in output){
  web <- i
  web
  
  #Attribut des noeuds
  deg <- degree(web)
  btw <- betweenness(web, v = V(web), directed=F,normalized = F)
  Orange <- colorRampPalette(c("yellow","red")) # du plus petit au plus grand degré
  black <- colorRampPalette(c("white","black")) # du plus petit au plus grand degré
  col <- Orange(max(btw)+1)
  col <- col[btw+1]
  col2 <- black(max(btw)+1)
  col2 <- col2[btw+1]
  
  V(web)$label.color <- col2
  V(web)$color <- col
  
  V(web)$deg <- deg
  
  #Tracé du graphe
  cut.off <- quantile(deg,prob=seq(0.25,0.25))
  
  j <- delete_vertices(web,V(web)[deg<0])
  
  
  
  plot(j, vertex.label=V(j)$Nom, vertex.size=10,vertex.shape="none",label.cex=9,
       main=paste0(web$name, 
                   " \ndont le degré de centralité est supérieur à ", 
                   cut.off, " [1er quartile]"))
  
  plot(j, vertex.label=V(j)$id, vertex.size=10,vertex.shape="circle",
       main=paste0(web$name, 
                   " \ndont le degré de centralité est supérieur à ", 
                   cut.off, " (1er quartile)"))
  
  #tracé des matrices
  V(web)$name <- V(web)$Nom
  webm <- as_adjacency_matrix(web,names=T,sparse = F)
  webm
  
  palf <- colorRampPalette(c("white", "black"))
  heatmap(webm, Rowv = T, Colv = T, col = palf(100),
          scale="none", margins=c(8,8),keep.dendro = TRUE)
  
  nom.fichier <- paste(web$name,"tiff",sep=".")

  dev.print(tiff, nom.fichier, res=600, height=10, width=10, units="in",
            compression = c("lzw"))

  
}


##### Découpage temporel

class(E(net2)$start)
year <- c(1990:2018)
year

for(i in year){
  web <- delete.edges(net2,E(net2)[start != i])
  web$name <- i
  if (ecount(web) > 0){
    print(web)
    deg <- degree(web)
    subweb <- delete.vertices(web,V(web)[deg < 1])
    plot(subweb,main=paste0("Réseau de l'année ", i), vertex.label=V(subweb)$Nom)}
}

class(E(net2)$start)
year <- c(1990,1995,2000,2005,2010,2015,2020)
year

for(i in year){
  web <- delete.edges(net2,E(net2)[start > i])
  web$name <- i
  if (ecount(web) > 0){
    print(web)
    deg <- degree(web)
    web <- delete.vertices(web,V(web)[deg == 0])
    web. <- bipartite.projection(web)
    web.a<- web.$proj2 #"Réseau des événements
    web.a$name <- "Réseau des événements"
    deg.a <- degree(web.a)
    btw.a <- betweenness(web.a, v = V(web.a), directed=F,normalized = F)
    Orange <- colorRampPalette(c("yellow","red")) # du plus petit au plus grand degré
    col <- Orange(max(btw.a)+1)
    col <- col[btw.a+1]
    
    black <- colorRampPalette(c("white","black")) # du plus petit au plus grand degré
    col2 <- black(max(btw.a)+1)
    col2 <- col2[btw.a+1]
    


    #On détermine la couleur des noeuds en fonction de leur degré de centralité
    #V(web.a)$label.color <- col
    V(web.a)$color <- col
    #V(web.a)$label.color <- col2
    #V(web.a)$label.color <- black
    plot(web.a,vertex.label= V(web.a)$Nom, main= paste0(web.a$name," avant l'année ", i))
    
    #Matrice
    V(web.a)$name <- V(web.a)$Nom
    web.m <- as_adjacency_matrix(web.a,names=T,sparse = F)
    
    
    palf <- colorRampPalette(c("white", "black"))
    heatmap(web.m, Rowv = T, Colv = T, col = palf(100),
            scale="none", margins=c(8,8),main= paste0(web.a$name," avant l'année ", i))
    
    }
}

for(i in year){
  web <- delete.edges(net2,E(net2)[start > i])
  web$name <- i
  if (ecount(web) > 0){
    print(web)
    deg <- degree(web)
    web <- delete.vertices(web,V(web)[deg == 0])
    web. <- bipartite.projection(web)
    web.a<- web.$proj1 #"Réseau des acteurs
    web.a$name <- "Réseau des acteurs"
    deg.a <- degree(web.a)
    btw.a <- betweenness(web.a, v = V(web.a), directed=F,normalized = F)
    Orange <- colorRampPalette(c("yellow","red")) # du plus petit au plus grand degré
    col <- Orange(max(btw.a)+1)
    col <- col[btw.a+1]
    #On détermine la couleur des noeuds en fonction de leur degré de centralité
    #V(web)$label.color <- col
    V(web.a)$color <- col
    plot(web.a,vertex.label= V(web.a)$Nom, main= paste0(web.a$name," avant ", i))
    }
}

for(i in year){
  web <- delete.edges(net2,E(net2)[start > i])
  web$name <- i
  if (ecount(web) > 0){
    print(web)
    deg <- degree(web)
    web <- delete.vertices(web,V(web)[deg < 3])
    btw <- betweenness(web, v = V(web), directed=F,normalized = F)
    Orange <- colorRampPalette(c("yellow","red")) # du plus petit au plus grand degré
    col <- Orange(max(btw)+1)
    col <- col[btw+1]
    #On détermine la couleur des noeuds en fonction de leur degré de centralité
    #V(web)$label.color <- col
    V(web)$color <- col
    plot(web,vertex.label= V(web)$Nom, main= paste0("Réseau des sommets avant ", i, " de degré > à 1"))
  }
}
