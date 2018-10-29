# Agrupamiento de datos
# Agrupamiento K-Means
setwd("/home/soyyo/projects/Modelos de datos/EjemplosR")
rm(list=ls())

library(cluster)

set.seed(2)
x=matrix(rnorm(50*2), ncol=2); x
x[1:25,1]=x[1:25,1]+3; x[1:25,1]
x[1:25,2]=x[1:25,2]-4; x[1:25,2]
km.out=kmeans(x,2,nstart=20)
km.out$cluster
par(mfrow=c(1,2))

plot(x, col=(km.out$cluster+1), main="K-Means Clustering con K=2", xlab="X[1]", ylab="X[2]", pch=20, cex=2)
grid(col="lightgray", lty="dotted", lwd=1, equilogs=TRUE)
points(km.out$centers,pch=3,cex=2,lwd=5,col="black")
clusplot(x, km.out$cluster, main="Representación 2D",
         color=TRUE, col.p=(km.out$cluster+1), col.clus=c("grey","grey"), shade=TRUE, labels=0, lines=1,
         sub=paste("(between_SS / total_SS =   ", round(100 * (km.out$betweenss/km.out$totss), digits = 2), "%)"))
grid(col="lightgray", lty="dotted", lwd=1, equilogs=TRUE)

set.seed(4)
km.out=kmeans(x,3,nstart=20)
km.out
par(mfrow=c(1,2))

plot(x, col=(km.out$cluster+1), main="K-Means Clustering con K=3", xlab="X[1]", ylab="X[2]", pch=20, cex=2)
grid(col="lightgray", lty="dotted", lwd=1, equilogs=TRUE)
points(km.out$centers,pch=3,cex=2,lwd=5,col="black")
clusplot(x, km.out$cluster, main="Representación 2D",
         color=TRUE, col.p=(km.out$cluster+1), col.clus=c("grey","grey","grey"), shade=TRUE, labels=0, lines=1,
         sub=paste("(between_SS / total_SS =   ", round(100 * (km.out$betweenss/km.out$totss), digits = 2), "%)"))
grid(col="lightgray", lty="dotted", lwd=1, equilogs=TRUE)

set.seed(3)
km.out=kmeans(x,3,nstart=1)
km.out$tot.withinss
km.out=kmeans(x,3,nstart=20)
km.out$tot.withinss

n <- 15
par(mfrow=c(1,1))
wss <- (nrow(x)-1)*sum(apply(x,2,var))
  for (i in 2:n) wss[i] <- sum(kmeans(x, centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Número de Clusters",
     ylab="Suma de cuadrados de los within groups",
     main="Valor óptimo de clusters mediante el método Elbow",
     pch=1, cex=2, col="red")
grid(col="lightgray", lty="dotted", lwd=1, equilogs=TRUE)

km.out=kmeans(x,4,nstart=1)
km.out$tot.withinss
par(mfrow=c(1,2))
plot(x, col=(km.out$cluster+1), main="K-Means Clustering con K=4", xlab="X[1]", ylab="X[2]", pch=20, cex=2)
grid(col="lightgray", lty="dotted", lwd=1, equilogs=TRUE)
points(km.out$centers,pch=3,cex=2,lwd=5,col="black")
clusplot(x, km.out$cluster, main="Representación 2D",
         color=TRUE, col.p=(km.out$cluster+1), col.clus=c("grey","grey","grey","grey"), shade=TRUE, labels=0, lines=1,
         sub=paste("(between_SS / total_SS =   ", round(100 * (km.out$betweenss/km.out$totss), digits = 2), "%)"))
grid(col="lightgray", lty="dotted", lwd=1, equilogs=TRUE)

# Agrupamiento jerárquico
hc.complete=hclust(dist(x), method="complete")
hc.average=hclust(dist(x), method="average")
hc.single=hclust(dist(x), method="single")
par(mfrow=c(1,3))

plot(hc.complete,main="Complete Linkage", xlab="", sub="", cex=.9)
plot(hc.average, main="Average Linkage", xlab="", sub="", cex=.9)
plot(hc.single, main="Single Linkage", xlab="", sub="", cex=.9)
cutree(hc.complete, 2)
cutree(hc.average, 2)
cutree(hc.single, 2)
cutree(hc.single, 4)

# Agrupamiento de datos de diferentes tipos de cáncer
# Conjunto de datos NCI60
library(ISLR)
nci.labs=NCI60$labs
nci.data=NCI60$data
dim(nci.data)
nci.labs[1:4]
table(nci.labs)

sd.data=scale(nci.data)
par(mfrow=c(1,3))
data.dist=dist(sd.data)

plot(hclust(data.dist), labels=nci.labs, main="Complete Linkage", xlab="", sub="",ylab="")
plot(hclust(data.dist, method="average"), labels=nci.labs, main="Average Linkage", xlab="", sub="",ylab="")
plot(hclust(data.dist, method="single"), labels=nci.labs,  main="Single Linkage", xlab="", sub="",ylab="")
hc.out=hclust(dist(sd.data))
hc.clusters=cutree(hc.out,4)

table(hc.clusters,nci.labs)
par(mfrow=c(1,1))

plot(hc.out, labels=nci.labs)
abline(h=139, col="red")
hc.out

set.seed(2)
km.out=kmeans(sd.data, 4, nstart=20)
km.clusters=km.out$cluster
table(km.clusters,hc.clusters)

# Para que sean reproducibles nuestros experimentos es necesario documentar el ambiente en que ocurrieron.
sessionInfo()
