# Agrupamiento k-means
# Comandos básicos
setwd("/home/soyyo/projects/Modelos de datos/datosR")
rm(list=ls())

library(class)
library(ggplot2)
library(caret)
library(cluster)

# Cargamos datos y los limpiamos.
x <- read.csv("Heart.csv")
x <- na.omit(x)
x <- x[,-c(1,15)]
dmy <- dummyVars(" ~ .", x)
x <- data.frame(predict(dmy, x))
x <- scale(x)
row.has.na <- apply(x, 1, function(x){any(is.na(x))})
sum(row.has.na)
x <- x[!row.has.na,]

# Aplicamos modelos k-means.
set.seed(2)
km.out = kmeans(x, 2, nstart=10)
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
km.out =kmeans(x, 3, nstart=10)
km.out
par(mfrow=c(1,2))
plot(x, col=(km.out$cluster+1), main="K-Means Clustering con K=3", xlab="X[1]", ylab="X[2]", pch=20, cex=2)
grid(col="lightgray", lty="dotted", lwd=1, equilogs=TRUE)
points(km.out$centers,pch=3,cex=2,lwd=5,col="black")
clusplot(x, km.out$cluster, main="Representación 2D",
         color=TRUE, col.p=(km.out$cluster+1), col.clus=c("grey","grey","grey"), shade=TRUE, labels=0, lines=1,
         sub=paste("(between_SS / total_SS =   ", round(100 * (km.out$betweenss/km.out$totss), digits = 2), "%)"))
grid(col="lightgray", lty="dotted", lwd=1, equilogs=TRUE)

n <- 15
par(mfrow=c(1,1))
wss <- (nrow(x)-1)*sum(apply(x,2,var))
  for (i in 2:n) wss[i] <- sum(kmeans(x, centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Número de Clusters",
     ylab="Suma de cuadrados de los within groups",
     main="Valor óptimo de clusters mediante el método Elbow",
     pch=1, cex=2, col="red")
grid(col="lightgray", lty="dotted", lwd=1, equilogs=TRUE)

set.seed(4)
km.out=kmeans(x,7,nstart=10)
km.out
par(mfrow=c(1,2))
plot(x, col=(km.out$cluster+1), main="K-Means Clustering con K=7", xlab="X[1]", ylab="X[2]", pch=20, cex=2)
grid(col="lightgray", lty="dotted", lwd=1, equilogs=TRUE)
points(km.out$centers,pch=3,cex=2,lwd=5,col="black")
clusplot(x, km.out$cluster, main="Representación 2D",
         color=TRUE, col.p=(km.out$cluster+1), col.clus=c("grey", "grey", "grey", "grey", "grey", "grey", "grey"), shade=TRUE, labels=0, lines=1,
         sub=paste("(between_SS / total_SS =   ", round(100 * (km.out$betweenss/km.out$totss), digits = 2), "%)"))
grid(col="lightgray", lty="dotted", lwd=1, equilogs=TRUE)

set.seed(4)
km.out=kmeans(x,8,nstart=10)
km.out
par(mfrow=c(1,2))
plot(x, col=(km.out$cluster+1), main="K-Means Clustering con K=8", xlab="X[1]", ylab="X[2]", pch=20, cex=2)
grid(col="lightgray", lty="dotted", lwd=1, equilogs=TRUE)
points(km.out$centers,pch=3,cex=2,lwd=5,col="black")
clusplot(x, km.out$cluster, main="Representación 2D",
         color=TRUE, col.p=(km.out$cluster+1), col.clus=c("grey", "grey", "grey", "grey", "grey", "grey", "grey", "grey"), shade=TRUE, labels=0, lines=1,
         sub=paste("(between_SS / total_SS =   ", round(100 * (km.out$betweenss/km.out$totss), digits = 2), "%)"))
grid(col="lightgray", lty="dotted", lwd=1, equilogs=TRUE)

# Para que sean reproducibles nuestros experimentos es necesario documentar el ambiente en que ocurrieron.
sessionInfo()
