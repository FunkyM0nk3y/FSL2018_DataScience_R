# Clasificación
# Dendogramas
rm(list=ls())
setwd("/home/soyyo/projects/Modelos de datos/datosR")

library(dendextend)

# Primero cargamos datos, los limpiamos y los exploramos.
credit = read.csv("Credit.csv")
View(credit)
dim(credit)
summary(credit)
head(credit)
credit <- credit[, -1]
credit <- na.omit(credit)
View(credit)
dmy <- dummyVars(" ~ .", credit)
credit <- data.frame(predict(dmy, credit))
View(credit)
credit <- scale(credit)

# Probamos con el método completo.
hc.complete = hclust(dist(credit), method="complete")
plot(hc.complete, main="Complete Linkage", cex=.9, hang=-1)
abline(h=11, col="red")
abline(h=9, col="red")
abline(h=8, col="red")
abline(h=7, col="red")

dend.complete <- as.dendrogram(hc.complete)
plot(dend.complete)

dend.complete8 <- as.dendrogram(hc.complete)
n <- 8
dend.complete8 <- color_branches(dend.complete8, k=n)
dend.complete8 <- color_labels(dend.complete8, k=n)
plot(dend.complete8)
abline(h=11, col="red")
abline(h=9, col="red")
abline(h=8, col="red")
abline(h=7, col="red")
cutree(dend.complete8, n)

dend.complete6 <- as.dendrogram(hc.complete)
n <- 6
dend.complete6 <- color_branches(dend.complete6, k=n)
dend.complete6 <- color_labels(dend.complete6, k=n)
plot(dend.complete6)
abline(h=11, col="red")
abline(h=9, col="red")
abline(h=8, col="red")
abline(h=7, col="red")
cutree(dend.complete6, n)

dend.complete4 <- as.dendrogram(hc.complete)
n <- 4
dend.complete4 <- color_branches(dend.complete4, k=n)
dend.complete4 <- color_labels(dend.complete4, k=n)
plot(dend.complete4)
abline(h=11, col="red")
abline(h=9, col="red")
abline(h=8, col="red")
abline(h=7, col="red")
cutree(dend.complete4, n)

dend.complete2 <- as.dendrogram(hc.complete)
n <- 2
dend.complete2 <- color_branches(dend.complete2, k=n)
dend.complete2 <- color_labels(dend.complete2, k=n)
plot(dend.complete2)
abline(h=11, col="red")
abline(h=9, col="red")
abline(h=8, col="red")
abline(h=7, col="red")
cutree(dend.complete2, n)

par(mfrow=c(2, 2))
plot(dend.complete8, main="8 grupos")
abline(h=11, col="red")
abline(h=9, col="red")
abline(h=8, col="red")
abline(h=7, col="red")
plot(dend.complete6, main="6 grupos")
abline(h=11, col="red")
abline(h=9, col="red")
abline(h=8, col="red")
abline(h=7, col="red")
plot(dend.complete4, main="4 grupos")
abline(h=11, col="red")
abline(h=9, col="red")
abline(h=8, col="red")
abline(h=7, col="red")
plot(dend.complete2, main="2 grupos")
abline(h=11, col="red")
abline(h=9, col="red")
abline(h=8, col="red")
abline(h=7, col="red")

# Método promedio.
par(mfrow=c(1,1))
hc.average=hclust(dist(credit), method="average")
dend.average <- as.dendrogram(hc.average)
plot(hc.average, main="Average Linkage", cex=.9, hang=-1)
abline(h=6, col="red")
abline(h=5, col="red")
abline(h=4, col="red")
abline(h=3, col="red")

dend.average8 <- as.dendrogram(hc.average)
n <- 8
dend.average8 <- color_branches(dend.average8, k=n)
dend.average8 <- color_labels(dend.average8, k=n)
plot(dend.average8)
abline(h=6, col="red")
abline(h=5, col="red")
abline(h=4, col="red")
abline(h=3, col="red")
cutree(dend.average8, n)

dend.average5 <- as.dendrogram(hc.average)
n <- 5
dend.average5 <- color_branches(dend.average5, k=n)
dend.average5 <- color_labels(dend.average5, k=n)
plot(dend.average5)
abline(h=6, col="red")
abline(h=5, col="red")
abline(h=4, col="red")
abline(h=3, col="red")
cutree(dend.average5, n)

dend.average4 <- as.dendrogram(hc.average)
n <- 4
dend.average4 <- color_branches(dend.average4, k=n)
dend.average4 <- color_labels(dend.average4, k=n)
plot(dend.average4)
abline(h=6, col="red")
abline(h=5, col="red")
abline(h=4, col="red")
abline(h=3, col="red")
cutree(dend.average4, n)

dend.average2 <- as.dendrogram(hc.average)
n <- 2
dend.average2 <- color_branches(dend.average2, k=n)
dend.average2 <- color_labels(dend.average2, k=n)
plot(dend.average2)
abline(h=6, col="red")
abline(h=5, col="red")
abline(h=4, col="red")
abline(h=3, col="red")
cutree(dend.average2, n)

par(mfrow=c(2, 2))
plot(dend.average8, main="8 grupos")
abline(h=6, col="red")
abline(h=5, col="red")
abline(h=4, col="red")
abline(h=3, col="red")
plot(dend.average5, main="5 grupos")
abline(h=6, col="red")
abline(h=5, col="red")
abline(h=4, col="red")
abline(h=3, col="red")
plot(dend.average4, main="4 grupos")
abline(h=6, col="red")
abline(h=5, col="red")
abline(h=4, col="red")
abline(h=3, col="red")
plot(dend.average2, main="2 grupos")
abline(h=6, col="red")
abline(h=5, col="red")
abline(h=4, col="red")
abline(h=3, col="red")

plot(hc.complete, main="Complete Linkage", cex=.9, hang=-1)
plot(hc.average, main="Average Linkage", cex=.9, hang=-1)
plot(dend.complete4, main="4 grupos")
plot(dend.average4, main="4 grupos")

# Para que sean reproducibles nuestros experimentos es necesario documentar el ambiente en que ocurrieron.
sessionInfo()
