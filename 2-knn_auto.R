# Clasificación
# K-vecinos más cercanos.
rm(list=ls())
setwd("/home/soyyo/projects/Modelos de datos/EjemplosR")

library(MASS)
library(ISLR)
library(class)
library(lattice)
library(ggplot2)
library(caret)

# Generemos datos para entrenar y probar nuestro modelo.
summary(Default)
Df <- Default[, -2]
Df$balance <- scale(Df$balance)
Df$income <- scale(Df$income)
set.seed(3456)
trainIndex <- createDataPartition(Df$default, p=.75, list=FALSE, times=1)
train.X = Df[trainIndex, -1]
test.X = Df[-trainIndex, -1]
train.Y = Df[trainIndex, 1]
test.Y = Df[-trainIndex, 1]
summary(train.X)
summary(train.Y)
summary(test.X)
summary(test.Y)

# Modelo para 10 vecinos.
knn.pred = knn(train.X, test.X, train.Y, k=10)
exito_del_modelo <- mean(test.Y==knn.pred); exito_del_modelo   # Probabilidad de éxito del modelo
modelo <- table(knn.pred,test.Y); modelo
exactitud <- (modelo[1,"No"]+modelo[2,"Yes"])/(modelo[1,"No"]+modelo[1,"Yes"]+modelo[2,"No"]+modelo[2,"Yes"]); exactitud
sensitividad <- modelo[1,"No"]/(modelo[1,"No"] + modelo[2,"No"]); sensitividad
especificidad <- modelo[2,"Yes"]/(modelo[2,"Yes"] + modelo[1,"Yes"]); especificidad
confusionMatrix(knn.pred,test.Y)

# Automatizando encontrar el modelo de 1 a N vecinos.
n <- 20
exactitud_modelos <- rep(1:n)
especificidad_modelos <- rep(1:n)
sensitividad_modelos <- rep(1:n)
vecinos <- 1:n
for(x in vecinos) {
  knn.pred = knn(train.X, test.X, train.Y, k=x)
  modelo <- table(knn.pred, test.Y)
  exactitud_modelos[x] <- (modelo[1,"No"]+modelo[2,"Yes"])/(modelo[1,"No"]+modelo[1,"Yes"]+modelo[2,"No"]+modelo[2,"Yes"])
  especificidad_modelos[x] <- modelo[2,"Yes"]/(modelo[2,"Yes"] + modelo[1,"Yes"])
  sensitividad_modelos[x] <- modelo[1,"No"]/(modelo[1,"No"] + modelo[2,"No"])
  confusionMatrix(knn.pred,test.Y)
}

exactitud_modelos
especificidad_modelos
sensitividad_modelos

plot(vecinos, especificidad_modelos, type='b', col="red", pch=1, ylab="%", ylim=c(0,1))
lines(vecinos, sensitividad_modelos, type='b', col="green", pch=2, ylab="%")
lines(vecinos, exactitud_modelos, type='b', col="blue", pch=5, ylab="%")
title(main=list("Modelos knn"), cex=1.5, col="black", font=4)
legend("bottomright", pch=c(1,2,5), col=c("red", "green", "blue"),legend=c("especificidad", "sensitividad", "exactitud"), bty="n", cex=.8)
grid(nx=n, ny=n, col="lightgray", lty="dotted", lwd=1, equilogs=TRUE)

# Validando valores observados en la gráfica anterior.
n <- 5
exactitud_modelos <- rep(1:n)
especificidad_modelos <- rep(1:n)
sensitividad_modelos <- rep(1:n)
vecinos <- 1:n
for(x in vecinos) {
  knn.pred = knn(train.X, test.X, train.Y, k=x)
  predict(knn.pred, test.X, interval="confidence")
  modelo <- table(knn.pred, test.Y)
  exactitud_modelos[x] <- (modelo[1,"No"]+modelo[2,"Yes"])/(modelo[1,"No"]+modelo[1,"Yes"]+modelo[2,"No"]+modelo[2,"Yes"])
  especificidad_modelos[x] <- modelo[2,"Yes"]/(modelo[2,"Yes"] + modelo[1,"Yes"])
  sensitividad_modelos[x] <- modelo[1,"No"]/(modelo[1,"No"] + modelo[2,"No"])
  confusionMatrix(knn.pred,test.Y)
}

exactitud_modelos
especificidad_modelos
sensitividad_modelos

plot(vecinos, especificidad_modelos, type='b', col="red", pch=1, ylab="%", ylim=c(0,1))
lines(vecinos, sensitividad_modelos, type='b', col="green", pch=2, ylab="%")
lines(vecinos, exactitud_modelos, type='b', col="blue", pch=5, ylab="%")
title(main=list("Modelos knn"), cex=1.5, col="black", font=4)
legend("bottomright", pch=c(1,2,5), col=c("red", "green", "blue"),legend=c("especificidad", "sensitividad", "exactitud"), bty="n", cex=.8)
grid(nx=n, ny=n, col="lightgray", lty="dotted", lwd=1, equilogs=TRUE)

