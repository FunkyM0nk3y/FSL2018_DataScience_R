# Selección
# Árboles
setwd("/home/soyyo/projects/Modelos de datos/datosR")
rm(list=ls())

library(caret)
library(tree)
library(ISLR)

# Árboles de clasificación
attach(Carseats)
High = ifelse(Sales<=8,"No", "Yes")
Carseats = data.frame(Carseats, High)
tree.carseats = tree(High~.-Sales, Carseats)
summary(tree.carseats)
dim(Carseats)
head(Carseats)

plot(tree.carseats)
title(main=list("Árbol para clasificación Carseats"), cex=1.2, col="black", font=4, cex=0.8)
text(tree.carseats, pretty=0, font=1, cex=0.7)
tree.carseats

set.seed(2)
train = sample(1:nrow(Carseats), 200)
Carseats.test = Carseats[-train, ]
High.test = High[-train]
tree.carseats = tree(High~.-Sales, Carseats, subset=train)
tree.pred = predict(tree.carseats, Carseats.test, type="class")
table(tree.pred, High.test)
confusionMatrix(tree.pred, High.test)
prune.carseats = prune.misclass(tree.carseats, best=9)

plot(prune.carseats)
title(main=list("Árbol para clasificación Carseats"), cex=1.2, col="black", font=4, cex=0.8)
text(prune.carseats, pretty=0, font=1, cex=0.7)
tree.pred = predict(prune.carseats, Carseats.test, type="class")
table(tree.pred, High.test)
confusionMatrix(tree.pred, High.test)
#(94+60)/200

# Árboles de regresión
library(MASS)

set.seed(1)
train = sample(1:nrow(Boston), nrow(Boston)/2)
tree.boston = tree(medv~., Boston, subset=train)
summary(tree.boston)
head(tree.boston)

plot(tree.boston)
text(tree.boston, pretty=0)
prune.boston = prune.tree(tree.boston, best=5)
plot(prune.boston)
text(prune.boston, pretty=0)
yhat = predict(tree.boston, newdata=Boston[-train, ])
boston.test = Boston[-train, "medv"]
plot(yhat, boston.test)
abline(0,1)
mean((yhat-boston.test)^2)

# Para que sean reproducibles nuestros experimentos es necesario documentar el ambiente en que ocurrieron.
sessionInfo()
