# 
# Bagging y Random forest
setwd("/home/soyyo/projects/Modelos de datos/datosR")
rm(list=ls())

library(randomForest)
library(ISLR)

# Agregamos los datos y generemos muestra.
attach(Auto)
Auto <- Auto[, -9]
Auto <- na.omit(Auto)
dim(Auto)
train = sample(1:nrow(Auto), nrow(Auto)/2)
auto.test = Auto[-train, "mpg"]

# Bagging con 100 bags 7 argumentos.
bag.autoM7T100 = randomForest(mpg~., data=Auto, subset=train, mtry=7, ntree=100, importance=TRUE)
yhat.bag = predict(bag.autoM7T100, newdata=Auto[-train, ])
plot(yhat.bag, auto.test, pch=19)
abline(0, 1, col="red")
mean((yhat.bag - auto.test)^2)
# Analicemos la importancia de las variables.
importance(bag.autoM7T100)
varImpPlot(bag.autoM7T100)
plot(bag.autoM7T100)

# Bagging con 100 bags 4 argumentos.
bag.autoM4T100 = randomForest(mpg~., data=Auto, subset=train, mtry=4, ntree=100, importance=TRUE)
yhat.bag = predict(bag.autoM4T100, newdata=Auto[-train, ])
plot(yhat.bag, auto.test, pch=19)
abline(0, 1, col="red")
mean((yhat.bag - auto.test)^2)
# Analicemos la importancia de las variables.
importance(bag.autoM4T100)
varImpPlot(bag.autoM4T100)
plot(bag.autoM4T100)

# Bagging con 25 bags 7 argumentos.
bag.autoM7T25 = randomForest(mpg~., data=Auto, subset=train, mtry=7, ntree=25, importance=TRUE)
yhat.bag = predict(bag.autoM7T25, newdata=Auto[-train, ])
plot(yhat.bag, auto.test, pch=19)
abline(0, 1, col="red")
mean((yhat.bag - auto.test)^2)
# Analicemos la importancia de las variables.
importance(bag.autoM7T25)
varImpPlot(bag.autoM7T25)
plot(bag.autoM7T25)

# Bagging con 25 bags 4 argumentos.
bag.autoM4T25 = randomForest(mpg~., data=Auto, subset=train, mtry=4, ntree=25, importance=TRUE)
yhat.bag = predict(bag.autoM4T25, newdata=Auto[-train, ])
plot(yhat.bag, auto.test, pch=19)
abline(0, 1, col="red")
mean((yhat.bag - auto.test)^2)
# Analicemos la importancia de las variables.
importance(bag.autoM4T25)
varImpPlot(bag.autoM4T25)
plot(bag.autoM4T25)

# Random Forest con 7 argumentos.
set.seed(1)
rf.autoM7 = randomForest(mpg~., data=Auto, subset=train, mtry=7, importance=TRUE)
yhat.rf = predict(rf.autoM7, newdata=Auto[-train, ])
plot(yhat.rf, auto.test, pch=19)
abline(0, 1, col="red")
mean((yhat.rf - auto.test)^2)
# Analicemos la importancia de las variables.
importance(rf.autoM7)
varImpPlot(rf.autoM7)
plot(rf.autoM7)

# Random Forest con 4 argumentos.
set.seed(1)
rf.autoM3 = randomForest(mpg~., data=Auto, subset=train, mtry=3, importance=TRUE)
yhat.rf = predict(rf.autoM3, newdata=Auto[-train, ])
plot(yhat.rf, auto.test, pch=19)
abline(0, 1, col="red")
mean((yhat.rf - auto.test)^2)
# Analicemos la importancia de las variables.
importance(rf.autoM3)
varImpPlot(rf.autoM3)
plot(rf.autoM3)

# Para que sean reproducibles nuestros experimentos es necesario documentar el ambiente en que ocurrieron.
sessionInfo()
