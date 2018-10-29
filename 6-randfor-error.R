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

# Random Forest con 7 argumentos.
set.seed(1)
rfM7.auto = randomForest(mpg~., data=Auto, subset=train, mtry=7, importance=TRUE)
yhat.rfM7 = predict(rfM7.auto, newdata=Auto[-train, ])
mean((yhat.rfM7 - auto.test)^2)
# Analicemos la importancia de las variables.
importance(rfM7.auto)
varImpPlot(rfM7.auto)

# Random Forest con 3 argumentos.
set.seed(1)
rfM3.auto = randomForest(mpg~., data=Auto, subset=train, mtry=3, importance=TRUE)
yhat.rfM3 = predict(rfM3.auto, newdata=Auto[-train, ])
mean((yhat.rfM3 - auto.test)^2)
# Analicemos la importancia de las variables.
importance(rfM3.auto)
varImpPlot(rfM3.auto)

# Graficamos un modelo contra el otro.
par(mfrow=c(2,2))
plot(yhat.rfM7, auto.test, pch=19)
abline(0, 1, col="red")
grid(col="lightgray", lty="dotted", lwd=1, equilogs=TRUE)
plot(rfM7.auto)
grid(col="lightgray", lty="dotted", lwd=1, equilogs=TRUE)
plot(yhat.rfM3, auto.test, pch=19)
abline(0, 1, col="red")
grid(col="lightgray", lty="dotted", lwd=1, equilogs=TRUE)
plot(rfM3.auto)
grid(col="lightgray", lty="dotted", lwd=1, equilogs=TRUE)

# Probamos el error para todas las combinaciones de variables.
oob.err=double(7)
test.err=double(7)

for(mtry in 1:7) {
  rf.auto = randomForest(mpg~., data=Auto, subset=train, mtry=mtry, importance=TRUE) 
  oob.err[mtry] = rf.auto$mse[500]
  
  pred <- predict(rf.auto, Auto[-train, ])
  test.err[mtry] = with(Auto[-train, ], mean( (mpg - pred)^2))
  
  cat(mtry, " ")
}

oob.err; test.err
par(mfrow=c(1,1))
matplot(1:mtry, cbind(oob.err,test.err), pch=19 , col=c("red","blue"), type="b", ylab="Mean Squared Error", xlab="Number of Predictors Considered at each Split")
legend("topright", legend=c("Out of Bag Error","Test Error"), pch=19, col=c("red","blue"), cex=.7)
grid(col="lightgray", lty="dotted", lwd=1, equilogs=TRUE)

# Para que sean reproducibles nuestros experimentos es necesario documentar el ambiente en que ocurrieron.
sessionInfo()
