# Selección del modelo.
# Bagging y bosques aleatorios.
setwd("/home/soyyo/projects/Modelos de datos/EjemplosR")
rm(list=(ls()))

library(randomForest)
library(MASS)

set.seed(1)
train = sample(1:nrow(Boston), nrow(Boston)/2)
boston.test = Boston[-train, "medv"]

# Random Forest
bag.boston = randomForest(medv~., data=Boston, subset=train, mtry=13, importance=TRUE, ntree=500)
bag.boston
yhat.bag = predict(bag.boston, newdata=Boston[-train, ])
plot(yhat.bag, boston.test)
abline(0, 1, col="red")
mean((yhat.bag-boston.test)^2)

# Bagging
bag.boston = randomForest(medv~., data=Boston, subset=train, mtry=13, ntree=25)
yhat.bag = predict(bag.boston, newdata=Boston[-train, ])
mean((yhat.bag-boston.test)^2)

set.seed(1)
rf.boston = randomForest(medv~., data=Boston, subset=train, mtry=6, importance=TRUE)
yhat.rf = predict(rf.boston, newdata=Boston[-train, ])
mean((yhat.rf-boston.test)^2)

# Analicemos la importancia de las variables.
importance(rf.boston)
varImpPlot(rf.boston)

# Para que sean reproducibles nuestros experimentos es necesario documentar el ambiente en que ocurrieron.
sessionInfo()
