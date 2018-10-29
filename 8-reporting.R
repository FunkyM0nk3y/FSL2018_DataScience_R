# Reporteo
# Código literado
rm(list=ls())
setwd("/home/soyyo/projects/Modelos de datos/datosR")

library(kernlab)
library(boot)

## Cargar el dataset de SPAM
data(spam)
str(spam[, 1:5])

## Realizar un submuestreo para entrenamiento y prueba
set.seed(3435)
trainIndicator = rbinom(4601, size = 1, prob = 0.5)
str(trainIndicator)
table(trainIndicator)
spam <- cbind(spam, trainIndicator)
str(spam)
spam$trainIndicator

trainSpam = spam[which(trainIndicator == 1), ]
testSpam = spam[which(trainIndicator == 0), ]

## Inspeccionar el set de entrenamiento
head(names(trainSpam), 20)
head(trainSpam[, 1:10])
table(trainSpam$type)

boxplot(capitalAve ~ type, data = trainSpam)
boxplot(log10(capitalAve + 1) ~ type, data = trainSpam)
pairs(log10(trainSpam[, 1:4] + 1))

hCluster = hclust(dist(t(trainSpam[, 1:57])))
plot(hCluster)
hClusterUpdated = hclust(dist(t(log10(trainSpam[, 1:55] + 1))))
plot(hClusterUpdated)

## Inspeccionar el set de pruebas
head(names(testSpam), 20)
head(testSpam[, 1:10])
table(testSpam$type)

boxplot(capitalAve ~ type, data = testSpam)
boxplot(log10(capitalAve + 1) ~ type, data = testSpam)
pairs(log10(testSpam[, 1:4] + 1))

hCluster = hclust(dist(t(testSpam[, 1:57])))
plot(hCluster)
hClusterUpdated = hclust(dist(t(log10(testSpam[, 1:55] + 1))))
plot(hClusterUpdated)

## Entrenar al modelo de predicción de SPAM
str(trainSpam$type)
trainSpam$numType = as.numeric(trainSpam$type) - 1
costFuction = function(x, y) sum(x != (y > 0.5))
cvError = rep(NA, 55)

for (i in 1:55) {
  lmFormula = reformulate(names(trainSpam)[i], response = "numType")
  glmFit = glm(lmFormula, family = "binomial", data = trainSpam)
  cvError[i] = cv.glm(trainSpam, glmFit, costFuction, 2)$delta[2]
}
warnings()

## ¿Qué predictor tiene el mínimo error de validación cruzada?
names(trainSpam)[which.min(cvError)]

## Usar el mejor modelo del grupo.
predictionModel = glm(numType ~ charDollar, family = "binomial", data = trainSpam)

## Tomar las predicciones del mejor set.
predictionTest = predict(predictionModel, testSpam)
predictedSpam = rep("nonspam", dim(testSpam)[1])

## Clasificar como 'SPAM' aquellos con probabilidad > 0.5
predictedSpam[predictionModel$fitted > 0.5] = "spam"

table(predictedSpam, testSpam$type)

## Error rate
(61 + 458)/(1346 + 458 + 61 + 449)

# Para que sean reproducibles nuestros experimentos es necesario documentar el ambiente en que ocurrieron.
sessionInfo()
