# Regresión lineal

setwd("/home/soyyo/projects/Modelos de datos/EjemplosR")
library(MASS)
library(ISLR)

# Regresión lineal simple
fix(Boston)
View(Boston)
?Boston
names(Boston)
summary( Boston)
# La siguiente línea generan un error por el alcance de las variables.
lm.fit = lm(medv~lstat)
lm.fit = lm(medv~lstat, data=Boston)
attach(Boston)
lm.fit = lm(medv~lstat)
lm.fit
summary(lm.fit)
names(lm.fit)
coef(lm.fit)
confint(lm.fit)
predict(lm.fit, data.frame(lstat=(c(5,10,15))), interval="confidence")
predict(lm.fit, data.frame(lstat=(c(5,10,15))), interval="prediction")

# Gráficas y algunos detalles de uso.
plot(lstat, medv)
abline(lm.fit)
abline(lm.fit, lwd=3)
abline(lm.fit, lwd=3, col="red")
plot(lstat, medv, col="red")
plot(lstat, medv, pch=20)
plot(lstat, medv, pch="+")
plot(1:20, 1:20, pch=1:20)

# Regresión lineal múltiple
lm.fit = lm(medv~lstat+age, data=Boston)
summary(lm.fit)
# medv~. para contemplar todos los predictores.
lm.fit = lm(medv~., data=Boston)
summary(lm.fit)
# medv~.-age usa todos menos la edad.
lm.fit = lm(medv~.-age, data=Boston)
summary(lm.fit)
