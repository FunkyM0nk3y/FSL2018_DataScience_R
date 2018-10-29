# Métodos de remuestreo
# Método del conjunto de validación
setwd("/home/soyyo/projects/Modelos de datos/datosR")
rm(list=ls())

library(ISLR)

set.seed(1)
train = sample(392, 196)
lm.fit = lm(mpg~horsepower, data=Auto, subset=train)

attach(Auto)
mean((mpg-predict(lm.fit, Auto))[-train]^2)
lm.fit2 = lm(mpg~poly(horsepower, 2), data=Auto, subset=train)
mean((mpg-predict(lm.fit2, Auto))[-train]^2)
lm.fit3 = lm(mpg~poly(horsepower, 3), data=Auto, subset=train)
mean((mpg-predict(lm.fit3, Auto))[-train]^2)

set.seed(2)
train = sample(392, 196)
lm.fit = lm(mpg~horsepower, subset=train)
mean((mpg-predict(lm.fit, Auto))[-train]^2)
lm.fit2 = lm(mpg~poly(horsepower, 2), data=Auto, subset=train)
mean((mpg-predict(lm.fit2, Auto))[-train]^2)
lm.fit3 = lm(mpg~poly(horsepower, 3), data=Auto, subset=train)
mean((mpg-predict(lm.fit3, Auto))[-train]^2)

# Validación cruzada dejando uno afuera
glm.fit = glm(mpg~horsepower, data=Auto)
coef(glm.fit)
lm.fit = lm(mpg~horsepower, data=Auto)
coef(lm.fit)

library(boot)

glm.fit =glm(mpg~horsepower, data=Auto)
cv.err = cv.glm(Auto, glm.fit)
cv.err$delta
cv.error = rep(0, 5)
for (i in 1:5){
  glm.fit = glm(mpg~poly(horsepower, i), data=Auto)
  cv.error[i] = cv.glm(Auto, glm.fit)$delta[1]
}
cv.error   # Validación dejando uno afuera.

# Validación cruzada de K-dobleces
set.seed(17)
cv.error.10 = rep(0, 10)
for (i in 1:10){
  glm.fit = glm(mpg~poly(horsepower, i), data=Auto)
  cv.error.10[i] = cv.glm(Auto, glm.fit, K=10)$delta[1]
}
cv.error.10

# Bootstrap
#sample(100, 100, replace=T)
alpha.fn = function(data, index){
 X = data$X[index]
 Y = data$Y[index]
 return((var(Y)-cov(X, Y))/(var(X)+var(Y)-2*cov(X, Y)))
 }
alpha.fn(Portfolio, 1:100)
set.seed(1)
alpha.fn(Portfolio, sample(100, 100, replace=T))
boot(Portfolio, alpha.fn, R=1000)

# Para que sean reproducibles nuestros experimentos es necesario documentar el ambiente en que ocurrieron.
sessionInfo()
