# Selección
# Árboles
setwd("/home/soyyo/projects/Modelos de datos/datosR")
rm(list=ls())

library(tree)
library(ISLR)

# Agregamos los datos y los análisamos rápido.
attach(Auto)
summary(Auto)
dim(Auto)
head(Auto)
autos.70 <- Auto[year<81, ]; dim(autos.70)
autos.80 <- Auto[year>80, ]; dim(autos.80)

tree.autos = tree(mpg~cylinders+displacement+horsepower+weight, autos.70)
plot(tree.autos)
text(tree.autos, pretty=0, cex=.70)
summary(tree.autos)

predict.auto = predict(tree.autos, newdata=autos.80)
dim(predict.auto) # El tipo de objeto devuelto no tiene la propiedad de dimensiones.
predict.auto
plot(predict.auto, autos.80$mpg)
abline(0, 1, col="red")

# Para que sean reproducibles nuestros experimentos es necesario documentar el ambiente en que ocurrieron.
sessionInfo()
