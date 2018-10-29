# Árboles de decisión
# J48
setwd("/home/soyyo/projects/Modelos de datos/datosR")
rm(list=ls())

library(RWeka)
library(party)
library(partykit)
library(FSelector)

# Revisamos el data set y generemos el árbol.
str(iris)
tree.iris <- J48(Species~., data=iris)
if(require("party", quietly=TRUE)) plot(tree.iris, tp_args=list(text="vertical", ymax=1.5))
summary(tree.iris)

# Analizamos el peso de las variables en el árbol resultante.
information.gain(Species~., data=iris)

subset1.iris <- subset(iris, Petal.Width > 0.6)
information.gain(Species~., data=subset1.iris)

subset2.iris <- subset(iris, Petal.Width <= 1.7)
information.gain(Species~., data=subset2.iris)

# Para que sean reproducibles nuestros experimentos es necesario documentar el ambiente en que ocurrieron.
sessionInfo()
