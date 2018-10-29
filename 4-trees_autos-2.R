# Árboles
# Árbol de clasificación.
setwd("/home/soyyo/projects/Modelos de datos/datosR")
rm(list=ls())

library(caret)
library(tree)
library(ISLR)

# Agregamos los datos y los analizamos rápido.
attach(Auto)
summary(Auto)
dim(Auto)
head(Auto)
Auto <- na.omit(Auto)
dim(Auto)
consumption <- ifelse(Auto$mpg > 22.75, "high", "low")
str(consumption)
Auto <- data.frame(Auto, consumption)
Auto <- Auto[, -1] # Quitamos la variable que queremos predecir para evitar bias.
Auto <- Auto[, -8] # Quitamos el nombre de los autos para eliminar ruido del modelo.
head(Auto)
autos.70 <- Auto[year < 81, ]
head(autos.70)
autos.80 <- Auto[year > 80, ]
head(autos.80)

# Generamos el modelo de árbol de clasificación.
tree.auto = tree(consumption~cylinders+displacement+horsepower+weight, autos.70)
plot(tree.auto)
text(tree.auto, pretty=0, cex=.70)
summary(tree.auto)

predict.consumption = predict(tree.auto, autos.80, type="class")
str(predict.consumption); str(autos.80$consumption)
predict.consumption; autos.80$consumption
table(predict.consumption, autos.80$consumption)
confusionMatrix(predict.consumption, autos.80$consumption)
prune.auto = prune.misclass(tree.auto, best=10); print("Árbol de decisión."); prune.auto
misclass.tree(tree.auto, detail=FALSE)
summary(tree.auto)

# Para que sean reproducibles nuestros experimentos es necesario documentar el ambiente en que ocurrieron.
sessionInfo()
