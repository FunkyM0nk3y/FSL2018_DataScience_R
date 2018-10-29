# 
# Lasso
setwd("/home/soyyo/projects/Modelos de datos/datosR")
rm(list=ls())

library(ISRL)
library(glmnet)

x = as.matrix(Carseats[, c(4, 5, 6, 8)])
y = as.vector(Carseats[, 1])
View(x)
y

lasso.mod = glmnet(x, y, alpha=1)
coef(lasso.mod)[, 10]
plot(lasso.mod, label=TRUE)
plot(lasso.mod, xvar="lambda", label=TRUE)

# Para que sean reproducibles nuestros experimentos es necesario documentar el ambiente en que ocurrieron.
sessionInfo()
