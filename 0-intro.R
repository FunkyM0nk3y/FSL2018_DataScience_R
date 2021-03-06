# Introducción a R
# Comandos básicos
setwd("/home/soyyo/projects/Modelos de datos/EjemplosR")
getwd()

x <- c(1,3,2,5)
x
x = c(1,6,2)
x
y = c(1,4,3)
length(x)
length(y)
x+y
ls()
rm(x,y)
ls()
rm(list=ls())

# Buscar ayuda
?matrix
??matrix
help(matrix)

x = matrix(data=c(1,2,3,4), nrow=2, ncol=2)
x
x = matrix(c(1,2,3,4), 2, 2)
matrix(c(1,2,3,4), 2, 2, byrow=TRUE)
sqrt(x)
x^2
x = rnorm(50)
x
y = x+rnorm(50, mean=50, sd=.1)
y
cor(x,y)
set.seed(1303)
rnorm(50)
set.seed(3)
y = rnorm(100)
mean(y)
var(y)
sqrt(var(y))
sd(y)
summary(y)

# Gráfica simple
x = rnorm(100)
y = rnorm(100)
plot(x, y)
plot(x, y, xlab="this is the x-axis", ylab="this is the y-axis", main="Plot of X vs Y")
pdf("Figure-20180110.pdf")
plot(x, y, col="green")
dev.off()

# Indexando datos
x = seq(1, 10)
x
x = 1:10
x
x[2]
A = matrix(1:16, 4, 4)
A
A[2, 3]
A[c(1,3), c(2,4)]
A[1:3, 2:4]
A[1:2,]
A[,1:2]
A[1,]
A[-c(1,3),]
A[-c(1,3), -c(1,3,4)]
dim(A)
summary(A)

# Cargando datos
Auto = read.table("Auto.data")
fix(Auto)
View(Auto)
Auto = read.table("Auto.data", header=T, na.strings="?")
fix(Auto)
View(Auto)
Auto = read.csv("Auto.csv", header=T, na.strings="?")
fix(Auto)
View(Auto)
dim(Auto)
Auto[1:4, ]
Auto = na.omit(Auto)
dim(Auto)
names(Auto)
Auto <- na.omit(Auto)
Auto$mpg

# Gráficas adicionales y resúmenes
# Este siguiente ejemplo genera un error dado que mpg no está definido en el ambiente.
plot(cylinders, mpg)
plot(Auto$cylinders, Auto$mpg)
attach(Auto)
boxplot(cylinders, mpg)
plot(cylinders, mpg)
cylinders = as.factor(cylinders)
plot(cylinders, mpg)
plot(cylinders, mpg, col="red")
plot(cylinders, mpg, col="red", varwidth=T)
plot(cylinders, mpg, col="red", varwidth=T,horizontal=T)
plot(cylinders, mpg, col="red", varwidth=T, xlab="cylinders", ylab="MPG")
hist(mpg)
hist(mpg, col=2)
hist(mpg, col=2, breaks=15)
pairs(Auto)
pairs(~ mpg + displacement + horsepower + weight + acceleration, Auto)
plot(horsepower, mpg)
plot(acceleration, mpg)
summary(Auto)
summary(mpg)
