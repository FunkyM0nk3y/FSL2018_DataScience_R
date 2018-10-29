# Clasificación

# K-vecinos más cercanos.
setwd("/home/soyyo/projects/Modelos de datos/EjemplosR")
library(class)
library("ISLR")

dim(Caravan)
attach(Caravan)
summary(Purchase)
348/5474   # Relación de gente que compra versus los que no.
head(Caravan[86])   # Esta columna contiene los que compran y los que no, uno por renglón.  Es lo que queremos predecir con el modelo.
standardized.X=scale(Caravan[,-86])   # La columna 86 es la que quiero predecir.
var(Caravan[,1])
var(Caravan[,2])
var(standardized.X[,1])
var(standardized.X[,2])

test=1:1000
train.X=standardized.X[-test,]   # Generamos datos de entrenamiento excluyendo las líneas con índices dentro del vector test
test.X=standardized.X[test,]     # Generamos datos de entrenamiento 
train.Y=Purchase[-test]          # Generamos datos de prueba excluyendo las líneas con índices dentro del vector test
test.Y=Purchase[test]            # Generamos datos de prueba
set.seed(1)

# Modelo para 1 vecino.
knn.pred=knn(train.X,test.X,train.Y,k=1)   # Generamos el modelo de K-n vecinos.
exito_del_modelo <- mean(test.Y==knn.pred); exito_del_modelo   # Probabilidad de éxito del modelo
mean(test.Y!=knn.pred)   # Probabilidad de falla del modelo
mean(test.Y!="No")       # Datos de prueba conocidos de personas que sí compran.
modelo <- table(knn.pred,test.Y); modelo
(873+9)/(873+50+68+9)    # Exactitud del modelo.  Exac = (VN + VP) / (VN + FN + FP + VP)
exactitud <- (modelo[1,"No"]+modelo[2,"Yes"])/(modelo[1,"No"]+modelo[1,"Yes"]+modelo[2,"No"]+modelo[2,"Yes"]); exactitud
9/(68+9)                 # Sensitividad del modelo.  Sens = VP/(FP + VP)
sensitividad <- modelo[2,"Yes"]/(modelo[2,"No"] + modelo[2,"Yes"]); sensitividad
873/(873+50)             # Especificidad del modelo.  Espe = VN/(FN + VN)
especificidad <- modelo[1,"No"]/(modelo[1,"No"] + modelo[1,"Yes"]); especificidad
# Modelo para 3 vecinos.
knn.pred=knn(train.X,test.X,train.Y,k=3)
exito_del_modelo <- mean(test.Y==knn.pred); exito_del_modelo   # Probabilidad de éxito del modelo
modelo <- table(knn.pred,test.Y); modelo
5/26
exactitud <- (modelo[1,"No"]+modelo[2,"Yes"])/(modelo[1,"No"]+modelo[1,"Yes"]+modelo[2,"No"]+modelo[2,"Yes"]); exactitud
sensitividad <- modelo[2,"Yes"]/(modelo[2,"No"] + modelo[2,"Yes"]); sensitividad
especificidad <- modelo[1,"No"]/(modelo[1,"No"] + modelo[1,"Yes"]); especificidad
# Modelo para 5 vecinos.
knn.pred=knn(train.X,test.X,train.Y,k=5)
exito_del_modelo <- mean(test.Y==knn.pred); exito_del_modelo   # Probabilidad de éxito del modelo
modelo <- table(knn.pred,test.Y); modelo
4/15
exactitud <- (modelo[1,"No"]+modelo[2,"Yes"])/(modelo[1,"No"]+modelo[1,"Yes"]+modelo[2,"No"]+modelo[2,"Yes"]); exactitud
sensitividad <- modelo[2,"Yes"]/(modelo[2,"No"] + modelo[2,"Yes"]); sensitividad
especificidad <- modelo[1,"No"]/(modelo[1,"No"] + modelo[1,"Yes"]); especificidad
# Modelo para 7 vecinos.
knn.pred=knn(train.X,test.X,train.Y,k=7)
exito_del_modelo <- mean(test.Y==knn.pred); exito_del_modelo   # Probabilidad de éxito del modelo
modelo <- table(knn.pred,test.Y); modelo
exactitud <- (modelo[1,"No"]+modelo[2,"Yes"])/(modelo[1,"No"]+modelo[1,"Yes"]+modelo[2,"No"]+modelo[2,"Yes"]); exactitud
sensitividad <- modelo[2,"Yes"]/(modelo[2,"No"] + modelo[2,"Yes"]); sensitividad
especificidad <- modelo[1,"No"]/(modelo[1,"No"] + modelo[1,"Yes"]); especificidad

