# Salida a animación en HTML.
library(animation)
library(caret)
library(ggplot2)
library(lattice)
library(class)
library(ISLR)
library(MASS)

df=data.frame(c(-9,9),c(9,-9))

saveHTML({
  for (i in 1:50) {
    plot(df)
    df = rbind(df,c(rnorm(1),rnorm(1)))
  }
},
img.name = "plot",
imgdir = "unif_dir",
htmlfile = "test.html",
autobrowse = FALSE,
title = "Prueba de animación",
description = "Probando el paquete de animación por primera vez.")

# Para que sean reproducibles nuestros experimentos es necesario documentar el ambiente en que ocurrieron.
sessionInfo()
