# Listar los paquetes instalados en R
packinfo <- installed.packages ()
packinfo

# Split the screen using the layout function
layout(matrix(c(1,1,2,2), 2, 2, byrow=T), heights=c(4,1))
plot(rnorm(10),ylab="", xlab="Usar el argumento xlab", main="Usar un título", ty="o", pch=19)
mtext("Usar la función mtext()")
temp <- locator(1) # En la gráfica, clickear dónde se quiera el texto.
text(temp,"Usar la función text() con o sin la función locator()")

# agregar texto usando la función Corner_text:
Corner_text <- function(text, location="topright"){
  legend(location,legend=text, bty ="n", pch=NA) 
}
Corner_text(text="Usar la función Corner_text()")
Corner_text(text="Usar la función Corner_text() con ubicación", location="bottomright")
title(sub="Agregar un subtítulo usando la función title()")

library(gplots)
par(mar=c(1,1,1,1))
#layout.show(2) # Muestra una división entre las pantallas superior e inferior.
temptext1 <- "Agregemos algunas notas aquí.
El plot de arriba representa 10 puntos aleatorios para una distribución normal.
El plot está generado para visualizar las distintas opciones para agregar texto.

Se puede agregar legibilidad saltando líneas."
temptext2 <- "Se puede agregar más texto utilizando de nuevo la función locator(1)+text()" 
textplot(temptext1, valign="center", cex=0.9, halign="left", mar=c(0, 0, 0, 0), col=2) 
# mar=c(0,0,0,0) # Remueve los márgenes.
temp <- locator(1)
text(temp, temptext2,col=4)

