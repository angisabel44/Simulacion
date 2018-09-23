library(lattice)
library(reshape2)
library(latticeExtra)

g <- function(x, y) {
  return(((x + 0.5)^4 - 30 * x^2 - 20 * x + (y + 0.5)^4 - 30 * y^2 - 20 * y)/100)
}

low <- -3
high <- -low
x <- seq(low, high, 0.05)
y <- x
z <- outer(x, y, g)
dimnames(z) <- list(x, y)
d <- melt(z)
names(d) <- c("x", "y", "z")

tmax <- 100

currx <- runif(1, low, high)
curry <- runif(1, low, high)
bestx <- currx
besty <- curry
step <- 0.3

for (tiempo in 1:tmax) {
  deltax <- c(-runif(1, 0, step), 0, runif(1, 0, step))
  deltay <- c(-runif(1, 0, step), 0, runif(1, 0, step))
  
  #codigo de la practica 4
  vecinosx <- numeric()
  vecinosy <- numeric()
  for (dx in deltax) {
    for (dy in deltay) {
      if (dx != 0 | dy != 0) { # descartar la posicion misma
        vecinosx <- c(vecinosx, currx + dx)
        vecinosy <- c(vecinosy, curry + dy)
      }
    }
  }
  
  gvecinos <- g(vecinosx, vecinosy)
  mejor <- max(gvecinos)
  elegido <- which(gvecinos %in% mejor)
  currx <- vecinosx[elegido]
  curry <- vecinosy[elegido]
  
  if (g(bestx, besty) < mejor) {
    bestx <- currx
    besty <- curry
  }
  
  salida <- paste("p7_flat", tiempo, ".png", sep = "")
  titulo <- paste("Paso", tiempo)
  plano <- levelplot(z ~ x * y, data = d, main = titulo)
  punto <- (xyplot(currx ~ curry, col = "black"))
  mejorpunto <- (xyplot(bestx ~ besty, col = "red"))
  graficapuntos <- plano + as.layer(punto) + as.layer(mejorpunto)
  png(salida, width = 500, height = 500)
  print(graficapuntos)
  graphics.off()
  
}