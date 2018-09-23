library(lattice)
library(reshape2)
library(latticeExtra)

g <- function(x, y) {
  return(((x + 0.5)^4 - 30 * x^2 - 20 * x + (y + 0.5)^4 - 30 * y^2 - 20 * y)/100)
}

low <- -3
high <- -low
step <- 0.3

x <- seq(low, high, 0.05)
y <- x
z <- outer(x, y, g)
dimnames(z) <- list(x, y)
d <- melt(z)
names(d) <- c("x", "y", "z")

tmax <- 30
puntos <- 15

coordenadas <- data.frame(x = double(), y = double(), bestgxy = double())
for (n in 1:puntos) {
  currx <- runif(1, low, high)
  curry <- runif(1, low, high)
  coordenadas <- rbind(coordenadas, data.frame(x = currx, y = curry, bestgxy = g(currx, curry)))
}

salida <- paste("p7_flat0.png")
titulo <- paste("Inicio")
plano <- levelplot(z ~ x * y, data = d, main = titulo)
puntoss <- (xyplot(coordenadas$x ~ coordenadas$y, col = "black"))
graficapuntos <- plano + as.layer(puntoss)
png(salida, width = 500, height = 500)
print(graficapuntos)
graphics.off()

for (tiempo in 1:tmax) {
  tablax <- double()
  tablay <- double()
  
  for (n in 1:puntos) {
    delta <- runif(1, 0, step)
    deltax <- c(-delta, 0, delta)
    delta <- runif(1, 0, step)
    deltay <- c(-delta, 0, delta)
    
    #codigo de la practica 4
    vecinosx <- numeric()
    vecinosy <- numeric()
    for (dx in deltax) {
      for (dy in deltay) {
        if (dx != 0 | dy != 0) { # descartar la posicion misma
          vecinosx <- c(vecinosx, dx)
          vecinosy <- c(vecinosy, dy)
        }
      }
    }
    
    tablax <- rbind(tablax, vecinosx + coordenadas$x[n])
    tablay <- rbind(tablay, vecinosy + coordenadas$y[n])
  }
  
  gvecinos <- g(tablax, tablay)
  cambios <- max.col(gvecinos)
  
  for (i in 1:puntos) {
    coordenadas$x[i] <- tablax[i,cambios[i]]
    coordenadas$y[i] <- tablay[i,cambios[i]]
  }
  
  salida <- paste("p7_flat", tiempo, ".png", sep = "")
  titulo <- paste("Paso", tiempo)
  plano <- levelplot(z ~ x * y, data = d, main = titulo)
  puntoss <- (xyplot(coordenadas$x ~ coordenadas$y, col = "black"))
  graficapuntos <- plano + as.layer(puntoss)
  png(salida, width = 500, height = 500)
  print(graficapuntos)
  graphics.off()
  
}
