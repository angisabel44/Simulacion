maximizar <- function(r) {
  coordenadas <- data.frame(x = double(), y = double(), bestgxy = double())
  for (n in 1:puntos) {
    currx <- runif(1, low, high)
    curry <- runif(1, low, high)
    coordenadas <- rbind(coordenadas, data.frame(x = currx, y = curry, bestgxy = g(currx, curry)))
  }
  
  contador <- 1
  dispercionbest <- var(coordenadas$bestgxy)
  
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
    
    gvecinosbest <- g(coordenadas$x, coordenadas$y)
    mejores <- coordenadas$bestgxy < gvecinosbest
    
    for (i in 1:puntos) {
      if(mejores[i]){
        coordenadas$bestgxy[i] <- gvecinosbest[i]
      }
    }
    
    dispercion <- var(coordenadas$bestgxy)
                
    if (dispercion < dispercionbest) {
      dispercionbest <- dispercion
      contador <- tiempo
    }
  }
  dato <- c(dispercionbest, contador)
  return(dato)
}
