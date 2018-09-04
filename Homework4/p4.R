source("inicio.R")
source("celda.R")
source("propaga.R")

tabla <- numeric()

zonas <- c(50, 100, 200, 300, 500)
semillas <- c(0.25, 0.5, 0.75)

for (n in zonas) {
  print(paste("Estamos con la zona igual a ", n))
  for (k in semillas) {
    print(paste("Con el numero de semillas igual a ", k))
    k <- ceiling(k * n)
    zona <- matrix(rep(0, n * n), nrow = n, ncol = n)
    x <- rep(0, k)
    y <- rep(0, k)
    for (semilla in 1:k) {
      while (TRUE) { # hasta que hallamos una posicion vacia para la semilla
        fila <- sample(1:n, 1)
        columna <- sample(1:n, 1)
        if (zona[fila, columna] == 0) {
          zona[fila, columna] = semilla
          x[semilla] <- columna
          y[semilla] <- fila
          break
        }
      }
    }
    
    suppressMessages(library(doParallel))
    registerDoParallel(makeCluster(detectCores()))
    celdas <- foreach(p = 1:(n * n), .combine=c) %dopar% celda(p)
    stopImplicitCluster()
    voronoi <- matrix(celdas, nrow = n, ncol = n, byrow=TRUE)
    
    vp <- data.frame(numeric(), numeric()) # posiciones de posibles vecinos
    for (dx in -1:1) {
      for (dy in -1:1) {
        if (dx != 0 | dy != 0) { # descartar la posicion misma
          vp <- rbind(vp, c(dx, dy))
        }
      }
    }
    names(vp) <- c("dx", "dy")
    vc <- dim(vp)[1]
    
    suppressMessages(library(doParallel))
    registerDoParallel(makeCluster(detectCores()))
    largos <- foreach(r = 1:200, .combine=c) %dopar% propaga(r, n, k)
    stopImplicitCluster()
    
    tabla <- c(tabla, largos)
  }
}

tablafinal <- matrix(tabla, nrow = 3000, ncol = 5, byrow = TRUE)
tablafinal <- as.data.frame(tablafinal)
names(tablafinal) <- c("Zona", "Semilla", "Replica", "Pixeles", "Manhatan")

colores <- c("yellow3","yellow4","yellowgreen","olivedrab4","green4")

png("manhatan.png")
boxplot(tablafinal$Manhatan ~tablafinal$Zona, ylab = "Grieta", xlab = "Zonas", main = "Manhatan", col=colores)
graphics.off()

png("pixeles.png")
boxplot(tablafinal$Pixeles ~tablafinal$Zona, ylab = "Grieta", xlab = "Zonas", main = "Area", col=colores)
graphics.off()

lmtsa <-  range(tablafinal$Pixeles)
lmtsm <- range(tablafinal$Manhatan) 

for (i in zonas) {
  png(paste("area", i, ".png"))
  boxplot(tablafinal$Pixeles[which(tablafinal$Zona == i)] ~tablafinal$Semilla[which(tablafinal$Zona == i)],
          ylab = "Grieta", xlab = "Semillas", col=colores, ylim = lmtsa, main = paste("Zona" , i))
  graphics.off()
  
  png(paste("manhatan", i, ".png"))
  boxplot(tablafinal$Manhatan[which(tablafinal$Zona == i)] ~tablafinal$Semilla[which(tablafinal$Zona == i)],
          ylab = "Grieta", xlab = "Semillas", col=colores, ylim = lmtsm, main = paste("Zona" , i))
  graphics.off()
}
