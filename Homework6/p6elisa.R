l <- 1.5
n <- 50
pr <- 0.02
v <- l / 30
r <- 0.1
tmax <- 100

timeelisa <- numeric()

probabilidades <- seq(0.1, .9, .1)

for (pi in probabilidades) {
  #print(pi)
  timeinitial <- Sys.time()
  for (replica in 1:40) {
    agentes <- data.frame(x = double(), y = double(), dx = double(), dy = double(), estado  = character())
    for (i in 1:n) {
      e <- "S"
      if (runif(1) < pi) {
        e <- "I"
      }
      agentes <- rbind(agentes, data.frame(x = runif(1, 0, l), y = runif(1, 0, l),
                                           dx = runif(1, -v, v), dy = runif(1, -v, v),
                                           estado = e))
    }
    
    if (agentes$estado[1] == "S"){
      levels(agentes$estado) <- c("S", "I", "R")
    } else {
      levels(agentes$estado) <- c("I", "S", "R")
    }
    
    for (tiempo in 1:tmax) {
      infectados <- dim(agentes[agentes$estado == "I",])[1]
      #porcentage <- (infectados*100)/n
      #resultadoselisa <- rbind(resultadoselisa, c(pi, replica, tiempo, infectados, porcentage))
      if (infectados == 0) {
        break
      }
      contagios <- rep(FALSE, n)
      for (i in 1:n) { # posibles contagios
        a1 <- agentes[i, ]
        if (a1$estado == "I") { # desde los infectados
          for (j in 1:n) {
            if (!contagios[j]) { # aun sin contagio
              a2 <- agentes[j, ]
              if (a2$estado == "S") { # hacia los susceptibles
                dx <- a1$x - a2$x
                dy <- a1$y - a2$y
                d <- sqrt(dx^2 + dy^2)
                if (d < r) { # umbral
                  p <- (r - d) / r
                  if (runif(1) < p) {
                    contagios[j] <- TRUE
                  }
                }
              }
            }
          }
        }
      }
      for (i in 1:n) { # movimientos y actualizaciones
        a <- agentes[i, ]
        if (contagios[i]) {
          a$estado <- "I"
        } else if (a$estado == "I") { # ya estaba infectado
          if (runif(1) < pr) {
            a$estado <- "R" # recupera
          }
        }
        a$x <- a$x + a$dx
        a$y <- a$y + a$dy
        if (a$x > l) {
          a$x <- a$x - l
        }
        if (a$y > l) {
          a$y <- a$y - l
        }
        if (a$x < 0) {
          a$x <- a$x + l
        }
        if (a$y < 0) {
          a$y <- a$y + l
        }
        agentes[i, ] <- a
      }
    }
  }
  timeelisa <- c(timeelisa, Sys.time() - timeinitial)
}

names(resultadoselisa) <- c("Probabilidad", "Replica", "Tiempo", "NumInfectados", "PorInfectados")
resultadoselisa$Probabilidad <- as.factor(resultadoselisa$Probabilidad)
resultadoselisa$Replica <- as.factor(resultadoselisa$Replica)
resultadoselisa$Tiempo <- as.factor(resultadoselisa$Tiempo)
resultadoselisa$NumInfectados <- as.numeric(resultadoselisa$NumInfectados)
resultadoselisa$PorInfectados <- as.numeric(resultadoselisa$PorInfectados)

#Data%maximos
porcientomaximoelisa <- data.frame()
avance <- 0
for (p in probabilidades) {
  for (i in 1:40) {
    bloque <- resultadoselisa$PorInfectados[which(resultadoselisa$Probabilidad == p & resultadoselisa$Replica == i)]
    maximo <- max(bloque)
    porcientomaximoelisa <- rbind(porcientomaximoelisa, resultadoselisa[which(bloque %in% maximo)[1] + avance,])
    avance <- avance + 100
  }
}

ggplot(porcientomaximoelisa, aes(Probabilidad, PorInfectados)) + geom_boxplot()
