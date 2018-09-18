multiagente <- function(replica) {
  agentes <- data.frame(x = double(), y = double(), dx = double(), dy = double(), estado  = character())
  for (i in 1:n) {
    e <- "S"
    if (runif(1) < pv) {
      e <- "R"
    } else {
      if (runif(1) < pi) {
        e <- "I"
      }
    }
    agentes <- rbind(agentes, data.frame(x = runif(1, 0, l), y = runif(1, 0, l),
                                         dx = runif(1, -v, v), dy = runif(1, -v, v),
                                         estado = e))
  }
  
  levels(agentes$estado) <- c(levels(agentes$estado), "R")
  epidemia <- numeric()
  
  for (tiempo in 1:tmax) {
    infectados <- dim(agentes[agentes$estado == "I",])[1]
    porcentage <- (infectados * 100) /n
    epidemia <-  c(epidemia, porcentage)
    if (infectados == 0) {
      break
    }
    contagios <- rep(FALSE, n)
    suceptibles <- dim(agentes[agentes$estado == "S",])[1]
    if (suceptibles != 0) {
      posibles <- as.numeric(row.names(agentes[which(agentes$estado == "I"),])) #solo los infectados
      for (i in posibles) { # propagar sobre los infectados
        a1 <- agentes[i, ]
        candidatos <- agentes[which(contagios %in% F),] # solo propagar con los FALSE
        elegidos <- as.numeric(row.names(candidatos[which(candidatos$estado == "S"),])) #solo los suceptibles
        for (j in elegidos) {
          a2 <- agentes[j, ]
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
  return(max(epidemia))
}
