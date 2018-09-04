propaga <- function(replica, z, s) {
  # probabilidad de propagacion interna
  prob <- 1
  dificil <- 0.99
  grieta <- voronoi # marcamos la grieta en una copia
  i <- inicio() # posicion inicial al azar
  xg <- i[1]
  yg <- i[2]
  largo <- 0
  distmanh <- 0
    while (TRUE) { # hasta que la propagacion termine
      grieta[yg, xg] <- 0 # usamos el cero para marcar la grieta
      largo <-  largo + 1
      frontera <- numeric()
      interior <- numeric()
      for (v in 1:vc) {
        vecino <- vp[v,]
        xs <- xg + vecino$dx # columna del vecino potencial
        ys <- yg + vecino$dy # fila del vecino potencial
        if (xs > 0 & xs <= n & ys > 0 & ys <= n) { # no sale de la zona
          if (grieta[ys, xs] > 0) { # aun no hay grieta ahi
            if (voronoi[yg, xg] == voronoi[ys, xs]) {
              interior <- c(interior, v)
            } else { # frontera
              frontera <- c(frontera, v)
            }
          }
        }
      }
      elegido <- 0
      if (length(frontera) > 0) { # siempre tomamos frontera cuando haya
        if (length(frontera) > 1) {
          elegido <- sample(frontera, 1)
        } else {
          elegido <- frontera # sample sirve con un solo elemento
        }
        prob <- 1 # estamos nuevamente en la frontera
      } else if (length(interior) > 0) { # no hubo frontera para propagar
        if (runif(1) < prob) { # intentamos en el interior
          if (length(interior) > 1) {
            elegido <- sample(interior, 1)
          } else {
            elegido <- interior
          }
          prob <- dificil * prob # mas dificil a la siguiente
        }
      }
      if (elegido > 0) { # si se va a propagar
        vecino <- vp[elegido,]
        xg <- xg + vecino$dx
        yg <- yg + vecino$dy
        dis <- abs(i[1] - xg) + abs(i[2] - yg)
        if (distmanh < dis) {
          distmanh <- dis
        }
      } else {
        break # ya no se propaga
      }
    }
  dato <- c(z, s, replica, largo, distmanh)
  return(dato)
}