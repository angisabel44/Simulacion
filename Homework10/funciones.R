knapsack <- function(cap, peso, valor) {
  inicio <- as.numeric(Sys.time())
  n <- length(peso)
  pt <- sum(peso) # deben ser enteros en este caso
  
  assert(n == length(valor))
  
  vt <- sum(valor) # pueden ser lo que sea
  
  if (pt < cap) { # cabe todo
    final <- as.numeric(Sys.time())
    return(c(final - inicio, vt))
  } else {
    filas <- cap + 1 # una para cada posible peso acumulado desde cero hasta cap
    cols <- n + 1 # una para cada objeto y una extra al inicio para no llevar nada
    
    tabla <- matrix(rep(-Inf, filas * cols),
                    nrow = filas, ncol = cols) # al inicio todo vale negativo infinito
    
    for (fila in 1:filas) {
      tabla[fila, 1] <- 0 # todas las filas tienen un cero al inicio (no llevar nada da cero valor)
    }
    
    rownames(tabla) <- 0:cap # filas corresponden a pesos acumulados posibles
    colnames(tabla) <- c(0, valor) # columnas corresponden a objetos considerados
    
    for (objeto in 1:n) { # consideremos a cada objeto por turno
      p <- peso[objeto] # tomamos su peso a una variable
      v <- valor[objeto] # y su valor a otra variable
      
      for (acum in 1:(cap+1)) { # consideramos cada fila de la tabla
        anterior <- acum - p
        tabla[acum, objeto + 1] <- tabla[acum, objeto]
        
        if (anterior > 0) { # si conocemos una combinacion con ese peso
          tabla[acum, objeto + 1] <- max(tabla[acum, objeto], tabla[anterior, objeto] + v)
        } 
      }
      
    }
    final <- as.numeric(Sys.time())
    return(c(final - inicio, max(tabla)))
  }
}

factible <- function(seleccion, pesos, capacidad) {
  return(sum(seleccion * pesos) <= capacidad)
}

objetivo <- function(seleccion, valores) {
  return(sum(seleccion * valores))
}

normalizar <- function(data) {
  menor <- min(data)
  mayor <- max(data)
  rango <- mayor - menor
  data <- data - menor # > 0
  return(data / rango) # entre 0 y 1
}

generador.pesos <- function(cuantos, min, max) {
  return(sort(round(normalizar(rnorm(cuantos)) * (max - min) + min)))
}

generador.valores <- function(pesos, min, max) {
  n <- length(pesos)
  valores <- double()
  for (i in 1:n) {
    media <- pesos[n]
    desv <- runif(1)
    valores <- c(valores, rnorm(1, media, desv))
  }
  valores <- normalizar(valores) * (max - min) + min
  return(valores)
}

poblacion.inicial <- function(n, tam) {
  pobl <- matrix(rep(FALSE, tam * n), nrow = tam, ncol = n)
  for (i in 1:tam) {
    pobl[i,] <- round(runif(n))
  }
  return(as.data.frame(pobl))
}

mutacion <- function(sol, n) {
  pos <- sample(1:n, 1)
  mut <- sol
  mut[pos] <- (!sol[pos]) * 1
  return(mut)
}

reproduccion <- function(x, y, n) {
  pos <- sample(2:(n-1), 1)
  xy <- c(x[1:pos], y[(pos+1):n])
  yx <- c(y[1:pos], x[(pos+1):n])
  return(c(xy, yx))
}
