celda <-  function(pos) {
  fila <- floor((pos - 1) / n) + 1
  columna <- ((pos - 1) %% n) + 1
  if (zona[fila, columna] > 0) { # es una semilla
    return(zona[fila, columna])
  } else {
    cercano <- NULL # sin valor por el momento
    menor <- n * sqrt(2) # mayor posible para comenzar la busqueda
    for (semilla in 1:k) {
      dx <- columna - x[semilla]
      dy <- fila - y[semilla]
      dist <- sqrt(dx^2 + dy^2)
      if (dist < menor) {
        cercano <- semilla
        menor <- dist
      }
    }
    return(cercano)
  }
}