rotura <- function(x) {
  return (1 / (1 + exp((c - x) / d)))
}
union <- function(x) {
  return (exp(-x / c))
}
romperse <- function(tam, cuantos) {
  romper <- round(rotura(tam) * cuantos) # independientes
  resultado <- rep(tam, cuantos - romper) # los demas
  if (romper > 0) {
    for (cumulo in 1:romper) { # agregar las rotas
      t <- 1
      if (tam > 2) { # sample no jala con un solo valor
        t <- sample(1:(tam-1), 1)
      }
      resultado <- c(resultado, t, tam - t)
    }
  }
  #assert(sum(resultado) == tam * cuantos) # no hubo perdidas
  return(resultado)
}
unirse <- function(tam, cuantos) {
  unir <- round(union(tam) * cuantos) # independientes
  if (unir > 0) {
    division <- c(rep(-tam, unir), rep(tam, cuantos - unir))
    #assert(sum(abs(division)) == tam * cuantos)
    return(division)
  } else {
    return(rep(tam, cuantos))
  }
}

faserotura <- function(i) {
  urna <- freq[i,]
  if (urna$tam > 1) { # no tiene caso romper si no se puede
    return(romperse(urna$tam, urna$num))
  } else {
    return(rep(1, urna$num))
  }
}

faseunion <- function(i) {
  urna <- freq[i,]
  return(unirse(urna$tam, urna$num))
}

fasejuntarse <- function(i) {
  return(juntarse[2*i-1] + juntarse[2*i])
}
