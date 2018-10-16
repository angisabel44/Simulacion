library(testit)
source("funciones.R")

#Cantidad de objetos
n <- 50

#Generadores
pesos <- generador.pesos(n, 15, 80)
valores <- generador.valores(pesos, 10, 500)
capacidad <- round(sum(pesos) * 0.65)

#Optimo
optimo <- knapsack(capacidad, pesos, valores)

#Tamaño de la poblacion
init <- 200

p <- poblacion.inicial(n, init)
tam <- dim(p)[1]

assert(tam == init)

pm <- 0.05 #probabilidad de mutacion
rep <- 50 #cantidad de reproducciones
tmax <- 50

mejores <- double()

for (iter in 1:tmax) {
  p$obj <- NULL
  p$fact <- NULL
  
  for (i in 1:tam) { # cada individuo puede mutarse con probabilidad pm
    if (runif(1) < pm) {
      p <- rbind(p, mutacion(p[i,], n))
    }
  }
  
  for (i in 1:rep) { # una cantidad fija de reproducciones
    padres <- sample(1:tam, 2, replace = FALSE)
    hijos <- reproduccion(p[padres[1],], p[padres[2],], n)
    p <- rbind(p, hijos[1:n]) # primer hijo
    p <- rbind(p, hijos[(n+1):(2*n)]) # segundo hijo
  }
  
  tam <- dim(p)[1]
  obj <- double()
  fact <- integer()
  
  for (i in 1:tam) {
    obj <- c(obj, objetivo(p[i,], valores))
    fact <- c(fact, factible(p[i,], pesos, capacidad))
  }
  
  p <- cbind(p, obj)
  p <- cbind(p, fact)
  mantener <- order(-p[, (n + 2)], -p[, (n + 1)])[1:init]
  p <- p[mantener,]
  tam <- dim(p)[1]
  
  assert(tam == init)
  
  factibles <- p[p$fact == TRUE,]
  mejor <- max(factibles$obj)
  mejores <- c(mejores, mejor)
}
