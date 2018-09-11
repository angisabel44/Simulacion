f <- function(x) {return(1 / (exp(x) + exp(-x)))}
g <- function(x) {return((2/pi) * f(x))}

suppressMessages(library(distr))
generador <- r(AbscontDistribution(d = g))

parte <- function(lol) {
  valores <- generador(pedazo)
  return(sum(valores >= desde & valores <= hasta))
}

desde <- 3
hasta <- 7
cuantos <- 500

wolfram <- 0.048834
muestras <- 2^(5:8)

library(parallel)

cluster <- makeCluster(detectCores())
clusterExport(cluster, "generador")
clusterExport(cluster, "parte")
clusterExport(cluster, "desde")
clusterExport(cluster, "hasta")
clusterExport(cluster, "cuantos")

tabla1 <- data.frame()
resultados <- vector()

for (pedazo in muestras) {
  print(pedazo)
  clusterExport(cluster, "pedazo")
  for (r in 1:40) {
    montecarlo <- parSapply(cluster, 1:cuantos, parte)
    
    integral <- sum(montecarlo) / (cuantos * pedazo)
    resultado <- (pi / 2) * integral
    
    tabla1 <- rbind(tabla1, c(pedazo, r, wolfram, resultado))
    resultados <- c(resultados, resultado)
  }
}

digitos <- function(num) {
  dato <- 0
  for (i in 1:6) {
    valor1 <- signif(num, digits = i)
    valor2 <- signif(wolfram, digits = i)
    if (valor1 == valor2) {
      dato <- i + 1
    }
    else {
      break()
    }
  }
  return(dato)
}

clusterExport(cluster, "digitos")
clusterExport(cluster, "wolfram")
datos <- parSapply(cluster, resultados, digitos)

stopCluster(cluster)

tabla1 <- cbind(tabla1, datos)
