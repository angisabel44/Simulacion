potencias <- c(5, 7, 9, 12, 15, 20)
muestras <- 2^(potencias)

calculando <- function(r){
  xs <- runif(muestra, min = -0.5, max = 0.5)
  ys <- runif(muestra, min = -0.5, max = 0.5)
  circle <- xs^2 + ys^2 <= 0.5^2
  return((sum(circle)/muestra)*4)
}

tabla <- data.frame()

library(parallel)
cluster <- makeCluster(detectCores() - 1)
clusterExport(cluster, "calculando")

for (muestra in muestras) {
  print(muestra)
  clusterExport(cluster, "muestra")
  for (i in 1:40) {
    numerospis <- parSapply(cluster, 1:500, calculando)
    piestimado <- sum(numerospis)/500
    
    digito <- 0
    for (i in 1:7) {
      valor1 <- signif(piestimado, digits = i)
      valor2 <- signif(pi, digits = i)
      if (valor1 == valor2) {
        digito <- i
      }
      else {
        break()
      }
    }
    
    tabla <- rbind(tabla, c(muestra, pi, piestimado, digito))
  }
}
stopCluster(cluster)
