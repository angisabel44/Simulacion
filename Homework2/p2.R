library(parallel)

resultados <- data.frame()
vecino <- vector()
probabilidad <- vector()
replica <- vector()
dato <- vector()

dim <- 15
num <- dim^2

t <- 0

replicas <- 50
itermax <- 50

suppressMessages(library("sna"))

paso <- function(pos) {
  fila <- floor((pos - 1) / dim) + 1
  columna <- ((pos - 1) %% dim) + 1
  vecindad <- actual[max(fila - 1, 1) : min(fila + 1, dim),
                     max(columna - 1, 1) : min(columna + 1, dim)]
  return(1 * ((sum(vecindad) - actual[fila, columna]) == v))
}

cluster <- makeCluster(detectCores())
clusterExport(cluster, "dim")
clusterExport(cluster, "paso")

for (v in 2:4) {
  vecino <- c(vecino, rep(v, 10*replicas))
  for (i in 1:10) {
    probabilidad <- c(probabilidad, rep(i/10, replicas))
    prob <- i/10
    for (iteracion in 1:replicas) {
      replica <- c(replica, iteracion)
      actual <- matrix(1 * (runif(num) < 0.3), nrow = dim, ncol = dim)
      n <- 0
      while (sum(actual) != 0 & (n < itermax)) {
        clusterExport(cluster, "actual")
        clusterExport(cluster, "v")
        siguiente <- parSapply(cluster, 1:num, paso)
        if (sum(siguiente) == 0) {
          break;
        }
        actual <- matrix(siguiente, nrow = dim, ncol = dim, byrow = TRUE)
        n <- n + 1
      }
      dato <- c(dato, n)
      t <- t + 1
      print(t)
    }
  }
}

resultados <- cbind(vecino, probabilidad, replica, dato)
resultados <- as.data.frame(resultados)

for (v in 2:4) {
  salida <- paste("p2_v", v, ".png", sep = " ")
  numvecino <- paste("Vecinos", v)
  png(salida)
  boxplot(resultados$dato[which(resultados$vecino == v)] ~resultados$probabilidad[which(resultados$vecino == v)], 
          xlab = "Probabilidad", ylab = "Muertes", main = numvecino)
  graphics.off()
}

png("inicio.png")
plot.sociomatrix(actual, diaglab = FALSE, main = "Ejemplo.")
graphics.off()