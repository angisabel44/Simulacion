datos <- data.frame()

for (dimension in 1:8) {
  c <- 0
  probabilidad <- 0
  for (replicas in 1:30) {
    pos <- rep(0, dimension)
    for (pasos in 1:2**10) {
      cambiar <- sample(1:dimension, 1)
      cambio <- 1
      if (runif(1) < 0.5) {
        cambio <- -1
      }
      pos[cambiar] <- pos[cambiar] + cambio
      if (all(pos==0)) {
        c <- c + 1
        break()
      }
    }
  }
  probabilidad <- c/30
  datos <- rbind(datos, probabilidad)
}
png("probabilidad.png")
plot(data.matrix(datos), xlab = "Dimensiones", ylab = "Probabilidad", main = "2^10")
print(datos)