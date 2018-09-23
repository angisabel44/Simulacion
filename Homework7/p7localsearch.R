source("maximizar.R")
g <- function(x, y) {
  return(((x + 0.5)^4 - 30 * x^2 - 20 * x + (y + 0.5)^4 - 30 * y^2 - 20 * y)/100)
}

resultados <- data.frame()

low <- -3
high <- -low
step <- 0.3

tiempos <- c(seq(10, 20, 2), 23, 26, 30)
cantidadpuntos <- c(15, 20, 30, 50)

replicas <- 30

suppressMessages(library(parallel))
cluster <- makeCluster(detectCores())
clusterExport(cluster, "low")
clusterExport(cluster, "high")
clusterExport(cluster, "step")
clusterExport(cluster, "g")
clusterExport(cluster, "maximizar")

for (puntos in cantidadpuntos) {
clusterExport(cluster, "puntos")
  for (tmax in tiempos) {
    clusterExport(cluster, "tmax")
    resultado <- parSapply(cluster, 1:replicas, maximizar)
    iteraciones <- t(resultado)
    datos <- cbind(rep(puntos, replicas), rep(tmax, replicas), 1:replicas, iteraciones)
    resultados <- rbind(resultados, datos)
  }
}
stopCluster(cluster)

names(resultados) <- c("Cantidad de puntos", "Tiempo", "Punto", "Iteracion", "Valor")
resultados$`Cantidad de puntos` <- as.factor(resultados$`Cantidad de puntos`)
resultados$Tiempo <- as.factor(resultados$Tiempo)
resultados$Punto <- as.factor(resultados$Punto)

suppressMessages(library(ggplot2))
ggplot(resultados, aes(Tiempo, Iteracion)) + geom_boxplot() + facet_wrap( ~`Cantidad de puntos`, nrow = 3)
ggplot(resultados, aes(`Cantidad de puntos`, Iteracion)) + geom_boxplot() + facet_wrap( ~Tiempo, nrow = 3)
