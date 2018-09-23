source("maximizar.R")

g <- function(x, y) {
  return(((x + 0.5)^4 - 30 * x^2 - 20 * x + (y + 0.5)^4 - 30 * y^2 - 20 * y)/100)
}

resultados <- data.frame()

low <- -3
high <- -low
puntos <- 15
wolfram <- 0.0666822

maxsteps <- 0.3^(5:1)
tiempos <- 2^(5:11)

replicas <- 30

suppressMessages(library(parallel))
cluster <- makeCluster(detectCores())
clusterExport(cluster, "low")
clusterExport(cluster, "high")
clusterExport(cluster, "puntos")
clusterExport(cluster, "wolfram")
clusterExport(cluster, "g")
clusterExport(cluster, "maximizar")

for (step in maxsteps) {
  clusterExport(cluster, "step")
  for (tmax in tiempos) {
    clusterExport(cluster, "tmax")
    resultado <- parSapply(cluster, 1:replicas, maximizar)
    iteraciones <- t(resultado)
    datos <- cbind(rep(step, replicas), rep(tmax, replicas), 1:replicas, iteraciones)
    resultados <- rbind(resultados, datos)
  }
}
stopCluster(cluster)

names(resultados) <- c("Pasos", "Tiempo", "Replica", "Dispercion", "Iteracion")
resultados$Pasos <- as.factor(resultados$Pasos)
resultados$Tiempo <- as.factor(resultados$Tiempo)
resultados$Replica <- as.factor(resultados$Replica)

suppressMessages(library(ggplot2))
ggplot(resultados, aes(Tiempo, Dispercion)) + geom_boxplot() + facet_wrap( ~Pasos, nrow = 3)
ggplot(resultados, aes(`Cantidad de puntos`, Iteracion)) + geom_boxplot() + facet_wrap( ~Tiempo, nrow = 3)

#Esta es la buena.
varianzapromedio <- aggregate(resultados$Dispercion, list(resultados$Pasos, resultados$Tiempo), FUN = mean)
names(varianzapromedio) <- c("Pasos", "Iteraciones", "Varianza")
png(paste("p7.png", sep=""), width=700, height=300)
ggplot(varianzapromedio, aes(Iteraciones, Varianza, group = Pasos, color = Pasos)) + geom_line() + xlab("Iteraciones") + ylab("Varianza")
graphics.off()
