source("multiagente.R")

l <- 1.5
n <- 50
pr <- 0.02
v <- l / 30
r <- 0.1
pv <- .3 
tmax <- 100

probabilidades <- seq(0, 1, .1)

resultadosangel <- data.frame()

library(parallel)
cluster <- makeCluster(detectCores())
clusterExport(cluster, "l")
clusterExport(cluster, "n")
clusterExport(cluster, "pr")
clusterExport(cluster, "pv")
clusterExport(cluster, "v")
clusterExport(cluster, "r")
clusterExport(cluster, "tmax")
clusterExport(cluster, "multiagente")

for (pi in probabilidades) {
  print(pi)
  clusterExport(cluster, "pi")
  #timeinitial <- Sys.time()
  datos <- parSapply(cluster, 1:40, multiagente)
  #timeangel <- c(timeangel, Sys.time() - timeinitial)
  datos <- cbind(rep(pi, 40), 1:40, datos)
  resultadosangel <- rbind(resultadosangel, datos)
}

stopCluster(cluster)

for (i in 1:40) {
  nombre <- paste("replica", i, ".png")
  png(nombre, width=600, height=300)
  plot(resultadosangel$Tiempo[which(resultadosangel$Replica == i & resultadosangel$Pobabilidad == p)], 
       resultadosangel$Infectados[which(resultadosangel$Replica == i & resultadosangel$Pobabilidad == p)], 
       xlab="Tiempo", ylab="Porcentaje de infectados", col = "green", ylim = c(0, 100));par(new=TRUE)
  plot(resultadoselisa$X1.1[which(resultadosangel$Replica == i & resultadosangel$Pobabilidad == p)], 
       resultadoselisa$porcentages[which(resultadosangel$Replica == i & resultadosangel$Pobabilidad == p)], 
       xlab="Tiempo", ylab="Porcentaje de infectados", col = "red", ylim = c(0, 100))
  graphics.off()
}

names(resultadosangel) <- c("Probabilidad", "Replica", "Tiempo", "NumInfectados", "PorInfectados")
resultadosangel$Probabilidad <- as.factor(resultadosangel$Probabilidad)
resultadosangel$Replica <- as.factor(resultadosangel$Replica)
resultadosangel$Tiempo <- as.factor(resultadosangel$Tiempo)
resultadosangel$NumInfectados <- as.numeric(resultadosangel$NumInfectados)
resultadosangel$PorInfectados <- as.numeric(resultadosangel$PorInfectados)

#Data%maximos
porcientomaximo <- data.frame()
avance <- 0
for (p in probabilidades) {
  for (i in 1:40) {
    bloque <- resultadosangel$PorInfectados[which(resultadosangel$Probabilidad == p & resultadosangel$Replica == i)]
    maximo <- max(bloque)
    porcientomaximo <- rbind(porcientomaximo, resultadosangel[which(bloque %in% maximo)[1] + avance,])
    avance <- avance + 100
  }
}

#Hace 
lol <- aggregate(resultados$PorInfectados, list(resultados$Tiempo, resultados$Categoria, resultados$Probabilidad), FUN = mean)

png("medias.png",width = 1200,height = 800)
ggplot(lol, aes(as.numeric(Group.1), x, group = Group.2, fill = Group.2)) + geom_line() + facet_wrap(~Group.3, nrow = 3) + xlab("Tiempo") + ylab("Porcentaje de infeccion")
graphics.off()
