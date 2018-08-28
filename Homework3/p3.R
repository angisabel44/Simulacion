source("primo.R")
source("vectores.R")

datos <- vector()

for (nucleos in 0:2) {
  print(paste("Estamos en el nucleo", 4 - nucleos))
  for (digitos in 4:6) {
    print(paste("Estamos con numeros de", digitos, "digitos."))
    if (digitos == 4) {
      numerosprimos <- datosprimos[,digitos - 3]
      numerosnoprimos <- datosprimos[,digitos - 2]
    }
    if (digitos == 5) {
      numerosprimos <- datosprimos[,digitos - 2]
      numerosnoprimos <- datosprimos[,digitos - 1]
    }
    if (digitos == 6) {
      numerosprimos <- datosprimos[,digitos - 1]
      numerosnoprimos <- datosprimos[,digitos]
    }
    for (proporcion in 1:3) {
      if (proporcion == 1) {
        numeros <- rep(sample(numerosprimos), len = 1000)
      }
      if (proporcion == 2) {
        numeros <- rep(sample(numerosnoprimos), len = 1000)
      }
      if (proporcion == 3) {
        numeros <- rep(sample(numerosprimos), len = 500)
        numeros <- c(numeros, rep(sample(numerosnoprimos), len = 500))
      }
      original <- sort(numeros, decreasing = FALSE)
      invertido <- sort(numeros, decreasing = TRUE)
      replicas <- 30
      
      suppressMessages(library(doParallel))
      registerDoParallel(makeCluster(detectCores() - nucleos))
      
      ot <- numeric()
      it <- numeric()
      at <- numeric()
      
      print(paste("Haciendo las replicas de la proporcion", proporcion))
      for (r in 1:replicas) {
        ot <- c(ot, system.time(foreach(n = original, .combine = c) %dopar% primo2(n)))
        it <- c(it, system.time(foreach(n = invertido, .combine = c) %dopar% primo2(n)))
        at <- c(at, system.time(foreach(n = sample(original), .combine = c) %dopar% primo2(n)))
      }
      stopImplicitCluster()
      
      datos <- c(datos, 4 - nucleos, digitos, proporcion, 1, mean(ot[which(ot != 0)]))
      datos <- c(datos, 4 - nucleos, digitos, proporcion, 2, mean(it[which(it != 0)]))
      datos <- c(datos, 4 - nucleos, digitos, proporcion, 3, mean(at[which(at != 0)]))
    }
  }
}

datosfinales <- matrix(datos, nrow = 81, ncol = 5, byrow = TRUE)

png("nucleos.png")
boxplot(datosfinales$Promedio ~datosfinales$Nucleos, xlab = "Nucleos", 
        ylab = "Tiempos")
graphics.off()

png("digitos.png")
boxplot(datosfinales$Promedio ~datosfinales$Digitos, xlab = "Digitos", 
        ylab = "Tiempos")
graphics.off()

png("proporciones.png")
boxplot(datosfinales$Promedio ~datosfinales$Proporcion, xlab = "Proporciones", 
        ylab = "Tiempos")
graphics.off()

png("orden.png")
boxplot(datosfinales$Promedio ~datosfinales$Orden, xlab = "Orden", 
        ylab = "Tiempos")
graphics.off()
