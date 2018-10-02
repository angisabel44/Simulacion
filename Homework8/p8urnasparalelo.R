suppressMessages(library(testit))
suppressMessages(library(doParallel))

source("cumulos.R")
source("funciones.R")

resultados <- data.frame()

k <- 10000      #Cumulos
n <- 1000000    #Particulas

duraciones <- c(2^(1:10))
replicas <- 1:30

cluster <- makeCluster(detectCores())
registerDoParallel(cluster)

for (duracion in duraciones) {
  cumulos <- creacion(k, n)
  
  c <- median(cumulos) # tamanio critico de cumulos
  d <- sd(cumulos) / 4 # factor arbitrario para suavizar la curva
  
  freqoriginal <- as.data.frame(table(cumulos))
  names(freqoriginal) <- c("tam", "num")
  freqoriginal$tam <- as.numeric(levels(freqoriginal$tam))[freqoriginal$tam]
  
  for (replica in replicas) {
    freq <- freqoriginal
    time <- system.time(
    for (paso in 1:duracion) {
      assert(sum(cumulos) == n)
      cumulos <- integer()
      
      cumulos <- foreach(i = 1:dim(freq)[1], .combine = c) %dopar% faserotura(i)
      
      assert(sum(cumulos) == n)
      assert(length(cumulos[cumulos == 0]) == 0) # que no haya vacios
      
      freq <- as.data.frame(table(cumulos)) # actualizar urnas
      names(freq) <- c("tam", "num")
      freq$tam <- as.numeric(levels(freq$tam))[freq$tam]
      
      assert(sum(freq$num * freq$tam) == n)
      cumulos <- integer()
      
      cumulos <- foreach(i = 1:dim(freq)[1], .combine = c) %dopar% faseunion(i)
      
      assert(sum(abs(cumulos)) == n)
      assert(length(cumulos[cumulos == 0]) == 0) # que no haya vacios
      
      juntarse <- -cumulos[cumulos < 0]
      cumulos <- cumulos[cumulos > 0]
      
      assert(sum(cumulos) + sum(juntarse) == n)
      nt <- length(juntarse)
      
      if (nt > 0) {
        if (nt > 1) {
          juntarse <- sample(juntarse)
          juntar <- foreach(i = 1:floor(nt / 2), .combine = c) %dopar% fasejuntarse(i)
          cumulos <- c(cumulos, juntar)
        }
        if (nt %% 2 == 1) {
          cumulos <- c(cumulos, juntarse[nt])
        }
      }
      
      assert(sum(cumulos) == n)
      
      freq <- as.data.frame(table(cumulos))
      names(freq) <- c("tam", "num")
      freq$tam <- as.numeric(levels(freq$tam))[freq$tam]
      
      assert(sum(freq$num * freq$tam) == n)
    })[3]
    resultados <- rbind(resultados, c(duracion, replica, time))
  }
}
stopImplicitCluster()
