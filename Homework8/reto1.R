library(testit)

source("funciones.R")
source("cumulos.R")

cantidadcumulos <- c(500, 1000, 2000, 3000, 5000)      #Cumulos
cantidadparticulas <- c(100000, 150000, 300000)       #Particulas

duracion <- 10
replicas <- 1:30

resultados <- data.frame(Metodo = character(), Cumulos = factor(), Particulas = factor(), 
                         Replica = numeric(), Tiempo = numeric())

for (k in cantidadcumulos) {
  for (n in cantidadparticulas) {
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
          
          for (i in 1:dim(freq)[1]) { # fase de rotura
            urna <- freq[i,]
            if (urna$tam > 1) { # no tiene caso romper si no se puede
              cumulos <- c(cumulos, romperse(urna$tam, urna$num))
            } else {
              cumulos <- c(cumulos, rep(1, urna$num))
            }
          }
          
          assert(sum(cumulos) == n)
          assert(length(cumulos[cumulos == 0]) == 0) # que no haya vacios
          
          freq <- as.data.frame(table(cumulos)) # actualizar urnas
          names(freq) <- c("tam", "num")
          freq$tam <- as.numeric(levels(freq$tam))[freq$tam]
          
          assert(sum(freq$num * freq$tam) == n)
          cumulos <- integer()
          
          for (i in 1:dim(freq)[1]) { # fase de union
            urna <- freq[i,]
            cumulos <- c(cumulos, unirse(urna$tam, urna$num))
          }
          
          assert(sum(abs(cumulos)) == n)
          assert(length(cumulos[cumulos == 0]) == 0) # que no haya vacios
          
          juntarse <- -cumulos[cumulos < 0]
          cumulos <- cumulos[cumulos > 0]
          assert(sum(cumulos) + sum(juntarse) == n)
          
          nt <- length(juntarse)
          if (nt > 0) {
            if (nt > 1) {
              juntarse <- sample(juntarse)
              for (i in 1:floor(nt / 2) ) {
                cumulos <- c(cumulos, juntarse[2*i-1] + juntarse[2*i])
              }
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
      
      resultados <- rbind(resultados, data.frame(Metodo = "Secuencial", Cumulos = k, Particulas = n,
                                                 Replica = replica, Tiempo = as.numeric(time)))
    }
  }
}

suppressMessages(library(doParallel))

cluster <- makeCluster(detectCores())
registerDoParallel(cluster)

for (k in cantidadcumulos) {
  for (n in cantidadparticulas) {
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
      
      resultados <- rbind(resultados, data.frame(Metodo = "Paralelo", Cumulos = k, Particulas = n,
                                                 Replica = replica, Tiempo = as.numeric(time)))
    }
  }
}

stopImplicitCluster()