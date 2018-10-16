library(testit)
library(parallel)
source("funciones.R")

n <- 100        #Cantidad de objetos
generaciones <- c(200, 300, 500)     #Tamaño de la poblacion
pmutaciones <- c(0.05, 0.1, 0.2)      #probabilidad de mutacion
reproducciones <- c(50, 75, 125)       #cantidad de reproducciones
iteraciones <- c(50, 100, 200)      #Tiempo maximo
replicas <- 5  #Replicas

#Data Frame
datos <- data.frame(Réplica = integer(), Optimo = integer(),
                    Generaciones = numeric(), ProbMut = numeric(),
                    Reproducciones = numeric(), Iteraciones = numeric(),
                    Objetivo = double())

#Creamos cluster
cluster <- makeCluster(detectCores())

#Generadores
pesos <- generador.pesos(n, 15, 80)
valores <- generador.valores(pesos, 10, 500)
capacidad <- round(sum(pesos) * 0.65)

#Optimo
optimo <- knapsack(capacidad, pesos, valores)[2]

clusterExport(cluster, "n")
clusterExport(cluster, "pesos")
clusterExport(cluster, "valores")
clusterExport(cluster, "capacidad")

for (tmax in iteraciones) {
  for (rep in reproducciones) {
    for (pm in pmutaciones) {
      for (init in generaciones) {
        for (r in 1:replicas) {
          
          #Inicio del genetico parsapply
          pobl <- t(parSapply(cluster, 1:init, function(i) {
            return(round(runif(n)))
          }))
          
          p <- as.data.frame(pobl)
          tam <- dim(p)[1]
          
          assert(tam == init)
          
          mejores <- double()
          
          for (iter in 1:tmax) {
            p$obj <- NULL
            p$fact <- NULL
            
            clusterExport(cluster, "p")
            clusterExport(cluster, "tam")
            
            probabilidades <- runif(tam) < pm
            mutados <- which(probabilidades %in% TRUE)
            
            mutaciones <- t(parSapply(cluster, mutados, function(i) {
              pos <- sample(1:n, 1)
              mut <- p[i,]
              mut[pos] <- (!p[i,][pos]) * 1
              return(as.numeric(mut))
            }))
            
            hijos <- matrix(parSapply(cluster, 1:rep, function(i) {
              padres <- sample(1:tam, 2, replace = FALSE)
              pos <- sample(2:(n-1), 1)
              x <- p[padres[1],]
              y <- p[padres[2],]
              xy <- c(x[1:pos], y[(pos+1):n])
              yx <- c(y[1:pos], x[(pos+1):n])
              return(as.numeric(c(xy, yx)))
            }), ncol = n, byrow = TRUE)
            
            p <- rbind(p, mutaciones, hijos)
            tam <- dim(p)[1]
            
            clusterExport(cluster, "p")
            clusterExport(cluster, "tam")
            
            p$obj <- parSapply(cluster, 1:tam, function(i) {
              return(sum(p[i,] * valores))
            })
            
            p$fact <- parSapply(cluster, 1:tam, function(i) {
              return(sum(p[i,] * pesos) <= capacidad)
            })
            
            mantener <- order(-p[, (n + 2)], -p[, (n + 1)])[1:init]
            
            p <- p[mantener,]
            tam <- dim(p)[1]
            
            assert(tam == init)
            
            factibles <- p[p$fact == TRUE,]
            mejor <- max(factibles$obj)
            mejores <- c(mejores, mejor)
            
          }
          
          datos <- rbind(datos, data.frame(Réplica = r, Optimo = optimo,
                                           Generaciones = init, ProbMut = pm,
                                           Reproducciones = rep, Iteraciones = tmax,
                                           Objetivo = max(mejores)))
        }
      }
    }
  }
}

stopCluster(cluster)
