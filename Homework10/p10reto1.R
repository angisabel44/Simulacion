library(testit)
library(parallel)
source("funciones.R")

init <- 200     #Tamaño de la poblacion
pm <- 0.05      #probabilidad de mutacion
rep <- 50       #cantidad de reproducciones
tmax <- 50      #Tiempo maximo
k <- 20         #Parametro de supervivencia
replicas <- 10  #Replicas

cantobjetos <- round(exp(4:10)) #Cantidades de objetos

#Data Frame
datos <- data.frame(Réplica = integer(), Objetos = integer(),
                    Selección = character(), Generación = character(),
                    Objetivo = double())
optimo <- double()

#Creamos cluster
cluster <- makeCluster(detectCores())

for (n in cantobjetos) {
  #Generadores
  pesos <- generador.pesos(n, 15, 80)
  valores <- generador.valores(pesos, 10, 500)
  capacidad <- round(sum(pesos) * 0.65)
  
  #Optimo
  exacto <- knapsack(capacidad, pesos, valores)[2]
  
  clusterExport(cluster, "n")
  clusterExport(cluster, "pesos")
  clusterExport(cluster, "valores")
  clusterExport(cluster, "capacidad")
  for (r in replicas) {
    
    optimo <- c(optimo, exacto)
    
    #Inicio del genetico aleatorio + mejores
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
    
    datos <- rbind(datos, data.frame(Réplica = r, Objetos = n,
                                     Selección = "Aleatorio", Generación = "Mejores",
                                     Objetivo = max(mejores)))
    
    #Inicio del genetico ruleta + mejores
    pobl <- t(parSapply(cluster, 1:init, function(i) {
      return(round(runif(n)))
    }))
    
    p <- as.data.frame(pobl)
    tam <- dim(p)[1]
    
    assert(tam == init)
    
    mejores <- double()
    
    clusterExport(cluster, "p")
    p$obj <- parSapply(cluster, 1:tam, function(i) {
      return(sum(p[i,] * valores))
    })
    
    for (iter in 1:tmax) {
      obj <- p$obj/sum(p$obj)
      
      p$obj <- NULL
      p$fact <- NULL
      
      clusterExport(cluster, "p")
      clusterExport(cluster, "tam")
      clusterExport(cluster, "obj")
      
      probabilidades <- runif(tam) < pm
      mutados <- which(probabilidades %in% TRUE)
      
      mutaciones <- t(parSapply(cluster, mutados, function(i) {
        pos <- sample(1:n, 1)
        mut <- p[i,]
        mut[pos] <- (!p[i,][pos]) * 1
        return(as.numeric(mut))
      }))
      
      hijos <- matrix(parSapply(cluster, 1:rep, function(i) {
        padres <- sample(1:tam, 2, replace = FALSE, prob = obj)
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
    
    datos <- rbind(datos, data.frame(Réplica = r, Objetos = n,
                                     Selección = "Ruleta", Generación = "Mejores",
                                     Objetivo = max(mejores)))
    
    #Inicio del genetico aleatorio + supervivencia
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
      
      ordenados <- order(-p[, (n + 2)], -p[, (n + 1)])
      p <- p[ordenados,]
      tam <- dim(p)[1]
      
      probabilidad <- p$obj[(k+1):tam]/sum(factibles$obj[(k+1):tam])
      mantener <- c(ordenados[1:k], sample(ordenados[(k+1):tam], init - k, replace = FALSE, prob = probabilidad))
      
      p <- p[mantener,]
      tam <- dim(p)[1]
      
      assert(tam == init)
      
      factibles <- p[p$fact == TRUE,]
      mejor <- max(factibles$obj)
      mejores <- c(mejores, mejor)
      
    }
    
    datos <- rbind(datos, data.frame(Réplica = r, Objetos = n,
                                     Selección = "Aleatorio", Generación = "Supervivencia",
                                     Objetivo = max(mejores)))
    
    #Inicio del genetico ruleta + supervivencia
    pobl <- t(parSapply(cluster, 1:init, function(i) {
      return(round(runif(n)))
    }))
    
    p <- as.data.frame(pobl)
    tam <- dim(p)[1]
    
    assert(tam == init)
    
    mejores <- double()
    
    clusterExport(cluster, "p")
    p$obj <- parSapply(cluster, 1:tam, function(i) {
      return(sum(p[i,] * valores))
    })
    
    for (iter in 1:tmax) {
      obj <- p$obj/sum(p$obj)
      
      p$obj <- NULL
      p$fact <- NULL
      
      clusterExport(cluster, "p")
      clusterExport(cluster, "tam")
      clusterExport(cluster, "obj")
      
      probabilidades <- runif(tam) < pm
      mutados <- which(probabilidades %in% TRUE)
      
      mutaciones <- t(parSapply(cluster, mutados, function(i) {
        pos <- sample(1:n, 1)
        mut <- p[i,]
        mut[pos] <- (!p[i,][pos]) * 1
        return(as.numeric(mut))
      }))
      
      hijos <- matrix(parSapply(cluster, 1:rep, function(i) {
        padres <- sample(1:tam, 2, replace = FALSE, prob = obj)
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
      
      ordenados <- order(-p[, (n + 2)], -p[, (n + 1)])
      p <- p[ordenados,]
      tam <- dim(p)[1]
      
      probabilidad <- p$obj[(k+1):tam]/sum(factibles$obj[(k+1):tam])
      mantener <- c(ordenados[1:k], sample(ordenados[(k+1):tam], init - k, replace = FALSE, prob = probabilidad))
      
      p <- p[mantener,]
      tam <- dim(p)[1]
      
      assert(tam == init)
      
      factibles <- p[p$fact == TRUE,]
      mejor <- max(factibles$obj)
      mejores <- c(mejores, mejor)
      
    }
    
    datos <- rbind(datos, data.frame(Réplica = r, Objetos = n,
                                     Selección = "Ruleta", Generación = "Supervivencia",
                                     Objetivo = max(mejores)))
  }
}
stopCluster(cluster)
