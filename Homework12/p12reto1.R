binario <- function(d, l) {
    b <-  rep(FALSE, l)
    while (l > 0 | d > 0) {
        b[l] <- (d %% 2 == 1)
        l <- l - 1
        d <- bitwShiftR(d, 1)
    }
    return(b)
}
 
decimal <- function(bits, l) {
    valor <- 0
    for (pos in 1:l) {
        valor <- valor + 2^(l - pos) * bits[pos]
    }
    return(valor)
}

r <- 5
c <- 3
tope <- 9
tmax <- 10000
replicas <- 10

dim <- r * c

entrenamiento <- ceiling(0.7 * tmax)
prueba <- tmax - entrenamiento # probamos despues de entrenar

digitos <- 0:tope
k <- length(digitos)
n <- floor(log(k-1, 2)) + 1

parametros <- c("binario", "decimal", "r", "c", "tope", "dim", "k", "n")

library(parallel)
nucleos <- makeCluster(detectCores())
clusterExport(nucleos, parametros)

negros <- c(0.995, 0.8)
grises <- c(0.75, 0.65)
blancos <- c(0.01, 0.001)

datos <- data.frame(Replica = integer(), Negros = numeric(), Grises = numeric(), Blancos = numeric(), Porcentaje = numeric())

for (black in negros) {
    for (gray in grises) {
        for (white in blancos) {
            modelos <- read.csv("digitos.modelo", sep=" ", header=FALSE, stringsAsFactors=F)
            modelos[modelos=='n'] <- black
            modelos[modelos=='g'] <- gray
            modelos[modelos=='b'] <- white

            clusterExport(nucleos, "modelos")

            for (iter in 1:replicas) {
                    tasa <- 0.15
                    tranqui <- 0.99

                    neuronas <- matrix(runif(n * dim), nrow=n, ncol=dim) # perceptrones

                    for (t in 1:entrenamiento) { # entrenamiento
                        d <- sample(0:tope, 1)
                        pixeles <- runif(dim) < modelos[d + 1,]
                        correcto <- binario(d, n)
                        for (i in 1:n) {
                            w <- neuronas[i,]
                            deseada <- correcto[i]
                            resultado <- sum(w * pixeles) >= 0
                            if (deseada != resultado) {
                                ajuste <- tasa * (deseada - resultado)
                                tasa <- tranqui * tasa
                                neuronas[i,] <- w + ajuste * pixeles
                            }
                        }
                    }

                    numeros <- table(sample(0:tope, prueba, replace = TRUE))
                    numeros <- cbind(0:tope, as.numeric(numeros))

                    clusterExport(nucleos, "neuronas")

                    contadores <- parRapply(nucleos, numeros, function(l){
                        count <- rep(0, k+1)
                        num <- l[1]
                        cuantos <- l[2]
                        modelo <- modelos[num+1,]
                        for (i in 1:cuantos) {
                          pixeles <- runif(dim) < modelo
                          salida <- rep(FALSE, n)
                          for (j in 1:n) {
                            w <- neuronas[j,]
                            resultado <- sum(w * pixeles) >= 0
                            salida[j] <- resultado
                          }
                          digito <- min(decimal(salida, n), k)
                          count[digito + 1] <- count[digito + 1] + 1
                        }
                        return(count)
                    })

                    contadores <- matrix(contadores, nrow = k, ncol = (k+1), byrow = TRUE)
                    rownames(contadores) <- 0:tope
                    colnames(contadores) <- c(0:tope, NA)

                    #print(contadores)
                    porcentaje <- 100 * (prueba - sum(diag(contadores)))/prueba

                    datos <- rbind(datos,
                        data.frame(Replica = iter, Negros = black, Grises = gray, Blancos = white, Porcentaje = porcentaje))
            }
        }
    }
}