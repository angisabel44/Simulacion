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
 
modelos <- read.csv("digitos.modelo", sep=" ", header=FALSE, stringsAsFactors=F)
modelos[modelos=='n'] <- 0.995
modelos[modelos=='g'] <- 0.92
modelos[modelos=='b'] <- 0.002
 
r <- 5
c <- 3
tope <- 9
replicas <- 15
pasos <- c(1000, 2500, 5000, 10000, 20000) # pasos en total

dim <- r * c 

digitos <- 0:tope
k <- length(digitos)
n <- floor(log(k-1, 2)) + 1

parametros <- c("binario", "decimal", "modelos", "r", "c", "tope", "dim", "k", "n")

library(parallel)
nucleos <- makeCluster(detectCores())
clusterExport(nucleos, parametros)

datos <- data.frame(Replica = integer(), Método = character(), Iteraciones = factor(), Tiempo = double())

for (tmax in pasos) {
    entrenamiento <- ceiling(0.7 * tmax)
    prueba <- tmax - entrenamiento # probamos despues de entrenar

    for (iter in 1:replicas) {
        tiempo <- system.time({
        tasa <- 0.15
        tranqui <- 0.99

        contadores <- matrix(rep(0, k*(k+1)), nrow=k, ncol=(k+1))
        rownames(contadores) <- 0:tope
        colnames(contadores) <- c(0:tope, NA)
 
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
 
    for (t in 1:prueba) { # prueba
        d <- sample(0:tope, 1)
        pixeles <- runif(dim) < modelos[d + 1,] # fila 1 contiene el cero, etc.
        salida <- rep(FALSE, n)
        for (i in 1:n) {
            w <- neuronas[i,]
            resultado <- sum(w * pixeles) >= 0
            salida[i] <- resultado
        }
        r <- min(decimal(salida, n), k) # todos los no-existentes van al final
        contadores[d+1, r+1] <- contadores[d+1, r+1] + 1
    }
    #print(contadores)
    })[3]

    datos <- rbind(datos, 
        data.frame(Replica = iter, Método = "Secuencial", Iteraciones = tmax, Tiempo = as.double(tiempo)))

    tiempo <- system.time({
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
    })[3]

    datos <- rbind(datos, 
        data.frame(Replica = iter, Método = "Paralelo", Iteraciones = tmax, Tiempo = as.double(tiempo)))

    }
}
stopCluster(nucleos)

library(ggplot2)
ggplot(datos, aes(x = Iteraciones, y = Tiempo, color = Método)) + geom_line() + xlab("Cantidad de Iteraciones") + ylab("Tiempo (segundo)") + geom_point()