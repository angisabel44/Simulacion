pick.one <- function(x) {
    if (length(x) == 1) {
        return(x)
    } else {
        return(sample(x, 1))
    }
} 
poli <- function(maxdeg, varcount, termcount) {
    f <- data.frame(variable=integer(), coef=integer(), degree=integer())
    for (t in 1:termcount) {
        var <- pick.one(1:varcount)
        deg <- pick.one(1:maxdeg)
        f <-  rbind(f, c(var, runif(1), deg))
    }
    names(f) <- c("variable", "coef", "degree")
    return(f)
}
eval <- function(pol, vars, terms) {
    value <- 0.0
    for (t in 1:terms) {
        term <- pol[t,]
        value <-  value + term$coef * vars[term$variable]^term$degree
    }
    return(value)
}
domin.by <- function(target, challenger, total) {
    if (sum(challenger < target) > 0) {
        return(FALSE) # hay empeora
    } # si no hay empeora, vemos si hay mejora
    return(sum(challenger > target) > 0)
}

vc <- 4
md <- 3
tc <- 5

funciones <- c("pick.one", "poli", "eval", "domin.by")
parametros <- c("vc", "md", "tc")

k <- 2 # cuantas funciones objetivo

library(parallel)
cl <- makeCluster(detectCores() - 1)
clusterExport(cl, funciones)
clusterExport(cl, parametros)

obj <- parSapply(cl, 1:k, function(i) {
    return(list(poli(vc, md, tc)))
    })

minim <- (runif(k) > 0.5)
sign <- (-1)^minim

n <- 200 # cuantas soluciones aleatorias
sol <- matrix(runif(vc * n), nrow=n, ncol=vc)

clusterExport(cl, c("obj", "n", "k"))
val <- matrix(parRapply(cl, sol, function(i) {
    evaluacion <- double()
    for (j in 1:k) { # para todos los objetivos
        evaluacion <- c(evaluacion, eval(obj[[j]], i, tc))
    }
    return(evaluacion)
    }), nrow=n, ncol=k, byrow = TRUE)

clusterExport(cl, c("sign", "val"))
datos <- t(parSapply(cl, 1:n, function(i) {
    d <- logical()
    for (j in 1:n) {
        d <- c(d, domin.by(sign * val[i,], sign * val[j,], k))
    }
    dominadores <- sum(d)
    no.dom <- dominadores == 0 # nadie le domina
    return(c(dominadores, no.dom))
    }))

datos <- data.frame(Dominadores = datos[,1], NoDominados = as.logical(datos[,2]))
frente <- subset(val, datos$NoDominados) # solamente las no dominadas
sol <- sol[order(datos$Dominadores),]

cual <- c("max", "min")
xl <- paste("Primer objetivo (", cual[minim[1] + 1], ")", sep="")
yl <- paste("Segundo objetivo (", cual[minim[2] + 1], ")", sep="")

pm <- 0.05
rep <- 50
tmax <- 25

digitos <- floor(log(tmax, 10)) + 1
tl <- "0"
while (nchar(tl) < digitos) {
    tl <- paste("0", tl, sep="")
}
png(paste("p11_t", tl, ".png", sep=""))
plot(val[,1], val[,2], xlab=xl, ylab=yl, main="Estado inicial", ylim = c(0, 3), xlim = c(0,3))
points(frente[,1], frente[,2], col="green", pch=16, cex=1.5)
graphics.off()

for (iter in 1:tmax) {
    clusterExport(cl, "sol")

    probabilidades <- runif(n) < pm
    mutados <- which(probabilidades %in% TRUE)

    mutaciones <- t(parSapply(cl, mutados, function(i) {
              pos <- sample(1:vc, 1)
              mut <- sol[i,]
              mut[pos] <- runif(1)
              return(as.numeric(mut))
            }))

    hijos <- matrix(parSapply(cl, 1:rep, function(i) {
              padres <- sample(1:n, 2, replace = FALSE)
              pos <- sample(2:(vc-1), 1)
              x <- sol[padres[1],]
              y <- sol[padres[2],]
              xy <- c(x[1:pos], y[(pos+1):vc])
              yx <- c(y[1:pos], x[(pos+1):vc])
              return(as.numeric(c(xy, yx)))
            }), ncol = vc, byrow = TRUE)

    sol <- rbind(sol, mutaciones, hijos)
    tam <- dim(sol)[1]

    val <- matrix(parRapply(cl, sol, function(i) {
            evaluacion <- double()
            for (j in 1:k) { # para todos los objetivos
                evaluacion <- c(evaluacion, eval(obj[[j]], i, tc))
            }
            return(evaluacion)
            }), nrow=tam, ncol=k, byrow = TRUE)

    clusterExport(cl, c("sign", "val", "tam"))
    datos <- t(parSapply(cl, 1:tam, function(i) {
        d <- logical()
        for (j in 1:tam) {
            d <- c(d, domin.by(sign * val[i,], sign * val[j,], k))
        }
        dominadores <- sum(d)
        no.dom <- dominadores == 0 # nadie le domina
        return(c(dominadores, no.dom))
        }))

    datos <- data.frame(Dominadores = datos[,1], NoDominados = as.logical(datos[,2]))
    mantener <- order(datos$Dominadores)

    frente <- subset(val, datos$NoDominados[mantener]) # solamente las no dominadas
    val <- val[mantener,]
    val <- val[1:n,]
    sol <- sol[mantener,]
    sol <- sol[1:n,]

    tl <- paste(iter, "", sep="")
    while (nchar(tl) < digitos) {
        tl <- paste("0", tl, sep="")
    }
    png(paste("p11_t", tl, ".png", sep=""))
    plot(val[,1], val[,2], xlab=xl, ylab=yl, main=paste("Paso", iter), ylim = c(0, 3), xlim = c(0,3))
    points(frente[,1], frente[,2], col="green", pch=16, cex=1.5)
    graphics.off()
}



stopCluster(cl)