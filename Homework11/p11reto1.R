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

k <- 3 # cuantas funciones objetivo

library(parallel)
cl <- makeCluster(detectCores() - 1)
clusterExport(cl, funciones)
clusterExport(cl, parametros)

obj <- parSapply(cl, 1:k, function(i) {
    return(list(poli(vc, md, tc)))
    })

minim <- (runif(k) > 0.5)
sign <- (-1)^minim

n <- 300 # cuantas soluciones aleatorias
sol <- matrix(runif(vc * n), nrow=n, ncol=vc)

clusterExport(cl, c("obj", "k"))
val <- matrix(parRapply(cl, sol, function(i) {
    evaluacion <- double()
    for (j in 1:k) { # para todos los objetivos
        evaluacion <- c(evaluacion, eval(obj[[j]], i, tc))
    }
    return(evaluacion)
    }), nrow=n, ncol=k, byrow = TRUE)

clusterExport(cl, c("sign", "val"))
mejores <- parSapply(cl, 1:k, function(i) {
    return(which.max(sign[i] * val[,i]))
    })

clusterExport(cl, "n")
datos <- t(parSapply(cl, 1:n, function(i) {
    d <- logical()
    for (j in 1:n) {
        d <- c(d, domin.by(sign * val[i,], sign * val[j,], k))
    }
    dominadores <- sum(d)
    no.dom <- dominadores == 0 # nadie le domina
    return(c(dominadores, no.dom))
    }))
stopCluster(cl)
datos <- data.frame(Dominadores = datos[,1], NoDominados = as.logical(datos[,2]))

frente <- subset(val, datos$NoDominados) # solamente las no dominadas
cantfrente <- dim(frente)[1]

ordenados <- order(frente[,1])
frente <- frente[ordenados,]
inicio <- frente[1,]
final <- frente[cantfrente,]
r <- sqrt((final[1] - inicio[1])^2 + (final[2] - inicio[2])^2 + (final[3] - inicio[3])^2)/7

newfrente <- c(TRUE, rep(FALSE, cantfrente - 1))
centro <- frente[1,]
for (i in 2:cantfrente) {
    punto <- frente[i,]
    d <- sqrt((centro[1] - punto[1])^2 + (centro[2] - punto[2])^2 + (centro[3] - punto[3])^2)
    if (d > r) {
        newfrente[i] <- TRUE
        centro <- punto
    }
}
newfrente <- subset(frente, newfrente)


cual <- c("max", "min")
xl <- paste("Primer objetivo (", cual[minim[1] + 1], ")", sep="")
yl <- paste("Segundo objetivo (", cual[minim[2] + 1], ")", sep="")
zl <- paste("Tercer objetivo (", cual[minim[3] + 1], ")", sep="")

plot(val[,1], val[,2], xlab=xl, ylab=yl)
points(frente[,1], frente[,2], col="green", pch=16)
points(newfrente[,1], newfrente[,2], col="red", pch=16, cex=1.5)

library("plot3D")
scatter3D(val[,1], val[,2], val[,3], colvar = NULL, theta = -45, phi = 20, bty ="g", 
    xlab = xl, ylab = yl, zlab = zl)
points3D(frente[,1], frente[,2], frente[,3], colvar = NULL, add = TRUE, 
    col = "blue", pch = 19)
points3D(newfrente[,1], newfrente[,2], newfrente[,3], colvar = NULL, add = TRUE, 
    col = "red", pch = 19, cex = 1)