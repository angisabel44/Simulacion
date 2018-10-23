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

obj <- parSapply(cl, 1:l, function(i) {
    return(list(poli(vc, md, tc)))
    })

minim <- (runif(k) > 0.5)
sign <- (-1)^minim

n <- 200 # cuantas soluciones aleatorias
sol <- matrix(runif(vc * n), nrow=n, ncol=vc)

clusterExport(cl, "obj")
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

cual <- c("max", "min")
xl <- paste("Primer objetivo (", cual[minim[1] + 1], ")", sep="")
yl <- paste("Segundo objetivo (", cual[minim[2] + 1], ")", sep="")

#Graficas
png("p11_init.png")
plot(val[,1], val[,2], xlab=xl, ylab=yl, main="Ejemplo bidimensional")
graphics.off()

png("p11_mejores.png")
plot(val[,1], val[,2], xlab=paste(xl, "mejor con cuadro azul"),
     ylab=paste(yl,"mejor con bolita naranja"),
     main="Ejemplo bidimensional")
points(val[mejores[1], 1], val[mejores[1], 2], col="blue", pch=15, cex=1.5)
points(val[mejores[2], 1], val[mejores[2], 2], col="orange", pch=16, cex=1.5)
graphics.off()

png("p11_frente.png")
plot(val[,1], val[,2], xlab=paste(xl, "mejor con cuadro azul"),
     ylab=paste(yl,"mejor con bolita naranja"),
     main="Ejemplo bidimensional")
points(frente[,1], frente[,2], col="green", pch=16, cex=1.5)
graphics.off()

library(ggplot2) # recordar instalar si hace falta
data <- data.frame(pos=rep(0, n), dom=datos$Dominadores)
png("p11_violin.png")
gr <- ggplot(data, aes(x=pos, y=dom)) + geom_violin(fill="orange", color="red")
gr + geom_boxplot(width=0.2, fill="blue", color="white", lwd=2) +
    xlab("") +
    ylab("Frecuencia") +
    ggtitle("Cantidad de soluciones dominantes")
graphics.off()