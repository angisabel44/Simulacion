library(readr)
primes1 <- read_table2("primes1.txt", col_names = FALSE)
primow<-primes1[which(primes1[,1] != 0),1:8]

f = as.numeric(data.matrix(primow))

vector1 <- f[which(f < 10000)]
vector2 <- f[which(f > 10000 & f < 100000)]
vector3 <- f[which(f > 100000 & f < 1000000)]

vector11 <- c(seq(1000, 2000, 2), seq(2000, 4000, 3), seq(4000, 6000, 5))
vector22 <- c(seq(10000, 20000, 2), seq(20000, 40000, 3), seq(40000, 60000, 5))
vector33 <- c(seq(100000, 200000, 2), seq(200000, 400000, 3), seq(400000, 600000, 5))

datosprimos <- cbind(vector1, vector11, vector2, vector22, vector3, vector33)