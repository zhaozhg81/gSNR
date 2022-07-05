source("gSNR.R")
source("SIR.R")

n <- 1000
H <- 20
m <- n/H
sigma <- 1

p <- 20

set.seed(2)
D <- 1


beta <- array(0, c(p, D) )
beta <- rnorm(p, 0, 1)


X <- matrix( rnorm(p*n), c(n, p) )


Z <- X%*% beta
Y <-  sin(Z[,1])*exp(Z[,1])*0.2 + sigma*rnorm(n,0,1)


res <- gSNR( X, Y, discrete=FALSE, pvalue=TRUE)

ORD <- order(Y)
Y2 <- array(0, n)
Y2[ ORD[1:floor(n/3)] ] <- 1
Y2[ ORD[ (floor(n/3)+1):floor(2*n/3)] ] <- 2
Y2[ ORD[ (floor(2*n/3)+1):n] ] <- 3
  
res.2 <- gSNR( X, Y2, discrete=TRUE, pvalue=TRUE)
