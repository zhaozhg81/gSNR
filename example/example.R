library(gSNR)

n <- 1000
H <- 20
m <- n/H
sigma <- 1

p <- 20
q <- 5


set.seed(2)
D <- 1


beta <- array(0, c(p, D) )
beta <- rnorm(p, 0, 1)

gamma <- rnorm(q, 0, 1)

X <- matrix( rnorm(p*n), c(n, p) )
Z <- matrix( rnorm(q*n), c(n, q) )


Y <-  sin( X%*%beta*0.2)*exp( X%*% beta *0.2 ) + sigma*rnorm(n,0,1)


res <- gSNR( X, Y, discrete=FALSE, pvalue=TRUE)
res$stat
res$pvalue

ORD <- order(Y)
Y2 <- array(0, n)
Y2[ ORD[1:floor(n/3)] ] <- 1
Y2[ ORD[ (floor(n/3)+1):floor(2*n/3)] ] <- 2
Y2[ ORD[ (floor(2*n/3)+1):n] ] <- 3
  
res.2 <- gSNR( X, Y2, discrete=TRUE, pvalue=TRUE)
res.2$stat
res.2$pvalue


Y <- sin( Z%*%gamma ) + sin( X %*%beta*0.2 )*exp( X %*% beta * 0.2 ) + sigma*rnorm(n,0,1)
## res.covariate <- gSNR:::gSNR_Covariate( X, Z, Y, discrete=FALSE, pvalue=TRUE)
res.covariate <- gSNR:::gSNR_Covariate( X, Z, Y, discrete=FALSE, pvalue=TRUE)
