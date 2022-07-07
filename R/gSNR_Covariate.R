gSNR_Covariate <-
function( X, Z, Y, H=NA, discrete=FALSE, pvalue=FALSE, perm=1000)
{
  p1 <- dim(X)[2]
  p2 <- dim(Z)[2]
  
  n <- dim(X)[1]
  p <- p1+p2
  
  if( is.na(H) )
    {
      H <- floor( sqrt(n) )
    }
  
  if( pvalue==FALSE){
    psi_1 <- SIR( cbind(X,Z), Y, H, discrete)$lambda
    res <- list( stat= psi_1, pvalue=NA, null.dist=NA)
  }else{
    
    psi_1 <- SIR( cbind(X,Z), Y, H, discrete)$lambda

    ## Calculate the statistics using permutation.
    psi_1.perm <- array(0, perm)
    for( no.perm in 1:perm )
      {
        perm.ind <- sample(c(1:n), n, replace=FALSE)
        
        psi_1.perm[no.perm] <- SIR( cbind(X[perm.ind,], Z), Y, H, discrete)$lambda
      }

    pvalue <- mean( psi_1 < psi_1.perm )

    res <- list( stat=psi_1, pvalue=pvalue, null.dist=psi_1.perm)
  }
  res
}
