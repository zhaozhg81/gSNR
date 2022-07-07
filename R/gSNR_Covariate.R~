gSNR <-
function( X, Y, H=NA, discrete=FALSE, pvalue=FALSE, perm=1000)
{
  p <- dim(X)[2]
  n <- dim(X)[1]
  
  if( is.na(H) )
    {
      H <- floor( sqrt(n) )
    }
  
  if( pvalue==FALSE){
    psi_1 <- SIR( X, Y, H, discrete)$lambda
    res <- list( stat= psi_1, pvalue=NA, null.dist=NA)
  }else{
    psi_1 <- SIR( X, Y, H, discrete)$lambda

    ## Calculate the statistics using permutation.
    psi_1.perm <- array(0, perm)
    for( no.perm in 1:perm )
      {
        Y.perm <- sample(Y, n, replace=FALSE)
        psi_1.perm[no.perm] <- SIR( X, Y.perm, H, discrete)$lambda
      }

    pvalue <- mean( psi_1 < psi_1.perm )

    res <- list( stat=psi_1, pvalue=pvalue, null.dist=psi_1.perm)
  }
  res
}
