SIR <-
function( X, Y, H, discrete=FALSE){
    
    ## p <- 1200
    ## n <- 480
    ## H <- 10
    ## m <- n/H

        
    p <- dim(X)[2]
    n <- dim(X)[1]
    

    if( discrete == FALSE)
      {
        
        ORD <- order( Y )
        X <- X[ORD, ]
        Y <- Y[ORD]

        ## Construct the  matrix M
        ms <- array(0, n)
        m <- floor( n/H )
        c <- n%%H
        M <- matrix(0, nrow=H, ncol=n )
        if( c==0 )
          {
            M <- diag( H ) %x% matrix( 1, nrow=1, ncol= m )/m
            ms <- m+ms
          }else{
            for(i in 1:c){
              M[i, ( (m+1)*(i-1)+1):( (m+1)*i )] <- 1/(m+1)
              ms[ ( (m+1)*(i-1)+1):( (m+1)*i) ] <- m
            }
            for( i in (c+1): H ){
              M[i, ( (m+1)*c + (i-c-1)*m +1):( (m+1)*c+(i-c)*m)] <- 1/m
              ms[ ( (m+1)*c +(i-c-1)*m+1):( (m+1)*c+(i-c)*m) ] <- m-1
            }
          }
        
        
        X.H <- matrix(0, nrow=H, ncol= dim(X)[2] )
        grand.mean <- matrix( apply(X, 2, mean ), nrow=1, ncol=dim(X)[2] )
        X.stand.ord <- X - grand.mean %x% matrix(1, nrow=dim(X)[1], ncol=1)   
        X.H <- M%*% X.stand.ord
      }else{
        H <- length( unique(Y) )
        X.H <- matrix(0, nrow=H, ncol= dim(X)[2] )
        grand.mean <- matrix( apply(X, 2, mean ), nrow=1, ncol=dim(X)[2] )
        X.stand.ord <- X - grand.mean %x% matrix(1, nrow=dim(X)[1], ncol=1)

        for(h in 1:H)
          {
            ind <- which( Y == unique(Y)[h] )
            X.H[h, ] <- apply( X.stand.ord[ ind, ], 2, mean)
          }        
     }


    svd.XH <- svd(X.H, nv=p)

    res.eigen.value <- ( (svd.XH$d)^2/H )[1]


    list( lambda= res.eigen.value )
}
