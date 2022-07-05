\name{gSNR}
\alias{gSNR}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
gSNR
%%  ~~function to do ... ~~
}
\description{
  This function calcualte the generalized signal-to-noise ratio.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
gSNR(X, Y, H = NA, discrete = FALSE, pvalue = FALSE, perm = 1000)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
%%     ~~Describe \code{X} here~~
}
  \item{Y}{
%%     ~~Describe \code{Y} here~~
}
  \item{H}{
%%     ~~Describe \code{H} here~~
}
  \item{discrete}{
%%     ~~Describe \code{discrete} here~~
}
  \item{pvalue}{
%%     ~~Describe \code{pvalue} here~~
}
  \item{perm}{
%%     ~~Describe \code{perm} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
library(gSNR)

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

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
