\name{SIR}
\alias{SIR}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
SIR
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
SIR(X, Y, H, discrete = FALSE)
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
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (X, Y, H, discrete = FALSE) 
{
    p <- dim(X)[2]
    n <- dim(X)[1]
    if (discrete == FALSE) {
        ORD <- order(Y)
        X <- X[ORD, ]
        Y <- Y[ORD]
        ms <- array(0, n)
        m <- floor(n/H)
        c <- n\%\%H
        M <- matrix(0, nrow = H, ncol = n)
        if (c == 0) {
            M <- diag(H) \%x\% matrix(1, nrow = 1, ncol = m)/m
            ms <- m + ms
        }
        else {
            for (i in 1:c) {
                M[i, ((m + 1) * (i - 1) + 1):((m + 1) * i)] <- 1/(m + 
                  1)
                ms[((m + 1) * (i - 1) + 1):((m + 1) * i)] <- m
            }
            for (i in (c + 1):H) {
                M[i, ((m + 1) * c + (i - c - 1) * m + 1):((m + 
                  1) * c + (i - c) * m)] <- 1/m
                ms[((m + 1) * c + (i - c - 1) * m + 1):((m + 
                  1) * c + (i - c) * m)] <- m - 1
            }
        }
        X.H <- matrix(0, nrow = H, ncol = dim(X)[2])
        grand.mean <- matrix(apply(X, 2, mean), nrow = 1, ncol = dim(X)[2])
        X.stand.ord <- X - grand.mean \%x\% matrix(1, nrow = dim(X)[1], 
            ncol = 1)
        X.H <- M \%*\% X.stand.ord
    }
    else {
        H <- length(unique(Y))
        X.H <- matrix(0, nrow = H, ncol = dim(X)[2])
        grand.mean <- matrix(apply(X, 2, mean), nrow = 1, ncol = dim(X)[2])
        X.stand.ord <- X - grand.mean \%x\% matrix(1, nrow = dim(X)[1], 
            ncol = 1)
        for (h in 1:H) {
            ind <- which(Y == unique(Y)[h])
            X.H[h, ] <- apply(X.stand.ord[ind, ], 2, mean)
        }
    }
    svd.XH <- svd(X.H, nv = p)
    res.eigen.value <- ((svd.XH$d)^2/H)[1]
    list(lambda = res.eigen.value)
  }
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
