\name{gSNR}
\alias{gSNR}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
gSNR
%%  ~~function to do ... ~~
}
\description{
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
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (X, Y, H = NA, discrete = FALSE, pvalue = FALSE, perm = 1000) 
{
    p <- dim(X)[2]
    n <- dim(X)[1]
    if (is.na(H)) {
        H <- floor(sqrt(n))
    }
    if (pvalue == FALSE) {
        psi_1 <- SIR(X, Y, H, discrete)$lambda
        res <- list(stat = psi_1, pvalue = NA, null.dist = NA)
    }
    else {
        psi_1 <- SIR(X, Y, H, discrete)$lambda
        psi_1.perm <- array(0, perm)
        for (no.perm in 1:perm) {
            Y.perm <- sample(Y, n, replace = FALSE)
            psi_1.perm[no.perm] <- SIR(X, Y.perm, H, discrete)$lambda
        }
        pvalue <- mean(psi_1 < psi_1.perm)
        res <- list(stat = psi_1, pvalue = pvalue, null.dist = psi_1.perm)
    }
    res
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
