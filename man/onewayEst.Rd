\name{onewayEst}
\alias{onewayEst}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
onewayEst
}
\description{
oneway analysis of varience
}
\usage{
onewayEst(z)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{z}{
A list of components
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
Xueli Gao
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
function (z) 
{
    s = sapply(z, sum)
    Total = sum(unlist(z)^2)
    group_n <- sapply(z, length)
    A = sum(s^2/group_n)
    CF <- (sum(unlist(z)))^2/length(unlist(z))
    SS_between <- A - CF
    SS_within <- Total - A
    df_between <- length(z) - 1
    df_within <- length(unlist(z)) - length(z)
    MSb <- (A - CF)/(length(z) - 1)
    MSw <- (Total - A)/(length(unlist(z)) - length(z))
    F <- MSb/MSw
    p <- 1 - pf(F, df_between, df_within, lower.tail = T)
    anotable = cbind(df = c(df_between, df_within), SS = c(SS_between, 
        SS_within), MS = c(MSb, MSw), F = c(F, NA), `Pr(<F)` = c(p, 
        NA))
    rownames(anotable) <- c("Treatment", "Residuals")
    printCoefmat(anotable, P.values = T, has.Pvalue = T, signif.stars = T, 
        na.print = "")
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ onewayEst}
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
