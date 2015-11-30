\name{oneway.default}
\alias{oneway.default}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
oneway.default
}
\description{
Default for oneway analysis of varience
}
\usage{
oneway.default(z, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{z}{
A list of input
}
  \item{\dots}{
Other arguments
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
function (z, ...) 
{
    est <- onewayEst(z)
    est$call <- match.call()
    class(est) <- "oneway"
    est
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ oneway.default }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
