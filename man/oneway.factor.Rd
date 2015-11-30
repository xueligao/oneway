\name{oneway.factor}
\alias{oneway.factor}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
oneway.factor
}
\description{
This method uses the standard input of a factor representing groups and a numeric response. 
}
\usage{
oneway.factor(z, y, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{z}{ The *z* argument should be the factor with levels representing samples used to split the response valuables into a list .

}
  \item{y}{
The response
}
  \item{\dots}{
Other arguments
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Return df, SS, MS, P value.
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
function (z, y, ...) 
{
    x <- split(y, z)
    result <- oneway(x)
    result
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ oneway.factor }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
