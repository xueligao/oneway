\name{print.oneway}
\alias{print.oneway}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
print.oneway
}
\description{
print out oneway results
}
\usage{
print.oneway(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
result of oneway analysis
}
  \item{\dots}{
other arguments
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
df
SS 
MS
P value
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
function (x, ...) 
{
    cat("Call:\n")
    print(x$call)
    cat("SS Between group")
    print(x[2])
    cat("\nSS within group")
    print(x[3])
    cat("\ndf Between group")
    print(x[4])
    cat("\ndf within group")
    print(x[5])
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
