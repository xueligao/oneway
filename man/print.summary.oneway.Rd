\name{print.summary.oneway}
\alias{print.summary.oneway}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
print.summary
}
\description{
print out the summary of oneway result
}
\usage{
print.summary.oneway(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Result of oneway analysis
}
  \item{\dots}{
other arguments
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
function (x, ...) 
{
    cat("Call:\n")
    print(x$call)
    cat("\n")
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ print.summary }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
