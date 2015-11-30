\name{oneway}
\alias{oneway}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
oneway analysis
}
\description{
oneway analysis of varience
}
\usage{
oneway(z, ...)
}

\arguments{
  \item{z}{
The list input
}
  \item{\dots}{
Other arguments
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
df
SS
MS
p value

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
UseMethod("oneway")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ oneway }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
