\name{summary.oneway}
\alias{summary.oneway}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
summary.oneway
}
\description{
summary the result of oneway analysis
}
\usage{
summary.oneway(object, ...)
}
 
\arguments{
  \item{object}{
the result of oneway analysis
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
function (object, ...) 
{
    MS_between = object[[3]]/object[[1]]
    MS_within = object[[4]]/object[[2]]
    F = MS_between/MS_within
    Pr = 1 - pf(F, object[[2]], object[[1]], lower.tail = T)
    result = data.frame(Df = c(object[[1]], object[[2]]), SS = c(object[[3]], 
        object[[4]]), MS = c(MS_between, MS_within), Fvalue = c(F, 
        NA), Pr = c(Pr, NA))
    row.names(result) <- c("Between group", "Within group")
    result
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ summary.oneway }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
