\name{import_cv_cycle}
\alias{import_cv_cycle}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Import constant voltage hold
}
\description{
Imports a Gamry .DTA file from a chronocoulometry experiment (potentiostatic hold).
}
\usage{
import_cv_cycle(inputfile = file.choose(), cols = c("T", "Q", "Vf", "Im"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{inputfile}{
  File to be imported.
}
  \item{cols}{
  Columns to output. Defaults to time, charge, voltage, and current.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Returns a data frame with the specified columns.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Mike Gerhardt
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
function (inputfile = file.choose(), cols = c("T", "Q", "Vf", 
    "Im")) 
{
    inputdf <- importGamry(inputfile)
    outdf <- inputdf[, cols]
    return(outdf)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
