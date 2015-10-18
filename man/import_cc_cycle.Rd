\name{import_cc_cycle}
\alias{import_cc_cycle}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Import constant current cycling curve
}
\description{
Looks at a Gamry .DTA file, imports it, cuts out all header data, takes columns specified by "cols".
Also makes a column for charge
}
\usage{
import_cc_cycle(inputfile = file.choose(), cols = c("T", "Vf", "Im"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{inputfile}{
The file to be imported
}
  \item{cols}{
Which columns to be imported - defaults to time, voltage, current
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Returns a data frame with time, voltage, current, and charge
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
function (inputfile = file.choose(), cols = c("T", "Vf", "Im")) 
{
    inputdf <- importGamry(inputfile)
    outdf <- inputdf[, cols]
    outdf$Q <- outdf[, "T"] * mean(outdf[, "Im"])
    return(outdf)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
