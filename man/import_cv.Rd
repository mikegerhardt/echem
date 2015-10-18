\name{import_cv}
\alias{import_cv}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Import cyclic voltammogram
}
\description{
Import a Gamry .dta file from a cyclic voltammetry experiment.
}
\usage{
import_cv(inputfile = file.choose(), cols = c("Vf", "Im"), refvoltage = 0.21, earea = 0.07065, multiplier = 1000)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{inputfile}{
The file to be imported
}
  \item{cols}{
The columns to be imported.
}
  \item{refvoltage}{
The standard potential of the reference electrode vs SHE. Defaults to Ag/AgCl at 0.210 V. 
This value is added to the Vf column if it exists.
}
  \item{earea}{
Electrode area. The Im column is divided by this number to give current density, if Im exists.
}
  \item{multiplier}{
A conversion factor for putting the current density column in specified units. Defaults to 1000 to convert to mA/cm2.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Returns a data frame with voltage, current, and current density columns.
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
function (inputfile = file.choose(), cols = c("Vf", "Im"), refvoltage = 0.21, 
    earea = 0.07065, multiplier = 1000) 
{
    inputdf <- importGamry(inputfile)
    outdf <- inputdf[, cols]
    if ("Im" \%in\% cols) {
        outdf$currentdensity <- outdf$Im * multiplier/earea
    }
    if ("Vf" \%in\% cols) {
        outdf$Vf <- outdf$Vf + refvoltage
    }
    return(outdf)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
