\name{import_cell_cv}
\alias{import_cell_cv}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Import full cell CV
}
\description{
Imports a cyclic voltammogram data file from an experiment performed on a full cell. Outputs a data frame, which may contain smoothed out current and power based on whether the movingavg and traceavg functions are on or off.
}
\usage{
import_cell_cv(fname = file.choose(), area = 5, rpm = 50, cols = c("T", "Vf", "Im"), movingavg = TRUE, traceavg = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fname}{
The file to import.

}
  \item{area}{
%%     ~~Describe \code{area} here~~
Area of the electrode, for conversion to current density

}
  \item{rpm}{
%%     ~~Describe \code{rpm} here~~

Pump rotations per minute. This is used by the moving average filter to make sure it averages the current over one pump pulse.
}
  \item{cols}{
%%     ~~Describe \code{cols} here~~
Which columns should be imported from the original data file, as a vector of text strings.
}
  \item{movingavg}{
%%     ~~Describe \code{movingavg} here~~
A flag to tell the function whether or not to do the moving average filter to smooth out the pol curve.

}
  \item{traceavg}{
%%     ~~Describe \code{traceavg} here~~
A flag to tell the function whether or not to average the top and bottom traces of the pol curve.
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
Other arguements to be passed on to importGamry or whatever.
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
Returns a data frame containing four columns: the current, voltage, power, and the name of the file from which each data point was extracted (referred to as "soc")
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
Michael Gerhardt
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

import_cell_cv("mrg_CV_#3.DTA")

## The function is currently defined as
function (fname = file.choose(), area = 5, rpm = 50, cols = c("T", 
    "Vf", "Im"), movingavg = TRUE, traceavg = TRUE, ...) 
{
    inputdf <- importGamry(fname, ...)
    outdf <- inputdf[, cols]
    outdf$current <- -outdf$Im/area
    outdf$power <- outdf$current * outdf$Vf
    if (movingavg) {
        sampletime <- outdf$T[[2]] - outdf$T[[1]]
        pts.per.rot <- 60/(rpm * sampletime) + 1
        fparams <- rep(1/pts.per.rot, pts.per.rot)
        outdf$scurrent <- as.vector(filter(outdf$current, fparams, 
            sides = 2))
        outdf$spower <- outdf$scurrent * outdf$Vf
    }
    if (traceavg) {
        outdf <- polcurve_averager(outdf, currentid = if (movingavg) 
            "scurrent"
        else "current")
    }
    outdf$soc <- fname
    return(outdf)
  }
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
