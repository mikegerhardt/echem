\name{importGamry}
\alias{importGamry}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Generic function for importing any Gamry data file
}
\description{
Imports a Gamry .dta file. First, the number of expected data columns is either input as ncols or calculated. Every line in the .dta file which does not have that number of columns is thrown out. The first remaining line sets the names of each column.
}
\usage{
importGamry(inputfile = file.choose(), ncols = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{inputfile}{
File to be imported
}
  \item{ncols}{
Number of expected data columns. If the default of 0 is used, the function automatically tabulates how many columns each line has and picks the number of columns which shows up most frequently
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Outputs a data frame with all the data in the .dta file.
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
function (inputfile = file.choose(), ncols = 0) 
{
    alldata <- readLines(inputfile)
    fields <- sapply(strsplit(alldata, split = "\t"), length)
    if (ncols == 0) {
        fields.to.remove <- grep(which.max(tabulate(fields)), 
            fields, invert = TRUE)
    }
    else {
        fields.to.remove <- grep(ncols, fields, invert = TRUE)
    }
    usable.data <- alldata[-fields.to.remove]
    if (usable.data[1] \%in\% usable.data[-1]) {
        name.repeats <- grep(usable.data[1], usable.data)[-1]
        importable.data <- usable.data[-name.repeats]
    }
    else {
        importable.data <- usable.data
    }
    outdf <- read.table(textConnection(importable.data), header = TRUE)
    return(outdf)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
