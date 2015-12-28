\name{import_ocp}
\alias{import_ocp}
\alias{getocp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Cell OCP vs state of charge import
%%  ~~function to do ... ~~
}
\description{
Given a set of OCP measurements at different states of charge, this function
outputs a data frame of OCP vs SoC
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
import_ocp(filepattern = "OCP_vs_SoC", state.of.charge = seq(from = 10, to = 100, by = 10))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filepattern}{
  The tag to look for when importing OCP data files. Usually "OCP_before_CV" or "OCP_vs_SoC" does it.
%%     ~~Describe \code{filepattern} here~~
}
  \item{state.of.charge}{
  A vector of numbers. Each state of charge will be mapped to one of these numbers.
%%     ~~Describe \code{state.of.charge} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Data frame containing each OCP value matched to a state of charge
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
Mike Gerhardt
%%  ~~who you are~~
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
function (filepattern = "OCP_vs_SoC", state.of.charge = seq(from = 10, 
    to = 100, by = 10)) 
{
    voltages <- ldply(mixedsort(list.files(pattern = filepattern)), 
        getocp)
    ocpoutputdf <- data.frame(soc = state.of.charge, open.circuit = voltages)
    colnames(ocpoutputdf) <- c("soc", "ocp")
    return(ocpoutputdf)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
