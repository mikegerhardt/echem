\name{import_all_cc_cycles}
\alias{import_all_cc_cycles}
\alias{import_all_cv_cycles}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Import each cycle in a folder
}
\description{
This function is just a convenient ldply wrapper for importing all the constant
current or constant voltage cycling curves from a folder. It relies on differences in filename to distinguish charge vs discharge
}
\usage{
import_all_cc_cycles(filepath = "mrgPWRCAPACITY/CHARGE_DISCHARGE/", allcyclesid = "CHARGE", dischargeid = "DISCHARGE", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filepath}{
    Path where the charge/discharge cycle files are located
}
  \item{allcyclesid}{
    All files in filepath are searched for the existence of allcyclesid.
    Each file with allcyclesid in its filename is assumed to be either a charge or discharge cycle.
}
  \item{dischargeid}{
%%     ~~Describe \code{dischargeid} here~~
    This differentiates the charge cycles from the discharge cycles by looking for dischargeid in each filename.
}
  \item{\dots}{
    arguments to pass to import_cc_cycle or import_cv_cycle
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
%%  ~~who you are~~
  Michael Gerhardt
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{import_cc_cycle}}
\code{\link{import_cv_cycle}}
}

\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (filepath = "mrgPWRCAPACITY/CHARGE_DISCHARGE/", allcyclesid = "CHARGE", 
    dischargeid = "DISCHARGE", ...) 
{
    if (!substr(filepath, nchar(filepath), nchar(filepath)) \%in\% c("/", "/")) {
        filepath <- paste(filepath, "/", sep = "")
    }
    all_cycles_list <- list.files(pattern = allcyclesid, path = filepath)
    discharge_cycles_list <- lapply(mixedsort(grep(pattern = dischargeid, 
        all_cycles_list, value = TRUE)), function(x) paste(filepath, 
        x, sep = ""))
    charge_cycles_list <- lapply(mixedsort(grep(pattern = dischargeid, 
        all_cycles_list, value = TRUE, invert = TRUE)), function(x) paste(filepath, 
        x, sep = ""))
    cycle_numbers <- seq(from = 1, to = length(charge_cycles_list), 
        by = 1)
    charge_cycles_data <- lapply(charge_cycles_list, FUN = import_cc_cycle, 
        ...)
    discharge_cycles_data <- lapply(X = discharge_cycles_list, 
        FUN = import_cc_cycle, ...)
    for (i in cycle_numbers) {
        charge_cycles_data[[i]]$cycle <- i
        discharge_cycles_data[[i]]$cycle <- i
    }
    charge_cycles_df <- ldply(charge_cycles_data)
    charge_cycles_df$type <- "Charge"
    discharge_cycles_df <- ldply(discharge_cycles_data)
    discharge_cycles_df$type <- "Discharge"
    all_cycles_df <- rbind(charge_cycles_df, discharge_cycles_df)
    return(all_cycles_df)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
