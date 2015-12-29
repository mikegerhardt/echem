\name{echem-package}
\alias{echem-package}
\alias{echem}
\docType{package}
\title{
Importing and manipulating data from Gamry potentiostats
}
\description{
Import .DTA files output from Gamry potentiostats during cyclic voltammetry, charge/discharge cycling, etc
}
\details{
\tabular{ll}{
Package: \tab echem\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2015-10-17\cr
License: \tab MIT\cr
}
The importGamry() function is designed to import almost any Gamry data file and split it up into its columns,
provided you know how many columns there are.

All other import_ functions are variants of importGamry designed to make specific files, like polarization curves and charge/discharge capacity, easy to import.

There are also a few plotting functions based on the ggplot2 package. 
These are still somewhat under development, but they should work with the rest of the ggplot2 functions.

I recommend creating scripts which import files of a given name in a given location. Then, every time you run an experiment, you can simply copy new data into the folder, or copy the script into a new location, and re-run the script on the new data. What tends to work for me is that I create a new R project for each experiment. You can use save() and load() in the base R package to save data from one project and import it into another.
}
\author{
Mike Gerhardt

Maintainer: Mike Gerhardt <gerhardt.mike@gmail.com>

}
\references{
Check out the Gamry website?
}

\keyword{ package }
\seealso{
\code{\link{ggplot}}
}
\examples{
importGamry()
}
