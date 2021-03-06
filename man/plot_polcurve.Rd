% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_polcurve.R
\name{plot_polcurve}
\alias{plot_polcurve}
\alias{plot_powercurve}
\title{Plot polarization curves}
\usage{
plot_polcurve(inputdf, xdata = "avgcurrent", ydata = "avgvoltage")

plot_powercurve(inputdf, xdata = "avgcurrent", ydata = "avgpower")
}
\arguments{
\item{inputdf}{Data frame with the data to plot.}

\item{xdata}{Name of the column under which the current data are stored.}

\item{ydata}{Name of the column under which the voltage/power data are stored.}
}
\value{
Returns a ggplot polarization curve, which should be manipulable
  by functions in the ggplot package.
}
\description{
This function takes a data frame with current and voltage data and
makes a polarization curve plot
}
\details{
This function is designed to be used on data frames generated with
\code{import_cell_cv}. But in reality as long as the data frames have named
current, voltage, and power columns, and those columns are named, this
function should be able to generate polarization curves from those data
frames.
}
\section{Functions}{
\itemize{
\item \code{plot_powercurve}: Plot power density versus current density, to see
peak galvanic power density.
}}
\author{
Mike Gerhardt
}
\seealso{
\code{\link[ggplot2]{ggplot}}, \code{\link{import_cell_cv}}
}

