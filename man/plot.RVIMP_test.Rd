% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_RVIMP_test.R
\name{plot.RVIMP_test}
\alias{plot.RVIMP_test}
\title{Generates the test desicions criteria plot based on RVIMP distribution}
\usage{
\method{plot}{RVIMP_test}(x, which = "all", ask = T, ...)
}
\arguments{
\item{x}{\code{RVIMP_test} object.}

\item{which}{For which variable shall the plot be generated. Either a \link{vector} with the names of the desired variables or \code{"all"} to generate the plot for each variable tested for dependencies.}

\item{ask}{Logical. If \code{TRUE} the user is asked to press Enter before a new figure is drawn. If \code{FALSE} all figures are drawn at once.}

\item{...}{Further arguments to the \link{plot} function.}
}
\description{
Generates the test desicions criteria plot based on RVIMP distribution for variables checked for dependencies
}
