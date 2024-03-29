% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_rvimp_distribution.R
\name{get_distribution}
\alias{get_distribution}
\title{Generate an estimation for RVIMPs distribution based on resamplings}
\usage{
get_distribution(data, confounder, rep, sample_seed, residual.model, num.trees)
}
\arguments{
\item{data}{data set the estimation shall be based on}

\item{confounder}{Variables the distribution shall be estimated for}

\item{rep}{Number of resamplings the distribution shall be estimated on}

\item{sample_seed}{Seed for drawing the resamplings}

\item{residual.model}{Model the residuals of \code{conf} shall be estimated with. Either Linear or RandomForest.}

\item{num.trees}{Number of trees within each ranger object.}
}
\description{
Estimation of the RVIMP-distribution based on resamplings
}
