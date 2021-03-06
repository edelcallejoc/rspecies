% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SpNaBaEps.R
\docType{class}
\name{SpNaBaEps-class}
\alias{SpNaBaEps-class}
\alias{initialize,SpNaBaEps-method}
\alias{SpNaBaEps}
\alias{show,SpNaBaEps-method}
\title{Class SpNaBaEps}
\usage{
SpNaBaEps(Ecx, ...)
}
\arguments{
\item{Ecx}{numeric \code{matrix}. See slot section for more details.}

\item{...}{argument pass to other methods.}
}
\value{
An object of class \code{SpNaBaEps}. See slot section for more details.
}
\description{
SpNaBaEps is used to store Epsilon's parameters of the Naive Bayes model.
}
\section{Slots}{

\describe{
\item{\code{Ecx}}{\code{matrix} with \eqn{S} rows and \eqn{M} columns. Each element
represents the epsilon parameter \eqn{\epsilon(c|x)}. See
\code{\link[rspecies]{SpNaBaMatrix}}.}
}}

\examples{

# Using SpNaBaMatrix constructor ----------------------

a <- matrix(rbinom(100, 100, runif(100, 0, 0.1)), nrow = 10, ncol = 10,
  dimnames = list(paste0("id:", 1:10), letters[1:10]))

x.mat <- SpNaBaMatrix(freqmatrix = a)

# Counts calculation ----------------------------------

x.counts <- counts(x.mat)

# Probability calculation -----------------------------

x.probs <- probs(x.counts)

# Estimating epsilon ----------------------------------

x.eps <- epsilon(x.counts, x.probs)

}
\references{
\url{http://species.conabio.gob.mx/}
}
\author{
Enrique Del Callejo Canal (\email{edelcallejoc@gmail.com}),
    based on implemented algortihms in web platform SPECIES (see References).
}
