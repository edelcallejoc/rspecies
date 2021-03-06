% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SpNaBaScore.R
\docType{class}
\name{SpNaBaScore-class}
\alias{SpNaBaScore-class}
\alias{initialize,SpNaBaScore-method}
\alias{SpNaBaScore}
\alias{show,SpNaBaScore-method}
\title{Class SpNaBaScore}
\usage{
SpNaBaScore(Apriori, Scx, ...)
}
\arguments{
\item{Apriori}{numeric \code{vector}. See slot section for more details.}

\item{Scx}{numeric \code{matrix}. See slot section for more details.}

\item{...}{argument pass to other methods.}
}
\value{
An object of class \code{SpNaBaScore}. See slot section for more details.
}
\description{
SpNaBaScore is used to store Score's parameters of the Naive Bayes model.
}
\section{Slots}{

\describe{
\item{\code{Apriori}}{\code{vector} with \eqn{S} rows. Each element represents
the apriori probability \eqn{log(P(C)/P(C')}. See
\code{\link[rspecies]{score}}.}

\item{\code{Scx}}{\code{matrix} with \eqn{M} rows and \eqn{S} columns. Each element
represents the Score function \eqn{S(X)}. See
\code{\link[rspecies]{score}}.}
}}

\examples{

# Using SpNaBaMatrix constructor ----------------------

a <- matrix(rbinom(100, 100, runif(100, 0, 0.1)), nrow = 10, ncol = 10,
  dimnames = list(paste0("id:", 1:10), letters[1:10]))

x.mat <- SpNaBaMatrix(freqmatrix = a)

# Counts calculation ----------------------------------

x.counts <- counts(x.mat, target = 1:3)

# Probability calculation -----------------------------

x.probs <- probs(x.counts)

# Estimating epsilon ----------------------------------

x.score <- score(x.probs)

}
\references{
\url{http://species.conabio.gob.mx/}
}
\author{
Enrique Del Callejo Canal (\email{edelcallejoc@gmail.com}),
    based on implemented algortihms in web platform SPECIES (see References).
}
