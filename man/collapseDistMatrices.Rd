% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/collapseDistMatrices.R
\name{collapseDistMatrices}
\alias{collapseDistMatrices}
\title{Collapse/sum matrices using weights}
\usage{
collapseDistMatrices(distMatrixList, weightVector = NULL, NAweight = 0.5)
}
\arguments{
\item{distMatrixList}{list of distances matrices and a vector of
corresponding weights default: all equal weights for each input matrix}

\item{weightVector}{a vector of weights for each matrix. If \code{NULL}, then
each matrix will be weighted equally.}

\item{NAweight}{a custom parameter specifying the score to be assigned for
missing data.}
}
\value{
a single weighted distance matrix
}
\description{
This function collapses/sums the individual matrices from
different feature columns together.

Right now, the weights are not actually implemented (multiplying each matrix
by 1 minus its weight did not give distances that clearly stratified between
true and false patient matches.)

The matrices are simply summed together and then divided by the total number
of matrices fed in to provide a single output distance matrix, where each
index is on a scale of 0 to 1, with 0 being a perfect match (zero distance
between the two rows.)
}
\examples{
# just sum two of the same matrices:
caseIDVector <- data.frame(c("AB-10-1", "AB-10-5", "AB-10_1"))
m1 <- nameDists(caseIDVector)
m2 <- nameDists(caseIDVector)
summedDistMatrix <- collapseDistMatrices(list(m1, m2), c(0.5, 0.5))
}

