% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RcppExports.R
\name{mat_rows_sumsBinary}
\alias{mat_rows_sumsBinary}
\title{Binary version of row sum.}
\usage{
mat_rows_sumsBinary(x)
}
\arguments{
\item{m}{a matrix.}
}
\value{
An integer vector. Each elements correspond to the sum of non empty cells according to the corresponding row.
}
\description{
Count the number of non empty cells of each row of an adjacency matrix.
}
\author{
Sebastian Sosa, Ivan Puga-Gonzales.
}
\keyword{internal}
