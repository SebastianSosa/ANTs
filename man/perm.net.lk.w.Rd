% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/perm.net.lk.w.R
\name{perm.net.lk.w}
\alias{perm.net.lk.w}
\title{Links weigths permutations}
\usage{
perm.net.lk.w(M, nperm, progress = TRUE)
}
\arguments{
\item{M}{a square adjacency matrix, or a list of square adjacency matrices.}

\item{nperm}{an integer indicating the number of permutations wanted.}

\item{progress}{a boolean indicating the visualization of the permutation process.}
}
\value{
a list of matrices of length nperm + 1 with the first matrix beeing the original one and the other elements, the permuted ones.
}
\description{
Permute network links weigths while keep same network structure.
}
\details{
Permute network links weigths while keep same network structure (density, modularity, binary global clustering coefficient).
}
\examples{
test = perm.net.lk.w(sim.m, nperm = 2)
test[[1]][test[[1]] > 0] == test[[2]][test[[2]] > 0]
}
\author{
Sebastian Sosa
}
