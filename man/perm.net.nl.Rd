% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/perm.net.nl.R
\name{perm.net.nl}
\alias{perm.net.nl}
\title{Node label permutations with or without random factor(s)}
\usage{
perm.net.nl(ldf, labels, rf = NULL, nperm, progress = TRUE)
}
\arguments{
\item{ldf}{a list of data frames, with each data frame representing a subset of the random factors (e.g. groups and/or periods)}

\item{labels}{a numeric or character vector representing the labels to permute.}

\item{rf}{an integer (column id) or a string (column name) indicating the column holding the factor grouping multiple networks.}

\item{nperm}{an integer indicating the number of permutations wanted.}

\item{progress}{a boolean indicating the visualization of the permutation process.}
}
\value{
A list of data frames. Each data frame is the merger of the input lists of data frames. The first element of the list is the original input data, the other elements are the different permutations.
}
\description{
Performs node label permutations.
}
\details{
Node label permutations for GLMM models need to permute labels within a control factor.
}
\examples{
df=met.strength(sim.m,df=sim.df)
t=perm.net.nl(df,labels='sex',rf=NULL,nperm=1000,progress=TRUE)
}
\references{
Croft, D. P., James, R., & Krause, J. (2008). Exploring animal social networks. Princeton University Press.

Croft, D. P., Madden, J. R., Franks, D. W., & James, R. (2011). Hypothesis testing in animal social networks. Trends in Ecology & Evolution, 26(10), 502-507.

Sosa, S. (2018). Social Network Analysis, \emph{in}: Encyclopedia of Animal Cognition and Behavior. Springer.
}
\author{
Sebastian Sosa, Ivan Puga-Gonzalez.
}
