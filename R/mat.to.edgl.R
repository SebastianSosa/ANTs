# Copyright (C) 2018  Sebastian Sosa, Ivan Puga-Gonzalez, Hu Feng He,Peng Zhang, Xiaohua Xie, Cédric Sueur
#
# This file is part of Animal Network Toolkit Software (ANTs).
#
# ANT is a free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# ANT is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

#' @title Matrix to edge list
#' @description Converts a square adjacency matrix into a data frame of three columns representing an edge list. Columns are: actor, receiver and weight.

#' @param M a square adjacency matrix.
#' @param sym if \emph{TRUE}, it extracts the lower triangle of the matrix only.
#' @param erase.diag if \emph{TRUE}, it omits diagonals.
#' @author Sebastian Sosa, Ivan Puga-Gonzalez.
#' @references Sosa, S. (2018). Social Network Analysis, \emph{in}: Encyclopedia of Animal Cognition and Behavior. Springer.
#' @examples
#' mat.to.edgl(sim.m, sym = FALSE, erase.diag = TRUE)

mat.to.edgl <- function(M, sym = FALSE, erase.diag = TRUE) {
  # If argument sym is equal to TRUE----------------------
  if (sym) {
    # If argument erase.diag is equal to TRUE
    if (erase.diag == TRUE) {
      test <- lower.tri(M) # Extract matrix lower triangle
    }
    else {
      test <- lower.tri(M, diag = TRUE) # Extract matrix lower triangle and diagonal
    }
    # Extract matrix cells
    weight <- M[test]
    # Extract cells ids
    tmp <- which(test, arr.ind = TRUE)
    # Create an edgelist if actor, receiver and interactions weights
    DF <- cbind("from" = colnames(M)[tmp[, 1]], "to" = colnames(M)[tmp[, 2]])
    DF <- data.frame(DF, weight)
  }
  # If argument sym is equal to FALSE----------------------
  else {
    if(is.null(colnames(M))){stop("Argument M doesn't have column names.")}
    # Create a vector of actors
    from <- rep(c(colnames(M)), dim(M)[2])
    # Create a vector of receivers
    to <- rep(c(colnames(M)), each = dim(M)[1])
    # Extract matrix cells
    weight <- as.vector(M)
    # Create a data frame of those vectors
    DF <- data.frame(from, to, weight)
    # if argument erase diag is equal to TRUE
    if (erase.diag == TRUE) {
      diagonals <- which(DF$from == DF$to) # Remove case where actor is equal to receiver
      DF <- DF[-c(diagonals), ]
    }
  }
  return(DF)
}
