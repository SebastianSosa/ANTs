# Copyright (C) 2018  Sebastian Sosa, Ivan Puga-Gonzalez, Hu Feng He,Peng Zhang, Xiaohua Xie, Cédric Sueur
#
# This file is part of Animal Network Toolkit (ANT).
#
# ANT is free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# ANT is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

#' @title Matrix vectorization
#' @details Transform an adjacency matrix (symetric or not) into a vector.
#' @param M an adjacency matrix or list of adjacency matrices.
#' @param sym if \emph{true} will vectorize only the lower triangle.
#' @param erase.diag if \emph{true} will not keep the diagonal of the matrix.
#' @description Transform a matrix into a vector.
#' @keywords internal
mat.vectorization <- function(M, sym = F, erase.diag = T) {
  # If argument sym is equal to TRUE----------------------
  if (sym) {
    # If argument erase.diag is equal to TRUE
    if (erase.diag) {
      y <- M[lower.tri(M)]# Extract matrix lower triangle without diagonal
    }
    else {
      y <- M[lower.tri(M, diag = T)]# Extract matrix lower triangle and diagonal
    }
  }
  # If argument sym is equal to FALSE----------------------
  else {
    # If argument erase.diag is equal to TRUE
    if (erase.diag) {
      diag(M) <- NA # Convert diagonals into NA values
      y <- as.vector(M) # Vectorize Matirx
      y <- y[!is.na(y)] # Eraze NA values
    }
    # If argument erase.diag is equal to FALSE
    else {
      y <- as.vector(M)
    }
  }
  return(y)
}
