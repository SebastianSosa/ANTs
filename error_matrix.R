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

#' @title Errors related to matrices
#' @description check if the input is a square adjacency matrix or a list of squares adjacency matrices.

#' @param M a square adjacency matrix or a list of square adjacency matrices
#' @return Nothing if M is a square adjacency matrix or a list of square adjacencies matrices.

#' @details tests if M is not a data frame and if M is a matrix and if it is a square matrix. If the input is a list then it check for all elements of the list if they are not a data frame and if they are a square adjacency matrix.
#' @author Sebastian Sosa, Ivan Puga-Gonzalez.
#' @keywords internal

error_matrix <- function(M) {
  is.square <- function(M) {
    dim(M)[1] == dim(M)[2]
  }
  is.mat <- function(M) {
    is.matrix(M)
  }
  if (is.matrix(M)) {
    if (is.square(M) == F) {
      stop("Argument is not a square matrix.")
    }
  }
  else {
    if (is.data.frame(M) == T) {
      stop("Argument is a data frame, not a matrix.")
    }
    if (is.vector(M) == T) {
      stop("Argument is a vector, not a matrix")
    }
    if (is.list(M) == T) {
      if (sum(unlist(lapply(M, function(x) {
        is.matrix(x) & is.square(x) == T
      }))) != length(M)) {
        stop("Incorrect data input, one of the elements in the list is not a matrix or a square matrix.")
      }
    }
  }
}
