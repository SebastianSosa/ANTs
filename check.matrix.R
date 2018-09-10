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

#' @title Matrix ANT check
#' @description check if the input is a square adjacency matrix or a list of squares adjacency matrices.

#' @param M a square adjacency matrix or a list of square adjacency matrices
#' @return Nothing if M is a square adjacency matrix or a list of square adjacencies matrices.

#' @details tests if M is not a data frame and if M is a matrix and if it is a square matrix. If the input is a list then it check for all elements of the list if they are not a data frame and if they are a square adjacency matrix.
#' @author Sebastian Sosa, Ivan Puga-Gonzalez.
#' @keywords internal

check.mat <- function(M) {
  if (is.matrix(M)) {
    if (mat_isSquare(M)) {
      cat("Argument M is a valid matrix.")
      return("mat ok")
    }
    else {
      warning("Argument M is not a valid matrix.")
      return("mat list ok")
    }
  }
  if (is.list(M)) {
    if (all(unlist(lapply(M, function(x) {
      test <- all(c(is.matrix(x), mat_isSquare(x)))
    })))) {
      cat("Argument M is a valid matrix.")
      return(TRUE)
    }
    else {
      warning("Argument M is not a valid matrix.")
      return(FALSE)
    }
  }
  if (is.data.frame(M)) {
    warning("Argument M is a data frame, not a matrix")
    return(FALSE)
  }
  if (is.vector(M)) {
    warning("Argument M is a vector, not a matrix")
    return(FALSE)
  }
}
