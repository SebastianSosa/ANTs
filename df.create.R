# Copyright (C) 2018  Sebastian Sosa, Ivan Puga-Gonzalez, Hu Feng He,Peng Zhang, Xiaohua Xie, Cédric Sueur
#
# This file is part of Animal Network Toolkit (ANT).
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

#' @title Creates an empty data frame or a list of empty data frames
#' @description Creates an empty data frame with as many columns as in the corresponding matrix.
#' @param M a square adjacency matrix or a list of square adjacency matrices.
#' @param names a boolean, if \emph{true} then a column is added in the data frame with the names of the matrix columns.
#' @return
#' \itemize{
#' \item if argument M is a matrix, then it creates a single empty data frame with as many rows or columns as in the corresponding matrix.
#' \item if the argument M is a list of matrices, it creates a list of empty data frames with as many rows or columns as in the corresponding matrix in the list.
#' }
#' @author Sebastian Sosa, Ivan Puga-Gonzalez.
#' @examples
#' sim.m
#' df.create(sim.m)

df.create <- function(M, names = T) {
  if (is.matrix(M) == T) {
    df <- df.create.single(M, names)
    return(df)
  }

  if (!is.matrix(M) & is.list(M) == T) {
    if (is.matrix(M[[1]])) { # !!!!!!!!!!!!!!! check if all elements of the lis is a matrix
      df <- lapply(M, df.create.single, names = names)
      return(df)
    }
  }

  else {
    stop("Your data is not a square matrix or a list of square matrices.")
  }
}
