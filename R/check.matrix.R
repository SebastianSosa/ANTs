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
  if(is.matrix(M)){
    # Check if  argument M a square Matrix
    if(dim(M)[1] != dim(M)[2]){
    stop("Argument M is not a square matrix.")
    }
    else{
      return("M ok")
    }
  }
  
  # Check if argument M is a list of square matrices----------------------
  if (is.list(M) == TRUE) {
      if (sum(unlist(lapply(M, function(x) {
        is.matrix(x) & dim(x)[1] == dim(x)[2]
        }))) != length(M)) {
        stop("Incorrect data input, one of the elements in the list is not a matrix or a square matrix.")
      }
      else{return("M list ok")}
  }

  # If none of the test work then argument M is not a square matrix neither a list of square matrices----------------------
  else{stop("Argument M is not a matrix or a list of matrices")}
  
}