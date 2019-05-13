# Copyright (C) 2018  Sebastian Sosa, Ivan Puga-Gonzalez, Hu Feng He,Peng Zhang, Xiaohua Xie, Cédric Sueur
#
# This file is part of Animal Network Toolkit Software (ANTs).
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
    if (!is.null(attributes(M)$ANT)) {
      # Check if argument M originates from a single network protocol
      test1 <- attributes(M)$ANT == "ANT data stream group sampling single matrix"
      test2 <- attributes(M)$ANT == "ANT data stream focal sampling single matrix"
      test3 <- attributes(M)$ANT == "ANT link permutations single matrix"
      
      # Check if argument M is issue from a multiples network protocol
      test4 <- attributes(M)$ANT == "ANT data stream group sampling multiple matrices"
      test5 <- attributes(M)$ANT == "ANT data stream focal sampling multiple matrices"
      test6 <- attributes(M)$ANT == "ANT link permutations multiple matrices"
      
      test7 <- attributes(M)$ANT == "list of matrices obtained through data frames of interactions"
      # If argumpent M originates from a single network protocol, we work on a list of matrices
      if (test1 | test2 | test3) {
        if (sum(unlist(lapply(M, function(x) {
          is.matrix(x) & dim(x)[1] == dim(x)[2]
        }))) != length(M)) {
          stop("Incorrect data input, one of the elements in the list is not a matrix or a square matrix.")
        }
        else{return("M list ok")}
      }
      
      # If argumpent M is originates from multiples network protocol with node links permutations, we work on a list of list of Matrices
      if (test3 | test4 | test5){
        if(all(unlist(lapply(M, function(x){
          sum(unlist(lapply(x, function(y){
            is.matrix(y) & dim(y)[1] == dim(y)[2]
          }))) != length(x)})
        )) == FALSE){
          return("M list ok")
        }
        else{
          stop("Incorrect data input, one of the elements in the list is not a matrix or a square matrix.")
        }
      }
      
      # if argument M is originates from  ANTs  importation function
      if(test7){
        if (sum(unlist(lapply(M, function(x) {
          is.matrix(x) & dim(x)[1] == dim(x)[2]
        }))) != length(M)) {
          stop("Incorrect data input, one of the elements in the list is not a matrix or a square matrix.")
        }
        else{return("M list ok")}
      }
    }
    else{
      if (sum(unlist(lapply(M, function(x) {
        is.matrix(x) & dim(x)[1] == dim(x)[2]
      }))) != length(M)) {
        stop("Incorrect data input, one of the elements in the list is not a matrix or a square matrix.")
      }
      else{return("M list ok")}
    }
  }
  # If none of the test work then argument M is not a square matrix neither a list of square matrices----------------------
  else{stop("Argument M is not a matrix or a list of matrices")}
}