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

#' @title Laplacian matrix
#' @description Create the laplacian matrix of a network.

#' @param M a square adjacency matrix.

#' @details  Laplacian matrix is the difference between the degrees matrix and the adjacency matrix.
#' @author Sebastian Sosa, Ivan Puga-Gonzalez.

#' @references REF laplacian !!!!!!!!!!!!!!!!
#' @references Sosa, S. (2018). Social Network Analysis, \emph{in}: Encyclopedia of Animal Cognition and Behavior. Springer. 
#' @keywords internal


mat.lp<-function(M){
  # Degrees matrix
  m0<-M
  m0[m0>0]=0
  met.degree=mat_cols_sums(M)
  diag(m0)=met.degree
  # Laplacian matrix
  LM<-m0-M
  return(LM)
}
