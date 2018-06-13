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

#' @title Binary global clustering coefficient
#' @description Calculates the binary global clustering coefficient.
#' @param M a square adjacency matrix.
#' @return  An integer representing the binary global clustering coefficient index of the network.
#' @details The binary global clustering coefficient is the ratio of closed triplets to all triplets (open and closed) in a network. Triplets are three nodes connected by two or three undirected links (open or closed triplets, respectively).
#' @author Sebastian Sosa, Ivan Puga-Gonzalez
#' @references Sosa, S. (2018). Social Network Analysis, \emph{in}: Encyclopedia of Animal Cognition and Behavior. Springer. 
#' @keywords internal

met.cc=function(M){
  N=ncol(M)
  M=M+t(M)
  M=mat_filter(M,1,1)
  possible_triangles=N*(N-1)*(N-2) / 6
  # Count number of triangles in a graph through EigenTriangle Theorem
  #eigen_values=eigen(M)$value
  #N_triangles=sum(eigen_values^3)/6
  N_triangles=met.coutTriangles(M)
  BGcc=N_triangles/possible_triangles
  return(BGcc)
}


