# Copyright (C) 2018  Sebastian Sosa, Ivan Puga-Gonzalez, Hu Feng He, Xiaohua Xie, Cédric Sueur
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

#' @title Geodesic distances
#' @description Calculates the geodesic distances of a network.

#' @param M a square adjacency matrix, or a list of square adjacency matrices, or an output of ANT functions \emph{stat.ds.grp}, \emph{stat.df.focal}, \emph{stat.net.lk}.
#' @param weighted if \emph{true}, it binarizes the square adjacency matrix M. Geodesic distances and diameter are based only on the presence or absence of edges.
#' @param shortest.weight if \emph{false}, it considers the highest met.strength as the shortest path.
#' @param normalization normalizes the weights of the links i.e. divides them by the average strength of the network. Argument normalization can't be TRUE when argument weighted is FALSE.
#' @param directed if \emph{false}, then it symmetrizes the matrix. Otherwise, it calculates geodesic distances and diameter according to the directionality of the links.
#' @param out if \emph{true}, it considers outgoing ties.
#' @return
#' \itemize{
#' \item a matrix representing the geodesic distances of the network if argument \emph{M} is a square matrix.
#' \item A list of matrices if argument \emph{M} is a list of matrices. Each matrix represents the geodesic distances of the corresponding matrix of the list.
#' }
#' @details Binary network met.density is the ratio of existing links of a network in relation to all potential links.
#' @author  Sebastian Sosa, Ivan Puga-Gonzalez.
#' @references Doreian, P. (1974). On the connectivity of social networks. Journal of Mathematical Sociology, 3(2), 245-258.
#' @references Burt, R. S. (1976). Positions in networks. Social forces, 55(1), 93-122.
#' @references Opsahl, T., Agneessens, F., & Skvoretz, J. (2010). Node centrality in weighted networks: Generalizing degree and shortest paths. Social networks, 32(3), 245-251.
#' @references Sosa, S. (2018). Social Network Analysis, \emph{in}: Encyclopedia of Animal Cognition and Behavior. Springer.
#' @examples
#' met.geodesic(sim.m)

met.geodesic <- function(M, weighted = TRUE, shortest.weight = FALSE, normalization = TRUE, directed = TRUE, out = TRUE) {
  test <- is.matrix(M)
  if (test) {
    result <- met.geodesicDiameter.single(M, weighted, shortest.weight, normalization, directed, out)[[2]]
    return(result)
  }

  else {
    if (!is.null(attributes(M)$ANT)) {
      # Check if argument M originates from ANTs multiples matrices importations
      test1 <- attributes(M)$ANT == "list of matrices obtained through data frames of interactions"
      
      if(test1){
        result <- lapply(M, function(x, weighted, shortest.weight, normalization, directed, out) {
          r <- met.geodesicDiameter.single(x, weighted = weighted, shortest.weight = shortest.weight, normalization = normalization, directed = directed, out = out)[[2]]
        }, weighted = weighted, shortest.weight = shortest.weight, normalization = normalization, directed = directed, out = out)
        return(result)
      }
      else{
        stop("Geodesic distances is a matrix and cannot be analyse on ANTs analyticial protocols ")
      }
    }
    else {
      if (!test & is.list(M)) {
        result <- lapply(M, function(x, weighted, shortest.weight, normalization, directed, out) {
          r <- met.geodesicDiameter.single(x, weighted = weighted, shortest.weight = shortest.weight, normalization = normalization, directed = directed, out = out)[[2]]
        }, weighted = weighted, shortest.weight = shortest.weight, normalization = normalization, directed = directed, out = out)
        return(result)
      }
    }
  }
}
