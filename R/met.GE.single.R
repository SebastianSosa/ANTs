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

#' @title Global efficiency
#' @description Calculates the global efficiency of a network.

#' @param m a square adjacency matrix.
#' @param weighted if \emph{true}, it binarizes the square adjacency matrix M. Geodesic distances and diameter are based only on the presence or absence of edges.
#' @param shortest.weight if \emph{false}, it considers the higher met.strength as the shortest path.
#' @param normalization normalizes the weigths of the links i.e. divides them by the average strength of the network.
#' @param directed if \emph{false}, it symmetrizes the matrix. Otherwise, it calculates geodesic distances and diameter according to the directionality of the links.
#' @param out if \emph{true}, it considers
#' @return An interger representing the global efficiency.


#' @details The global efficiency provides a measure of how efficiently information is exchanged in a network. It can be calculated in binary or weighted networks, and for undirected or directed networks. Depending on the problematic, care is needed for the choice of calculation method (binary or weighted, directed or undirected, and using the lowest or the highest met.strength as shortest path).
#' @author Sebastian Sosa, Ivan Puga-Gonzalez

#' @references Sosa, S. (2018). Social Network Analysis, \emph{in}: Encyclopedia of Animal Cognition and Behavior. Springer.

#' @keywords internal
#'

met.ge.single <- function(m, weighted = TRUE, shortest.weight = FALSE, normalization = TRUE, directed = TRUE, out = TRUE) {
  if (weighted == FALSE) {
    m <- mat_filter(m, 1, 1)
  }
  if (shortest.weight == FALSE) {
    # opshal method
    avg_strength <- mean(m)
    if (normalization == TRUE) {
      m <- m / avg_strength
    }
    m <- 1 / m
    m[is.infinite(m)] <- 0
  }
  if (directed == FALSE) {
    m <- m + t(m)
    result <- metric_global_shortestPath(m)[[1]]
    diag(result) <- 0
    s <- sum(result)
    GE <- s / (ncol(result) * (ncol(result) - 1))
    return(GE)
  }
  else {
    if (out == TRUE) {
      result <- metric_global_shortestPath(m)[[1]]
      diag(result) <- 0
      s <- sum(result)
      GE <- s / (ncol(result) * (ncol(result) - 1))
      return(GE)
    }
    else {
      result <- metric_global_shortestPath(t(m))[[1]]
      diag(result) <- 0
      s <- sum(result)
      GE <- s / (ncol(result) * (ncol(result) - 1))
      return(GE)
    }
  }
}
