# Copyright (C) 2018  Sebastian Sosa, Ivan Puga-Gonzalez, Hu Feng He, Xiaohua Xie, Cédric Sueur
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

#' @title Geodesic distances and diameter
#' @description Calculates 1) Geodesic Distances and 2) network diameter .

#' @param m a square adjacency matrix.
#' @param weighted if \emph{FALSE}, it binarizes the square adjacency matrix M. Geodesic distances and diameter are based only on the presence or absence of edges.
#' @param shortest.weight if \emph{false}, it considers the higher met.strength as the shortest path.
#' @param normalization normalizes the weigths of the links i.e. divides them by the average strength of the network. Argument normalization can't be TRUE when  argument weighted is FALSE.
#' @param directed if \emph{false}, then it symmetrizes the matrix. Otherwise, it calculates geodesic distances and diameter according to the directionality of the links.
#' @param out if \emph{true}, it considers
#' @return A list of two elements:
#' \itemize{
#' \item The first element is the diameter of the network according to the option specified (weigthed or not, directed or not, throught shortest weights or stronger weights)
#' \item The second element is the geodesic distances between all nodes according to the option specified (weigthed or not, directed or not, throught shortest weights or stronger weights)
#' }

#' @details Binary network met.density is the ratio of existing links of a network in relation to all potential links.
#' @author  Sebastian Sosa, Ivan Puga-Gonzalez.

#' @references Sosa, S. (2018). Social Network Analysis, \emph{in}: Encyclopedia of Animal Cognition and Behavior. Springer.
#' @keywords internal
#'
met.geodesicDiameter.single <- function(m, weighted = TRUE, shortest.weight = FALSE, normalization = TRUE, directed = TRUE, out = TRUE) {
  if (weighted == FALSE & normalization == TRUE) {
    stop("Argument normalization can't be TRUE when  argument weighted is FALSE.")
  }
  if (directed == FALSE){
    m <- m + t(m)
  }
  if (weighted == FALSE) {
    m <- mat_filter(m, 1, 1)
  }
  if (shortest.weight == FALSE) {
    # opshal method
    if (normalization) {
      number.of.links = sum(m>0)
      avg_strength <- sum(m) / number.of.links
      m <- m / avg_strength
    }
    
    m <- 1 / m
    m[is.infinite(m)] <- 0
  }
  if (directed == FALSE) {
    results <- metric_global_shortestPath(m)
    r1 <- results[[2]]
    r2 <- results[[1]]
    diag(r2) <- 0
    return(list("diameter" = r1, "geodesic" = r2))
  }
  else {
    if (out) {
      results <- metric_global_shortestPath(m)
      r1 <- results[[2]]
      r2 <- results[[1]]
      diag(r2) <- 0
      return(list("diameter" = r1, "geodesic" = r2))
    }
    else {
      results <- metric_global_shortestPath(t(m))
      r1 <- results[[2]]
      r2 <- results[[1]]
      diag(r2) <- 0
      return(list("diameter" = r1, "geodesic" = r2))
    }
  }
}
