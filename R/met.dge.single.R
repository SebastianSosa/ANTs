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

#' @title Average Dyadic Efficiency
#' @description Calculates the average dyadic efficiency of a network.

#' @param m a square adjacency matrix.
#' @param weighted if \emph{true}, it binarizes the square adjacency matrix M. Geodesic distances and diameter are based only on the presence or absence of edges.
#' @param shortest.weight if \emph{false}, it considers the higher met.strength as the shortest path.
#' @param normalization normalizes the weigths of the links i.e. divides them by the average strength of the network.
#' @param directed if \emph{false}, it symmetrizes the matrix. Otherwise, it calculates geodesic distances and diameter according to the directionality of the links.
#' @return An interger representing the average dyadic efficiency.


#' @details The average dyadic efficiency provides a measure of how efficiently information is exchanged in a network. It can be calculated in binary or weighted networks, undirected or directed networks, normalized or not, and through the strongest or the weakest links. Depending on the problematic, care is needed for the choice of calculation method.
#' @author Sebastian Sosaez

#' @references Pasquaretta, C., Levé, M., Claidiere, N., Van De Waal, E., Whiten, A., MacIntosh, A. J., ... & Crofoot, M. C. (2014). Social networks in primates: smart and tolerant species have more efficient networks. Scientific reports, 4, 7600.

#' @keywords internal

met.dge.single <- function(m, weighted = TRUE, shortest.weight = FALSE, normalization = TRUE, directed = TRUE) {
  if (directed == FALSE){
    m <- m + t(m)
  }
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
    result <- metric_global_shortestPath(m)[[1]]
    diag(result) <- 0
    GE <- s / (ncol(result) * (ncol(result) - 1))
    return(GE)
  }
  else {
    result <- metric_global_shortestPath(m)[[1]]
    diag(result) <- 0
    s <- sum(result)
    GE <- s / (ncol(result) * (ncol(result) - 1))
    return(GE)
  }
}
