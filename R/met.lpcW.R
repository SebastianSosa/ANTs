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

#' @title Wheigthed symetric Laplacian centrality
#' @description Calculate the wheigthed symetric Laplacian centrality for each verteces.

#' @param M a square adjacency matrix.

#' @details Laplacian centrality is the drop in the Laplacian energy of the graph when the vertex is removed
#' This version uses the degrees to calculate laplacian centrality
#' @author Sebastian Sosa, Ivan Puga-Gonzalez.

#' @references REF laplacian !!!!!!!!!!!!!!!!
#' @references Sosa, S. (2018). Social Network Analysis, \emph{in}: Encyclopedia of Animal Cognition and Behavior. Springer.
#' @keywords internal

met.lpcW <- function(M) {
  error_matrix(M)
  diag(M) <- 0
  M <- mat.symetrize(M)

  le0 <- laplacian_energy_degrees(M)

  le_c <- NULL
  for (a in 1:length(M[1, ])) {
    m0 <- M
    m0 <- m0[-a, -a]
    le_a <- laplacian_energy_degrees(m0)
    le_c[a] <- (le0 - le_a) / le0
  }
  return(le_c)
}
