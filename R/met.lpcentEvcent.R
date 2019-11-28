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

#' @title Laplacian centrality
#' @description Calculate the directed weighted or binary Laplacian centrality for each verteces.

#' @param M a square adjacency matrix.
#' @param binary if if \emph{true}  then calculate the directed binary version of Laplacian centrality

#' @details Laplacian centrality is the drop in the Laplacian energy of the graph when the vertex is removed
#' This version uses the eigenvalues to calculate laplacian centrality
#' @author Sebastian Sosa, Ivan Puga-Gonzalez.

#' @references REF laplacian !!!!!!!!!!!!!!!!
#' @references Sosa, S. (2018). Social Network Analysis, \emph{in}: Encyclopedia of Animal Cognition and Behavior. Springer.
#' @keywords internal

met.lpcentEvcent <- function(M, binary = FALSE) {
  error_matrix(M)
  if (binary == TRUE) {
    M <- mat.binaryzation(M)
  }
  laplacian_energy_0 <- met.lpEnergyEigen(M)

  LM <- NULL
  for (i in 1:length(M[, 1])) {
    m0 <- M
    m0 <- m0[-i, -i]
    LM[i] <- (laplacian_energy_0 - met.lpEnergyEigen(m0)) / laplacian_energy_0
  }
  return(list(laplacian_energy = laplacian_energy_0, laplacian_centrality = LM))
}
