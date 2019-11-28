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

#' @title Laplacian energy.
#' @description Calculate the Laplacian energy.

#' @param M a square adjacency matrix.

#' @details Laplacian energy is the sum of squares of the eigenvalues in the Laplacian matrix.
#' This version use the eigenvalues to calculated the Laplacian energy.
#' @author Sebastian Sosa, Ivan Puga-Gonzalez.

#' @references REF laplacian !!!!!!!!!!!!!!!!
#' @references Sosa, S. (2018). Social Network Analysis, \emph{in}: Encyclopedia of Animal Cognition and Behavior. Springer.
#' @keywords internal

met.lpEnergyEigen <- function(M) {
  LM <- mat.lp(M)
  laplacian_energy <- sum(eigen(LM)$values^2)
  return(laplacian_energy)
}
