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

#' @title Centralisation index
#' @description Compute network Centralisation index
#' @param M a square adjacency matrix or a list of squares adjacencymatrices
#' @details Centralisation index of a network is based on eigenvector centrality.
#' @return numeric vector
#' @author Sebastian Sosa, Ivan Puga-Gonzalez
#' @references Pasquaretta, C., Levé, M., Claidiere, N., Van De Waal, E., Whiten, A., MacIntosh, A. J., ... & Crofoot, M. C. (2014). Social networks in primates: smart and tolerant species have more efficient networks. Scientific reports, 4, 7600.
#' @keywords internal

met.ci.single <- function(M) {
  # Real network index calculation
  eigen <- met.eigen(M)
  eigen.max <- eigen[which.max(eigen)]
  num <- sum(eigen.max - eigen)

  # New mat simulation of a star
  m <- matrix(rep(0, ncol(M) * nrow(M)), ncol = ncol(M), nrow = nrow(M))
  e <- sample(c(1:nrow(m)), 1)
  m[e, ] <- rep(1, ncol(m))
  m[, e] <- rep(1, nrow(m))
  diag(m) <- 0

  # Index calculation on simulated network
  eigen2 <- met.eigen(m)
  eigen2.max <- eigen2[which.max(eigen2)]
  den <- max(eigen2.max - eigen2)

  # Centralisation index
  result <- 100 * (num / den)
  names(result) <- "CI"
  return(result)
}
