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

#' @title Calculates the number of triangles in a network
#' @description Calculates the number of triangles in a network.
#' @param M an adjacency matrix.
#' @return  An integer representing the binary global clustering coefficient index of the network.
#' @details It uses an algorithm similar to the Node-iterator-core algorithm.
#' @author Sebastian Sosa, Ivan Puga-Gonzalez.
#' @keywords internal

met.coutTriangles <- function(M) {
  n_tiangles <- c(0)
  for (a in 1:ncol(M)) {
    alters_a_id <- which(M[, a] > 0)
    for (b in a:ncol(M)) {
      if (M[a, b] != 0) {
        for (c in b:ncol(M)) {
          if (M[a, c] != 0 & M[b, c] != 0) {
            n_tiangles <- n_tiangles + 1
          }
        }
      }
    }
  }
  return(n_tiangles)
}
