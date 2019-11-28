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

#' @title Alters sum or average of met.degree or met.strength.
#' @param M a square adjacency matrix.
#' @param i The ego in which we want to calculate his aleer met.degree.
#' @param av a boolean, if TRUE compute the sum of alters before making alter mean.
#' @details calculates average or sum of alters' degrees. By default it calculates the sum of the degrees of alters.
#' @return Vector fo Alters sum or average of met.degree or met.strength values for each verteces.
#' @description Computer the degre of a specific node.
#' @author Sebastian Sosa, Ivan Puga-Gonzalez
#' @keywords internal

met.alterDegree <- function(M, i, av = FALSE, binary = FALSE) {
  nodes_degree <- mat_cols_sums(M)
  if (nodes_degree[i] != 0) {
    if (binary == TRUE) {
      M <- mat_filter(M, 1, 1)
    }
    alters_id <- which(M[, i] > 0)
    alters_d <- nodes_degree[alters_id]
    if (av == FALSE) {
      alters_metric <- sum(alters_d)
    }
    else {
      alters_metric <- mean(alters_d)
    }
  }
  else {
    alters_d <- 0
  }
  return(alters_d)
}
