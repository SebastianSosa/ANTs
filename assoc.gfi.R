# Copyright (C) 2018  Sebastian Sosa, Ivan Puga-Gonzalez, Hu Feng He,Peng Zhang, Xiaohua Xie, Cédric Sueur
#
# This file is part of Animal Network Toolkit (ANT).
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

#' @title Generalized affiliation index
#' @description Computes generalized affiliation indices based on a matrix of interactions or associations and a confounding factor.
#' @param M1 a square adjacency matrix representing individual interactions or associations.
#' @param M2 a square adjacency matrix representing individual values of confounding factors.
#' @param fr if \emph{true}, it considers the argument M1 as an adjacency matrix representing interaction frequencies between individuals.
#' Otherwise, it considers the argument M1 as an adjacency matrix representing associations between individuals.
#' @param sym if \emph{true}, it considers the argument M1 as an adjacency matrix representing symmetric interactions/associations.
#' @param erase.diag if \emph{true}, it omits the diagonal of the matrix.
#' @return a square adjacency matrix representing the generalized affiliation index between individuals.
#' @details Generalized affiliation indices allow to control for individual associations by a given confounding factor (such as temporal or spatial overlaps, gregariousness, social unit membership, kinship...).
#' The principle is to perform a Generalized Linear Regression (GLR) on both matrices (one representing the individual interactions/associations and the other one representing the confounding factor)
#' and to use GLR residuals as association indices. For an adjacency matrix representing individual interactions, the GLR belongs to the Poisson family. For an adjacency matrix representing individual associations, the GLR belongs to the Binomial family.
#' High positive values suggest strong associations between two individuals and negative values suggest avoidance between two individuals.
#' @author Sebastian Sosa, Ivan Puga-Gonzalez.
#' @references Whitehead, H., & James, R. (2015). Generalized affiliation indices extract affiliations from social network data. Methods in Ecology and Evolution, 6(7), 836-844.

assoc.gfi <- function(M1, M2, fr = T, sym = F, erase.diag = T) {
  y <- mat.vectorization(M1, sym, erase.diag)
  x <- mat.vectorization(M2, sym, erase.diag)
  if (fr == T) {
    res <- glm(y ~ x, family = poisson(link = "log"))$residuals
  }
  else {
    res <- glm(y ~ x, family = binomial(link = "logit"))$residuals
  }
  new_df <- mat.to.edgl(M1, sym, erase.diag)
  new_df[, 3] <- res
  g_associations_matrix <- edgl_to_matrix(new_df)
  return(g_associations_matrix)
}
