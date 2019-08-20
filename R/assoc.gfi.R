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
#' @param M1 a square adjacency matrix representing individual interactions or associations. In the latter case associations have to be in the form of a gbi.
#' @param M2 a square adjacency matrix representing individual values of confounding factors.
#' @param fr if \emph{true}, it considers the argument M1 as an adjacency matrix representing interaction frequencies between individuals.
#' Otherwise, it considers the argument M1 as an adjacency matrix representing associations between individuals.
#' @param sym if \emph{true}, it considers the argument M1 as an adjacency matrix representing symmetric interactions/associations.
#' @param erase.diag if \emph{true}, it omits the diagonal of the matrix.
#' @param index a string indicating the association index to compute:
#' \itemize{
#' \item 'sri' for Simple ratio index: \eqn{x/x+yAB+yA+yB}
#' \item 'hw' for Half-weight index: \eqn{x/x+yAB+1/2(yA+yB)}
#' \item 'sr' for Square root index:\eqn{x/sqr((x+yAB+yA)(x+yAB+yB))}
#' }
#' @return a square adjacency matrix representing the generalized affiliation index between individuals.
#' @details Generalized affiliation indices allow to control for individual associations by a given confounding factor (such as temporal or spatial overlaps, gregariousness, social unit membership, kinship...).
#' The principle is to perform a Generalized Linear Regression (GLR) on both matrices (one representing the individual interactions/associations and the other one representing the confounding factor)
#' and to use GLR residuals as association indices. For an adjacency matrix representing individual interactions, the GLR belongs to the Poisson family. For an adjacency matrix representing individual associations, the GLR belongs to the Binomial family.
#' High positive values suggest strong associations between two individuals and negative values suggest avoidance between two individuals.
#' @author Sebastian Sosa, Ivan Puga-Gonzalez.
#' @references Whitehead, H., & James, R. (2015). Generalized affiliation indices extract affiliations from social network data. Methods in Ecology and Evolution, 6(7), 836-844.
#' @example 
#' assoc.gfi(sim.gbi,sim.gbi.att, fr = FALSE)
assoc.gfi <- function(M1, M2, fr = TRUE, sym = FALSE, erase.diag = TRUE, index = "sri") {
  # Is argument M1 an adjacency matrix representing associations between individuals----------------------
  if (fr) {
    # Vectorize Matrix----------------------
    y <- mat.vectorization(M1, sym, erase.diag)
    x <- mat.vectorization(M2, sym, erase.diag)
    res <- glm(y ~ x, family = poisson(link = "log"))$residuals
    
    # Convert residuals into a matrix----------------------
    # Creat edglist of all possible associations/interactions
    new_df <- mat.to.edgl(M1, sym, erase.diag)
    # Add residuals as weigth of associations/interactions
    new_df[, 3] <- res
    # Convert the edgelist into a matrix
    g_associations_matrix <- edgl_to_matrix(new_df)
  }

  # Check if argument M1 is an adjacency matrix representing associations between individuals----------------------
  else {
    # Computing association matrix and extracting numerator and denominator-----------------
    tmp = assoc_mat_full(M1, method = index)
    M1.1 = tmp[[1]]
    M1.2 = tmp[[2]]
    M1.3 = tmp[[3]]
    
    colnames(M1.1)=colnames(M1)
    rownames(M1.1)=colnames(M1)
    
    colnames(M1.2)=colnames(M1)
    rownames(M1.2)=colnames(M1)
    
    colnames(M1.3)=colnames(M1)
    rownames(M1.3)=colnames(M1)

    # Ordering M1.1 and M1.2 according to matrix M2------------------
    M1.2 = M1.2[match(colnames(M1.2), colnames(M2)),match(colnames(M1.2), colnames(M2))]
    M1.3 = M1.3[match(colnames(M1.3), colnames(M2)),match(colnames(M1.3), colnames(M2))]
    
    # Vectorize Matrix----------------------
    y1 = mat.vectorization(M1.2, sym, erase.diag)
    y2 = mat.vectorization(M1.3, sym, erase.diag)
    y = matrix(0, ncol = 2, nrow = length(y1))
    y[,1] = y1
    y[,2] = y2
    x <- mat.vectorization(M2, sym, erase.diag)
    res <- glm(y ~ x, family = binomial(link = "logit"))$residuals
    
    # Convert residuals into a matrix----------------------
    # Create edgelist of all possible associations/interactions
    new_df <- mat.to.edgl(M1.1, sym, erase.diag)
    # Add residuals as weight of associations/interactions
    new_df[, 3] <- res
    # Convert the edgelist into a matrix
    g_associations_matrix <- edgl_to_matrix(new_df, sym = sym)
    g_associations_matrix <- g_associations_matrix
  }
    return(g_associations_matrix)
}
