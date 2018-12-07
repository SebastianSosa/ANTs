# Copyright (C) 2018  Sebastian Sosa, Ivan Puga-Gonzalez, Hu Feng He,Peng Zhang, Xiaohua Xie, Cédric Sueur
#
# This file is part of Animal Network Toolkit Software (ANTs).
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

#' @title Matrix TauKr correlations
#' @description Matrix TauKr correlation. Partial or not, with significant test or not.
#' @param X a square matrix of individual interactions or associations
#' @param Y a square matrix of individual interactions or associations
#' @param Z a square matrix of individual interactions or attributes to use as a control in the matrix correlation between X and Y  (only for partial correlation)
#' @param nperm an integer indicating the number of permutations wanted.
#' @param omitDiag boolean to omit or not the diagonals in the matrices if \emph{TRUE} it does not consider diagonals
#' @details  returns a list of matrices in the same order as is the original folder
#' @author Ivan Puga-Gonzalez, Sebastian Sosa.
#' @references Hemelrijk, C. K. 1990. Models of, and tests for, reciprocity, unidirectional and other social interaction patterns at a group level. Animal Behavior, 39, 1013-1029
#' @references Hemelrijk, C. K. 1990. A matrix partial correlation test used in investigations of reciprocity and other social interaction patterns at a group level. Journal of theoretical Biology, 143, 405-420.

stat.tauKr <- function(X, Y, Z = NULL, nperm = NULL, omitDiag) {
  ############ TauKr matrix correlation ##################
  ### Compute correlation without significance test (no permuations)
  if (is.null(Z) & is.null(nperm)) {
    stopifnot(is.matrix(X), is.matrix(Y))
    stopifnot(dim(X) <= dim(Y))
    result <- stat.tauKrSimple(X, Y, omitDiag = omitDiag)
  }
  ## Compute correlation with significance test (permuations)
  if (is.null(Z) & !is.null(nperm)) {
    stopifnot(is.matrix(X), is.matrix(Y))
    stopifnot(dim(X) <= dim(Y))
    result <- stat.tauKrPermSig(X, Y, nperm = nperm, omitDiag = omitDiag)
  }
  #################TauKr partial Correlations###################################
  ### Compute partial correlation without significance test (no permuations)
  if (!is.null(Z) & is.null(nperm)) {
    stopifnot(is.matrix(X), is.matrix(Y), is.matrix(Z))
    stopifnot(dim(X) <= dim(Y))
    stopifnot(dim(X) <= dim(Z))
    result <- stat.tauKrPartial(X, Y, Z, omitDiag = omitDiag)
  }
  ### Compute partial correlation with significance test (permuations)
  if (!is.null(Z) & !is.null(nperm)) {
    stopifnot(is.matrix(X), is.matrix(Y), is.matrix(Z))
    stopifnot(dim(X) == dim(Y))
    stopifnot(dim(X) == dim(Z))
    result <- stat.tauKrPartialPermSig(X, Y, Z, nperm = nperm, omitDiag = omitDiag)
  }
  return(result)
}
