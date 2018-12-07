# Copyright (C) 2018  Sebastian Sosa, Ivan Puga-Gonzalez, Hu Feng He,Peng Zhang, Xiaohua Xie, Cédric Sueur
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

#' @title Matrix TauKr correlations
#' @description Matrix TauKr correlation with significant test.
#' @param X a square matrix of individual interactions or associations
#' @param Y a square matrix of individual interactions or attributes
#' @param nperm number of permutations.
#' @param omitDiag boolean to omit or not the diagonal from the matrices
#' @return a list with the 1) TauKr coefficient value, p-value on the right 2) p-value on the left
#' @author Sebastian Sosa, Ivan Puga-Gonzalez.
#' @keywords internal
#' @references Hemelrijk, C. K. 1990. Models of, and tests for, reciprocity, unidirectional and other social interaction patterns at a group level. Animal Behavior, 39, 1013-1029
stat.tauKrPermSig <- function(X, Y, nperm = nperm, omitDiag = TRUE) {
  ## Compute statistic original data
  o <- tauSD(X, Y, NULL, omitDiag)
  le <- 0
  ge <- 0
  if (is.nan(o$tau)) {
    return(list(tau = NA, pR = NA, pL = NA))
  }
  else {
    ## do permutations and compute statistic of each permutation
    for (n in 1:nperm) {
      pp <- sample(1:dim(X)[[2]])
      p <- tauSD(X[pp, pp], Y, NULL, omitDiag)
      if (p$tau >= o$tau) ge <- ge + 1
      if (p$tau <= o$tau) le <- le + 1
    }
    ## calculate p-values and return result
    return(list(
      tau = o$tau,
      pR = (ge + 1) / (nperm + 1),
      pL = (le + 1) / (nperm + 1)
    ))
  }
}
