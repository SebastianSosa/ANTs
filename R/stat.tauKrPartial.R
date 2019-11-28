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

#' @title Partial matrix TauKr correlations
#' @description Partial Matrix TauKr correlation without significant test.
#' @param X a square matrix of individual interactions or associations
#' @param Y a square matrix of individual interactions or attributes
#' @param Z a square matrix of individual interactions or attributes to use as a control in the matrix correlation between X and Y
#' @param omitDiag boolean to omit or not the diagonal from the matrices
#' @return a list with the correlation coefficients 1) TauKrxy, 2) TauKrxz, 3) TauKryz, 4) TauKrxyz
#' @author Ivan Puga-Gonzalez, Sebastian Sosa.
#' @keywords internal
#' @references Hemelrijk, C. K. 1990. A matrix partial correlation test used in investigations of reciprocity and other social interaction patterns at a group level. Journal of theoretical Biology, 143, 405-420.
stat.tauKrPartial <- function(X, Y, Z, omitDiag = TRUE) {
  ## calculate statistic
  Tauxyz <- 0
  Txy <- tauSD(X, Y, NULL, omitDiag)
  Txz <- tauSD(X, Z, NULL, omitDiag)
  Tyz <- tauSD(Y, Z, NULL, omitDiag)
  # if nan, return NA
  if (is.nan(Txy$tau) || is.nan(Txz$tau) || is.nan(Tyz$tau)) {
    return(list(Tauxy = NA, Tauxz = NA, Tauyz = NA, Tauxyz = NA))
  }
  else {
    Tauxyz <- (Txy$tau - (Txz$tau * Tyz$tau)) / (sqrt((1 - (Txz$tau^2))) * sqrt((1 - (Tyz$tau^2))))
    return(list(Tauxy = Txy$tau, Tauxz = Txz$tau, Tauyz = Tyz$tau, Tauxyz = Tauxyz))
  }
}
