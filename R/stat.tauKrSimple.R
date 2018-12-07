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
#' @param omitDiag boolean to omit or not the diagonal from the matrices
#' @return TauKr coefficient value
#' @author Sebastian Sosa, Ivan Puga-Gonzalez.
#' @author Ivan Puga-Gonzalez, Sebastian Sosa.
#' @keywords internal
#' @references Hemelrijk, C. K. 1990. Models of, and tests for, reciprocity, unidirectional and other social interaction patterns at a group level. Animal Behavior, 39, 1013-1029
stat.tauKrSimple <- function(X, Y, omitDiag = omitDiag) {
  return(tauSD(X, Y, NULL, omitDiag))
}
