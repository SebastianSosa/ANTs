# Copyright (C) 2018  Sebastian Sosa, Ivan Puga-Gonzalez, Hu Feng He,Peng Zhang, Xiaohua Xie, Cédric Sueur
#
# This file is part of Animal Network Toolkit (ANT).
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
#' @description Partial Matrix TauKr correlation with significant test.
#' @param X a square matrix of individual interactions or associations
#' @param Y a square matrix of individual interactions or attributes
#' @param Z a square matrix of individual interactions or attributes to use as a control in the matrix correlation between X and Y
#' @param nperm number of permutations.
#' @param omitDiag boolean to omit or not the diagonal from the matrices
#' @return a list with the correlation coefficients 1) TauKrxy, 2) TauKrxz, 3) TauKryz, 4) TauKrxyz; p-values of the TauKrxyz coefficient value 5) on the right, 6) on the left
#' @author Ivan Puga-Gonzalez, Sebastian Sosa.
#' @keywords internal
#' @references Hemelrijk, C. K. 1990. A matrix partial correlation test used in investigations of reciprocity and other social interaction patterns at a group level. Journal of theoretical Biology, 143, 405-420. 
stat.tauKrPartialPermSig <- function (X, Y, Z, nperm, omitDiag = T)
{
  PartialTauKr <- stat.tauKrPartial (X, Y, Z, omitDiag)
  le <- 0
  ge <- 0
  if (is.nan(PartialTauKr$Tauxyz)) {
    return(list(Tauxy = PartialTauKr$Tauxy,
                Tauxz = PartialTauKr$Tauxz,
                Tauyz = PartialTauKr$Tauyz,
                Tauxyz = NA, pR = NA, pL = NA))
  }
  else{
    for (n in 1:nperm) {
      PP <- sample(1:dim(X)[[2]])
      if (n %% 2 != 0){
        P <- stat.tauKrPartial(X[PP,PP], Y, Z, omitDiag)
        X <- X[PP,PP]
      }
      if(gtools::even(n)){
        P <- stat.tauKrPartial(X, Y[PP,PP], Z, omitDiag)
        Y <- Y[PP,PP]
      }
      if (P$Tauxyz >= PartialTauKr$Tauxyz) ge <- ge + 1
      if (P$Tauxyz <= PartialTauKr$Tauxyz) le <- le + 1
    }
    return(list(Tauxy = PartialTauKr$Tauxy,
                Tauxz = PartialTauKr$Tauxz,
                Tauyz = PartialTauKr$Tauyz,
                Tauxyz = PartialTauKr$Tauxyz,
                pR = (ge + 1) / (nperm + 1),
                pL = (le + 1) / (nperm + 1)))
  }
  
}