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

#' @title TauKr standard deviation
#' @description Matrix TauKr correlationsstandard deviation.
#' @author Ivan Puga-Gonzalez, Sebastian Sosa.
#' @keywords internal
tauSD <- function(X, Y, j, omitDiag) {
  # omits the rows and columns where the rows sum is zero in one of the matrices
  # the & in the comparison looks strange but it works whereas a | does not!!?
  zrowX <- apply(X, 1, sum) == 0
  zrowY <- apply(Y, 1, sum) == 0
  X <- X [!zrowX & !zrowY, !zrowX & !zrowY]
  Y <- Y [!zrowX & !zrowY, !zrowX & !zrowY]

  S <- 0
  D <- 1
  rows <- dim(X)[[1]]
  if (is.null(j)) {
    # if not given (re)calculate index matrix for Kendall()
    cols <- dim(X)[[2]]
    j <- matrix(1:rows, nrow = rows, ncol = cols) # index
    if (omitDiag) {
      diag(j) <- NA
      j <- matrix(j[!is.na(j)], nrow = rows - 1, ncol = cols)
    }
  }
  for (i in 1:rows) {
    o <- Kendall(X[i, j[, i]], Y[i, j[, i]])
    S <- S + as.numeric(o[[3]]) # score
    D <- D + as.numeric(o[[4]]) # denominator
  }

  return(list(tau = S / D))
}
