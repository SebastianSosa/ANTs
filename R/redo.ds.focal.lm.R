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

#' @title Focal Data stream Recursive function for error found in permutations
#' @description Performs cumulative data stream permutations for gambit of the group until the glmm found no error or warnings
#' @keywords internal
redo.ds.focal.lm <- function(formula, new.perm, gbi, gbi2, oda, odf, target.metrics, focal, ctrl, alters, index, model, method, x, y, qr, singular.ok, contrasts, ...) {
  if (new.perm == 0) {
    nperm <- attributes(odf)$permutation
    # redo.ds.focal.cum function return a list of list, each ones with two elements with the permutations : 1) a data frame of associations; 2) a matrix of associations
    tmp1 <- redo.ds.focal.cum(df = oda, focal = focal, alters = alters, ctrl = ctrl, nperm = nperm, method = index)

    # Reordering permutations results in two new objects : 1) list of data frame of associations; 2) list of a matrices of associations
    GBI <- tmp1[[1]]
    GBI2 <- tmp1[[2]]
    ASSOC <- tmp1[[3]]

    # Computing target metrics and creating new data frame for the glmm test
    odf.tmp <- met.all.single.mat(ASSOC, odf, target.metrics)

    # Glm test
    r <- tryCatch(lm(formula = formula, data = odf.tmp, model = model, method = method, x = x, y = y, qr = qr, singular.ok = singular.ok, contrasts = contrasts, ...), error = identity)

    # If error or warnings recale the function
    if (is(r, "error") | is(r, "warning")) {
      redo.ds.focal.lm(formula, new.perm, gbi = gbi, gbi2 = gbi2, oda, odf, target.metrics, focal, ctrl, alters, index, model, method, x, y, qr, singular.ok, contrasts, ...)
    }

    # if no error or warnings
    else {
      # new.perm is equal to the permutation where the error or warning have been found
      new.perm <- attributes(odf)$permutation

      # Result of the function is a list of 3 ellements: 1) permutation index, 2) data frame of associations 3) glmm estimates
      return(list("new.perm" = new.perm, "GBI" = GBI, "GBI2" = GBI2, "model" = summary(r)$coefficients[, 1]))
    }
  }
  else {
    nperm <- attributes(odf)$permutation - new.perm
    # redo.ds.focal.cum function return a list of list, each ones with two elements with the permutations : 1) a data frame of associations; 2) a matrix of associations
    tmp1 <- redo.perm.ds.focal.cum.scd(GBI = gbi, GBI2 = gbi2, method = index, nperm = nperm)

    # Reordering permutations results in two new objects : 1) list of data frame of associations; 2) list of a matrices of associations
    GBI <- tmp1[[1]]
    GBI2 <- tmp1[[2]]
    ASSOC <- tmp1[[3]]

    # Computing target metrics and creating new data frame for the glmm test
    odf.tmp <- met.all.single.mat(ASSOC, odf, target.metrics)

    # Glmm test
    r <- tryCatch(lm(formula = formula, data = odf.tmp, model = model, method = method, x = x, y = y, qr = qr, singular.ok = singular.ok, contrasts = contrasts, ...), error = identity)

    # If error or warnings recale the function
    if (is(r, "error") | is(r, "warning")) {
      redo.ds.focal.lm(formula, new.perm, gbi = gbi, gbi2 = gbi2, oda, odf, target.metrics, focal, ctrl, alters, index, model, method, x, y, qr, singular.ok, contrasts, ...)
    }

    # if no error or warnings
    else {
      # new.perm is equal to the permutation where the error or warning have been found
      new.perm <- attributes(odf)$permutation

      # Result of the function is a list of 3 ellements: 1) permutation index, 2) data frame of associations 3) glmm estimates
      return(list("new.perm" = new.perm, "GBI" = GBI, "GBI2" = GBI2, "model" = summary(r)$coefficients[, 1]))
    }
  }
}
