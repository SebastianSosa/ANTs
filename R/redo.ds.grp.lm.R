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

redo.ds.grp.lm <- function(new.perm, gbi, oda, odf, target.metrics, formula, Scan, ctrlf, index, method, model, x, y, qr, singular.ok, contrasts, ...) {
  if (new.perm == 0) {
    # Cumulative permutations
    tmp1 <- redo.perm.ds.grp.cum(oda, Scan = Scan, method = index, control_factor = ctrlf, nperm = attr(odf, "permutation"))

    # Reordering permutations results
    GBI <- tmp1[[1]]
    ASSOC <- tmp1[[2]]

    # Computing target metrics and creating new data frame for the glmm test
    new.odf <- met.all.single.mat(M = ASSOC, df = odf, vec = target.metrics)

    # LM test
    m <- tryCatch(lm(formula = formula, data = new.odf, model = model, method = method, x = x, y = y, qr = qr, singular.ok = singular.ok, contrasts = contrasts, ...), error = identity)

    # If error or warnings recale the function
    if (is(m, "error") | is(m, "warning")) {
      redo.ds.grp.lm(new.perm, gbi, oda, odf, target.metrics, formula, Scan, ctrlf, index, method = method, x = x, y = y, qr = qr, singular.ok = singular.ok, contrasts = contrasts, ...)
    }

    # if no error or warnings
    else {
      # new.perm is equal to the permutation where the error or warning have been found
      new.perm <- attributes(odf)$permutation

      # Result of the function is a list of 3 ellements: 1) permutation index, 2) gbi or controlGBI 3) glmm estimates
      return(list("new.perm" = new.perm, "gbi" = GBI, "model" = summary(m)$coefficients[, 1]))
    }
  }

  else {
    # Permutation to do is equal to the permutation where the error or the warning is found less the permutation already done during previous error or warning.
    nperm <- attributes(odf)$permutation - new.perm

    # Cumulative permutations
    tmp1 <- redo.perm.ds.grp.cum.scd(gbi, method = index, nperm = nperm, control_factor = ctrlf)

    # Reordering permutations results
    GBI <- tmp1[[1]]
    ASSOC <- tmp1[[2]]

    # Computing target metrics and creating new data frame for the glmm test
    new.odf <- met.all.single.mat(ASSOC, odf, target.metrics)

    # Glmm test
    m <- tryCatch(lm(formula = formula, data = new.odf, model = model, method = method, x = x, y = y, qr = qr, singular.ok = singular.ok, contrasts = contrasts, ...), error = identity)

    # Checking error or warnings
    if (is(m, "error") | is(m, "warning")) {
      redo.ds.grp.lm(new.perm, gbi, oda, odf, target.metrics, formula, Scan, ctrlf, index, method = method, x = x, y = y, qr = qr, singular.ok = singular.ok, contrasts = contrasts, ...)
    }
    # if no error or warnings
    else {
      # Result of the function is a list of 3 ellements: 1) permutation index, 2) gbi or controlGBI 3) glmm estimates
      new.perm <- attributes(odf)$permutation
      return(list("new.perm" = new.perm, "gbi" = GBI, "model" = summary(m)$coefficients[, 1]))
    }
  }
}
