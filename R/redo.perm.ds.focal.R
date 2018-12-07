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

#' @title Focal Data stream Recursive function for error found in permutations
#' @description Performs cumulative data stream permutations for group fellow until the glmm found no error or warnings
#' @keywords internal
redo.ds.focal.glmm <- function(family, formula, new.perm, gbi, gbi2, oda, odf, target.metrics, focal, ctrl, alters, index, fam, ...) {
  if (new.perm == 0) {
    nperm <- attributes(odf)$permutation
    # redo.ds.focal.cum function return a list of list, each ones with two elements with the permutations : 1) a data frame of associations; 2) a matrix of associations
    tmp1 <- lapply(oda, redo.ds.focal.cum, focal = focal, alters = alters, ctrl = ctrl, nperm = nperm, method = index)

    # Reordering permutations results in two new objects : 1) list of data frame of associations; 2) list of a matrices of associations
    GBI <- list()
    GBI2 <- list()
    ASSOC <- list()
    for (a in 1:length(tmp1)) {
      GBI[[a]] <- tmp1[[a]][[1]]
      GBI2[[a]] <- tmp1[[a]][[2]]
      ASSOC[[a]] <- tmp1[[a]][[3]]
    }

    # Computing target metrics and creating new data frame for the glmm test
    odf.tmp <- met.all(ASSOC, odf, target.metrics)

    # Glmm test
    if (fam == "gaussian") {
      r <- tryCatch(lme4::lmer(formula = formula, data = odf.tmp, ...), error = identity)
    }
    # if(family=='nb'){r=tryCatch(lme4::glmer.nb(formula=formula, data = new.odf,family=family,...), error=identity)}
    if (fam != "gaussian") {
      r <- tryCatch(lme4::glmer(formula = formula, data = odf.tmp, family = family, ...), error = identity)
    }

    # Checking error or warnings
    if (isS4(r)) {
      r2=with(r@optinfo$derivs,solve(Hessian,gradient))
      if(max(abs(r2))<0.001){test=TRUE}
      else{
        test <- c(!is(r, "error"), !is(r, "warning"),r@optinfo$conv$opt == 0, length(r@optinfo$conv$lme4$messages) == 0, length(r@optinfo$warnings) == 0)
      }
    }
    if (is(r, "error")) {
      test <- FALSE
    }
    if (is(r, "warning")) {
      test <- FALSE
    }

    # If error or warnings recale the function
    if (all(test) != TRUE) {
      redo.ds.focal.glmm(family, formula, new.perm, gbi, gbi2, oda, odf, target.metrics, focal, ctrl, alters, index, fam, ...)
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
    tmp1 <- lapply(seq_along(gbi), function(i, gbi, gbi2, method, nperm) {
      r <- redo.perm.ds.focal.cum.scd(GBI = gbi[[i]], GBI2 = gbi2[[i]], method = method, nperm = nperm)
    }, gbi = gbi, gbi2 = gbi2, method = index, nperm = nperm)

    # Reordering permutations results in two new objects : 1) list of data frame of associations; 2) list of a matrices of associations
    GBI <- list()
    GBI2 <- list()
    ASSOC <- list()
    for (a in 1:length(tmp1)) {
      GBI[[a]] <- tmp1[[a]][[1]]
      GBI2[[a]] <- tmp1[[a]][[2]]
      ASSOC[[a]] <- tmp1[[a]][[3]]
    }

    # Computing target metrics and creating new data frame for the glmm test
    odf.tmp <- met.all

    # Computing target metrics and creating new data frame for the glmm test
    odf.tmp <- met.all(ASSOC, odf, target.metrics)

    # Glmm test
    if (fam == "gaussian") {
      r <- tryCatch(lme4::lmer(formula = formula, data = odf.tmp, ...), error = identity)
    }
    # if(family=='nb'){r=tryCatch(lme4::glmer.nb(formula=formula, data = new.odf,family=family,...), error=identity)}
    if (fam != "gaussian") {
      r <- tryCatch(lme4::glmer(formula = formula, data = odf.tmp, family = family, ...), error = identity)
    }

    # Checking error or warnings
    if (isS4(r)) {
      r2=with(r@optinfo$derivs,solve(Hessian,gradient))
      if(max(abs(r2))<0.001){test=TRUE}
      else{
        test <- c(!is(r, "error"), !is(r, "warning"),r@optinfo$conv$opt == 0, length(r@optinfo$conv$lme4$messages) == 0, length(r@optinfo$warnings) == 0)
      }
    }
    if (is(r, "error")) {
      test <- FALSE
    }
    if (is(r, "warning")) {
      test <- FALSE
    }

    # If error or warnings recale the function
    if (all(test) != TRUE) {
      redo.ds.focal.glmm(family, formula, new.perm, gbi, gbi2, oda, odf, target.metrics, focal, ctrl, alters, fam, ...)
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
