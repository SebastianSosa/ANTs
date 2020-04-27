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

#' @title P-value
#' @description Calculation of the p-value based on a posterior distribution through data permutations.
#' @param metrics  an integer vector of permuted statistical values of interest
#' @param stat.hist if \emph{true}, a stat.hist of the posterior distribution is ploted. The observed metric is also ploted.
#' @return
#' \itemize{
#' \item  An integer vector of left and rigth p-values, if \emph{metrics} is an integer vector and if \emph{stat.hist} is \emph{false}.
#' \item  An integer vector of left and rigth p-values and an histogram of the posterior distribution, if \emph{metrics} is an integer vector and if \emph{stat.hist} is \emph{false}.
#' }
#' @details Due to data dependency, a valuable p-value in social networking has to be computed through a null model approach.
#' @author Sebastian Sosa, Ivan Puga-Gonzalez.
#' @references Farine, D. R. (2017). A guide to null models for animal social network analysis. Methods in Ecology and Evolution.
#' @references Sosa, S. (2018). Social Network Analysis, \emph{in}: Encyclopedia of Animal Cognition and Behavior. Springer.
#' @keywords internal


stat.p <- function(metrics, stat.hist = FALSE) {
  if (is.vector(metrics)) {
    # First value is the one of the real data
    v <- metrics[1]
    # Other ones are the permuted ones
    v_perm <- metrics[-1]
  }
  if (is.data.frame(metrics)) {
    v <- metrics[1, ]
    v_perm <- metrics[-1, ]
  }

  # Computing two-side p-values
  p_valuevalue_left_side <- sum(v_perm <= v) / length(v_perm)
  p_valuevalue_right_side <- sum(v_perm >= v) / length(v_perm)
  p_one_side <- c(p_valuevalue_left_side,p_valuevalue_right_side)[which.min(c(p_valuevalue_left_side,p_valuevalue_right_side))]*2

  # Posterior distribution
  if (stat.hist == TRUE) {
    histo <- stat.hist(v, v_perm)
    p <- c("p-value_rigth_side" = p_valuevalue_left_side, "p-value_left_side" = p_valuevalue_right_side, "p-value_one_side" = p_one_side)
    return(list("p-values" = p, "stat.hist" = histo))
  }
  else {
    return(p = c("p-value_rigth_side" = p_valuevalue_left_side, "p-value_left_side" = p_valuevalue_right_side, "p-value_one_side" = p_one_side))
  }
}
