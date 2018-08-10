# Copyright (C) 2018  Sebastian Sosa, Ivan Puga-Gonzalez, Hu Feng He,Peng Zhang, Xiaohua Xie, Cédric Sueur
#
# This file is part of Animal Network Toolkit (ANT).
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

#' @title Data stream permutation for association data
#' @description Pre-network permutation on association data for group fellow data collection protocol. The data frame must have a column named 'ID'.
#' @param df A data frame.The data frame must have a column named 'ID'.
#' @param scan  an integer indicating the column of scans of individual associations.
#' @param ctrlf A confounding factor by which to control group associations.
#' @param method Which type of index of associations to calculate:
#' \itemize{
#' \item 'sri' for Simple ratio index: \eqn{x \div x+yAB+yA+yB}
#' \item 'hw' for Half-weight index: \eqn{x/x+yAB+1/2(yA+yB)}
#' \item 'sr' for Square root index:\eqn{x/sqr((x+yAB+yA)(x+yAB+yB))}
#' }
#' @param perm number of permutations to perform.
#' @param progress a boolean indicating if the permutation process must be visible.
#' @return list of square association index matrices. The first element of the list is the non-permuted association index matrix.
#' @details Data stream permutation is a pre-network permutation approach. It is used on association data based on the gambit of the group.
#' @author Sebastian Sosa, Ivan Puga-Gonzalez.
#' @references Whitehead, H. A. L. (1997). Analysing animal social structure. Animal behaviour, 53(5), 1053-1067.
#' @references Farine, D. R. (2017). A guide to null models for animal social network analysis. Methods in Ecology and Evolution.
#' @references Sosa, S. (2018). Social Network Analysis, \emph{in}: Encyclopedia of Animal Cognition and Behavior. Springer.
#' @examples
#' head(sim.grp)
#' t=perm.ds.grp(df=sim.grp,scan='location',ctrlf='time',perm=10,,method='sri')

perm.ds.grp <- function(df, scan, ctrlf = NULL, method = "sri", perm, progress = T) {
  test <- check.df(df)
  if (test == "df ok") {
    result <- perm.dataStream.group(df, scan = scan, control_factor = ctrlf, method = method, perm = perm, progress = progress)
    attr(result, "ANT") <- "ANT data stream group sampling single matrix"
    attr(result, "scan") <- scan
    attr(result, "ctrlf") <- ctrlf
    attr(result, "method") <- method
    return(result)
  }
  if (test == "df list ok") {
    result <- lapply(df, perm.dataStream.group, scan = scan, control_factor = ctrlf, method = method, perm = perm, progress = progress)
    attr(result, "ANT") <- "ANT data stream group sampling multiple matrices"
    attr(result, "scan") <- scan
    attr(result, "ctrlf") <- ctrlf
    attr(result, "method") <- method
    return(result)
  }
}
