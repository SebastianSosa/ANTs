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

#' @title Node label permutations
#' @description Performs node label permutations

#' @param df a dataframe or a list of data frames.
#' @param labels column names or numbers in which to permute the data
#' @param nperm Number of permutations.
#' @param progress a boolean indicating if the permutation process has to be visible.
#' @return A list of data frames with the specified column permuted.

#' @description Node label permutation is one of the methods used to build null models.
#' @author Sebastian Sosa, Ivan Puga-Gonzalez.

#' @references Whitehead, H. A. L. (1997). Analysing animal social structure. Animal behaviour, 53(5), 1053-1067.
#' @references Sosa, S. (2018). Social Network Analysis, \emph{in}: Encyclopedia of Animal Cognition and Behavior. Springer.

#' @keywords internal


perm.nodeLabel <- function(df, labels, nperm, progress = T) {
  if (is.data.frame(df) == FALSE) {
    stop("Argument df is not a data frame")
  }
  col.id <- df.col.findId(df, labels)
  result <- perm_nodeLabels(df, label = col.id, nperm, progress = progress)
  return(result)
}
