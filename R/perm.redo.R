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

#' @title Repeats a permutation according to random factors
#' @description Performs node label permutations for GLMM by intra random factor label permutations.
#' @param df a list of data frame with each data frame representing a subset of the random factors (e.g. groups and/or periods)
#' @param labels a numeric or character vector representing the labels to permute.
#' @param ctrl a numeric or character vector representing one or more columns that will be use as control facors.
#' @return a data frame in which the data frames of the list are merged, each data frame being permuted according to the input argument labels.
#' @details This is a function to repeat a permutation according to random factors. It allows to handle GLMM warnings or errors that may appear due to the permutation approach.
#' @author Sebastian Sosa, Ivan Puga-Gonzalez.
#' @keywords internal
perm.redo <- function(df, labels, ctrl) {
  # Split df according to the control
  col.ctrl <- df.col.findId(df, ctrl)
  if (length(col.ctrl) > 1) {
    df$control <- apply(df[, col.ctrl], 1, paste, collapse = "_")
  }
  else {
    df$control <- df[, col.ctrl]
  }
  ldf <- split(df, df$control)

  # Permutation through the list
  col.id <- df.col.findId(df, labels)
  perm <- lapply(ldf, function(x, col.id) {
    r <- perm_nodeLabels(df = x, nperm = 1, label = col.id, progress = F)[[2]]
  }, col.id)

  # Merge the list permuted
  result <- do.call("rbind", perm)
  result <- result[, -c(max(ncol(result)))]
  return(result)
}
