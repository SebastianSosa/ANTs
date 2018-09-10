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

#' @title Converts a data frame of individual associations into a group by individual matrix.
#' @description Converts a data frame of individual associations into a group by individual matrix.
#' @param df a data frame in which to include a control column.
#' @param scan a numeric or character vector representing one or more columns used as scan factors.
#' @param id a numeric or character vector indicating the column holding ids of individuals.
#' @return The same data frame input with an extra column named 'control', representing the control factors.
#' @details Control factors are used in permutation approaches to constrain their permutations.
#' @author Sebastian Sosa, Ivan Puga-Gonzalez.
#' @keywords internal
perm.dataStream.focal <- function(df, focal, scan, alters, nperm, progress = T, method = "sri") {
  col.scan <- df.col.findId(df, scan)
  col.alters <- df.col.findId(df, alters)
  col.focal <- df.col.findId(df, focal)
  ctrl <- c(col.scan, col.focal)
  df <- df.ctrlFactor(df, control = ctrl)
  df$control <- as.factor(df$control)

  focalids <- unique(df$control)
  Vecids <- unique(c(as.character(df[, col.alters]), as.character(df[, col.focal])))
  group_scan <- unique(df[, ncol(df)])

  GBI2 <- df_to_gbi(df, ncol(df), col.focal, Vecids, group_scan)

  GBI <- df_to_gbi(df, ncol(df), col.alters, Vecids, group_scan)

  result <- perm_dataStream1_focal(GBI, GBI2, nperm = nperm, progress = progress, method = method)

  result <- lapply(seq_along(result), function(x, Vecids, i) {
    colnames(x[[i]]) <- Vecids
    rownames(x[[i]]) <- Vecids
    attr(x[[i]], "permutation") <- i
    return(x[[i]])
  }, x = result, Vecids = Vecids)

  return(result)
}
