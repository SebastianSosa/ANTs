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

#' @title Function for cumulative permutations for symmetric behaviour through focal sampling
#' @description Perform cumulative data stream permutations for scan sampling. Is the core function for the permutations.
#' @keywords internal
redo.ds.focal.cum <- function(df, focal, alters, ctrl, nperm, method) {

  # Extracting permutations informations
  col.ctrl <- df.col.findId(df, ctrl)
  col.alters <- df.col.findId(df, alters)
  col.focal <- df.col.findId(df, focal)
  ctrl <- c(col.ctrl, col.focal)
  df <- df.ctrlFactor(df, control = ctrl)
  df$control <- as.factor(df$control)
  focalids <- unique(df$control)
  Vecids <- unique(c(as.character(df[, col.alters]), as.character(df[, col.focal])))
  group_scan <- unique(df[, ncol(df)])

  # Compute GBI
  GBI2 <- df_to_gbi(df, ncol(df), col.focal, Vecids, group_scan)

  GBI <- df_to_gbi(df, ncol(df), col.alters, Vecids, group_scan)

  # Permute gbi
  r <- redo_perm_dataStream1_focal(M = GBI, M2 = GBI2, nperm = nperm, method = method)

  colnames(r[[1]]) <- Vecids
  rownames(r[[1]]) <- group_scan

  colnames(r[[2]]) <- Vecids
  rownames(r[[2]]) <- group_scan

  colnames(r[[3]]) <- Vecids
  rownames(r[[3]]) <- Vecids

  names(r) <- c("GBI", "GBI2", "MAT")

  return(r)
}
