# Copyright (C) 2018  Sebastian Sosa, Ivan Puga-Gonzalez, Hu Feng He,Peng Zhang, Xiaohua Xie, Cédric Sueur
#
# This file is part of Animal Network Toolkit Software (ANTs).
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

#' @title Data frame to GBI.
#' @description Converts a data frame of individual associations into a group by individual matrix.
#' @param df a data frame of individual associations.
#' @param scan a numeric or character vector representing one or more columns used as scan factors.
#' @param id a numeric or character vector indicating the column holding ids of individuals.
#' @return A group by individual matrix.
#' @details Several association indices are coputed on GBI.
#' @author Sebastian Sosa, Ivan Puga-Gonzalez.
#' @seealso \code{assoc.indices}
#' @examples
#' head(sim.grp)
#' df.to.gbi(sim.grp,scan=c('location','time'),id='ID')

df.to.gbi <- function(df, scan, id) {
  # Find columns ids  corresponding to individuals----------------------
  col.id <- df.col.findId(df, id)

  # Check if argument scan correspond to multiples columns or to a single one----------------------
  if (length(scan > 1)) {
    # Find id columns correspondign to argment scan----------------------
    df <- df.ctrlFactor(df, scan)
    # Creat new column with a fusion of thos scan columns----------------------
    col.scan <- df.col.findId(df, "control")
  }

  else {
    col.scan <- df.col.findId(df, scan)
  }
  # Find all unique individuals----------------------
  Vecids <- unique(df[, id])

  # Find all unique scan----------------------
  group_scan <- unique(df[, col.scan])

  # Convert data frames to a matrix of Group By Individuals (GBI)----------------------
  GBI <- df_to_gbi(df, col.scan, col.id, Vecids, group_scan)
  GBI
}
