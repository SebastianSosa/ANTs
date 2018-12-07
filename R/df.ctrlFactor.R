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

#' @title Creates a column that merges multiple columns
#' @description Creates a new column in the input data frame corresponding to the combination of given control factors.
#' @param df a data frame.
#' @param control a numeric or character vector representing one or more columns that are combined into one column of control factors.
#' @return The input data frame with an additional column named 'control', representing the control factors.
#' @details Control factors are used in permutation approaches to constrain the permutation between those factors.
#' @author Sebastian Sosa, Ivan Puga-Gonzalez.
#' @keywords internal
df.ctrlFactor <- function(df, control) {
  # Find columns id(s)----------------------
  col.ctrl <- df.col.findId(df, control)

  # Check number of columns to collapse----------------------
  if (length(control) == 1) {
    df$control <- df[, col.ctrl]
  }

  # Collapse columns----------------------
  else {
    df$control <- apply(df[, col.ctrl ], 1, paste, collapse = "_")
  }
  
  return(df)
}
