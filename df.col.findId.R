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

#' @title Finds a data frame index
#' @description Finds the data frame index of a column from the name of the column or his index.
#' @param df a data frame in which to find the index of a specific column(s).
#' @param label_name a character or numeric vector indicating the column name or index respectively.
#' @return an numeric vector corresponding to the column index that matches argument label_name.
#' @details This function allows the user to select on or several columns according to their name(s) or their index(es).
#' @author Sebastian Sosa, Ivan Puga-Gonzalez.
#' @keywords internal

df.col.findId <- function(df, label_name) {
  if (!is.character(label_name) & !is.numeric(label_name)) {
    stop("Argument label_name is not a character or a numeric vector.")
  }
  if (is.character(label_name) == T) {
    if (all(!is.na(label_name))) {
      col.id <- match(label_name, colnames(df))
    }
    else {
      stop("Argument label_name doesn't match any column name.")
    }
  }
  else {
    if (length(label_name) <= ncol(df)) {
      col.id <- label_name
    }
    else {
      stop("Argument label_name is out of bound.")
    }
  }
  return(col.id)
}
