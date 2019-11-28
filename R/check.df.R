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

#' @title Data frame ANT check
#' @description Check that the argument is a data frame or a list of data frames
#' @author Sebastian Sosa, Ivan Puga-Gonzalez
#' @keywords internal
check.df <- function(df) {
  # Check if argument df is a single dataframe----------------------
  if (is.data.frame(df)) {
    return("df ok")
  }

  # Check if argument df is a list of dataframes----------------------
  if (!is.data.frame(df) & is.list(df)) {
    if (all(unlist(lapply(df, is.data.frame)))) {
      return("df list ok")
    }
    else {
      stop("Argument df is not a list of data frames.", "\n")
    }
  }
  else{stop("Argument df is not a data frame or a list of data frames")}
}
