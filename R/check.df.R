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

#' @title Data frame ANT check
#' @description Check that the argument is a data frame or a list of data frames
#' @author Sebastian Sosa, Ivan Puga-Gonzalez
#' @keywords internal
check.df <- function(df) {
  ## df is a single dataframe
  if (is.data.frame(df)) {
    return("df ok")
  }
  ## df is a list of dataframes
  if (!is.data.frame(df) & is.list(df)) {
    if (all(unlist(lapply(df, is.data.frame)))) {
      return("df list ok")
    }
    else {
      stop("Argument df is not a valid ANT object.", "\n")
    }
  }
  ## df is a matrix, stop
  if (is.matrix(df)) {
    stop("Argument df is a matrix, not a data frame.", "\n")
  }
  ## df is a vector, stop
  if (is.vector(df)) {
    stop("Argument df is a vector, not a data frame.", "\n")
  }
}
