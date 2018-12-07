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

#' @title Imports matrices
#' @description  Imports all current directory filesin the format of a list of matrices.
#' @param header a logical value indicating whether the file contains the names of the variables as its first line. If missing, the value is determined from the file format: header is set to TRUE if and only if the first row contains one fewer field than the number of columns.
#' @param row.names a vector of row names. This can be a vector giving the actual row names, or a single number giving the column of the table which contains the row names, or character string giving the name of the table column containing the row names.
#' If there is a header and the first row contains one fewer field than the number of columns, the first column in the input is used for the row names. Otherwise if row.names is missing, the rows are numbered.
#' Using row.names = NULL forces row numbering. Missing or NULL row.names generate row names that are considered to be ‘automatic’ (and not preserved by as.matrix).
#' @param sep the field separator character. Values on each line of the file are separated by this character. If sep = "" (the default for read.table) the separator is ‘white space’, that is one or more spaces, tabs, newlines or carriage returns.
#' @param ... Further arguments to be passed to \code{read.csv}.
#' @details  returns a single matrix if there is only one file in the folder or a list of matrices in the same order as in the original folder.
#' By default, it considers that the matrix has header and row names=1.
#' It also orders the matrix according to row names.
#' @author Sebastian Sosa, Ivan Puga-Gonzalez

import.mat <- function(header = TRUE, sep = ",", row.names = 1, ...) {
  # Extract files names----------------------
  files <- list.files(getwd())

  # If their is no file in the folder----------------------
  if (length(files) == 0) {
    stop("Your repertory is empty")
  }

  # If just one file in the folder then use as.matrix----------------------
  if (length(files) > 1) {
    M <- lapply(files, function(x, header, sep, row.names) {
      r <- as.matrix(read.csv(file = x, header = header, sep = sep, row.names = row.names))
    }, header = header, sep = sep, row.names = row.names)
    files <- gsub(".csv", "", files)
    names(M) <- files
  }

  # Else apply read.csv for all files and store them in a list of matrices----------------------
  if (length(files) == 1) {
    M <- as.matrix(read.csv(file = files[1], header = header, sep = sep, row.names = row.names))
  }
  return(M)
}
