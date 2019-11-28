# Copyright (C) 2018  Sebastian Sosa, Ivan Puga-Gonzalez, Hu Feng He, Xiaohua Xie, Cédric Sueur
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

#' @title Imports data frames
#' @description  Imports all current directory files in the format of a list of data frames.
#' @param header a logical value indicating whether the file contains the names of the variables as its first line. If missing, the value is determined from the file format: header is set to TRUE if and only if the first row contains one fewer field than the number of columns.
#' @param sep the field separator character. Values in each line of the file are separated by this character. If sep = "" (the default for read.table) the separator is ‘white space’, that is one or more spaces, tabs, newlines or carriage returns.
#' @param quote the set of quoting characters. To disable quoting altogether, use quote = "". See scan for the behaviour on quotes embedded in quotes. Quoting is only considered for columns read as character, which is all of them unless colClasses is specified.
#' @param dec	the character used in the file for decimal points.
#' @param fill logical If TRUE then in case the rows have unequal lengths, blank fields are implicitly added. See ‘Details’.
#' @param comment.char a character vector of length 1 containing a single character or an empty string. Use "" to turn off the interpretation of comments altogether.
#' @param ... Further arguments to be passed to \code{\link{read.csv}}.
#' #' @return a single data frame or a list of data frames according to the number of files in the selected folder.
#' @details  After selecting the directory, this function returns a single data frame if there is only one file in the folder or a list of data frames in the same order as in the original folder if there are several files in the folder.
#' by default it considers that the data frame has header= TRUE and row names= 1.
#' @author Sebastian Sosa, Ivan Puga-Gonzalez.
#' @seealso \code{read.csv}

import.df <- function(header = TRUE, sep = ",", quote = "\"", dec = ".",
                      fill = TRUE, comment.char = "", ...) {
  # Extract files names----------------------
  files <- list.files(getwd())

  # If their is no file in the folder----------------------
  if (length(files) == 0) {
    stop("Your repertory is emtpy")
  }

  # If just one file in the folder then use read.csv----------------------
  if (length(files) > 1) {
    DF <- lapply(files, read.csv, header = header, sep = sep, quote = quote, dec = dec, fill = fill, comment.char = comment.char, ...)
    files <- gsub(".csv", "", files)
    names(DF) <- files
  }

  # Else apply read.csv for all files and store them in a list of data frames----------------------
  if (length(files) == 1) {
    DF <- read.csv(file = files[1], header = header, sep = sep, quote = quote, dec = dec, fill = fill, comment.char = comment.char, ...)
    print("You could have used the function read.csv")
  }

  return(DF)
}
