# Copyright (C) 2018  Sebastian Sosa, Ivan Puga-Gonzalez, Hu Feng He,Peng Zhang, Xiaohua Xie, Cédric Sueur
#
# This file is part of Animal Network Toolkit (ANT).
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

#' @title Converts Socprog data frame
#' @description Converts socprog 'Group mode' data frame (one scan per line with several individuals observed during the scan)
#' @param df a 'dyadic' or 'group' data frame socprog format
#' @param id an integer or string indicating the column of ids
#' @param scan an integer or string vector indicating the column of the scans
#' @param sep a character indicating the type of sepration between individuals inside one scan
#' @author Sebastian Sosa, Ivan Puga-Gonzalez
#' @examples
#' head(sim.socprog)
#' convert.socprog(sim.socprog,id=3,scan=c(1,2),sep=";")

convert.socprog <- function(df, id, scan, sep = ";") {
  col.id <- df.col.findId(df, id)
  df <- df.ctrlFactor(df, scan)
  col.scan <- ncol(df)
  r <- apply(df, 1, function(x, col.id, col.scan, sep) {
    ID <- strsplit(x[col.id], split = sep)
    scan <- rep(x[col.scan], length(ID))
    return(data.frame(scan, ID, row.names = NULL))
  }, col.id, col.scan, sep)
  return(do.call("rbind", r))
}
