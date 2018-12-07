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

#' @title Create a matrix of time of observation per dyades
#' @description Creat a a matrix of time of observation per dyades in order to control for time of observation
#' @param vec a numeric vector
#' @author Sebastian Sosa, Ivan Puga-Gonzalez
#' @keywords internal
time.heterogeneity <- function(vec) {
  n <- length(vec)
  m <- matrix(rep(0, n * n), ncol = n, nrow = n)
  for (a in 1:length(vec)) {
    m[a, ] <- vec + vec[a]
    m[a, a] <- 0
  }
  return(m)
}
