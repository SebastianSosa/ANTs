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

#' @title Matrix binarization.
#' @description Convert matrix element higher to one in zeros.
#' @param M a data frame to know the total size of the concatenated vector.
#' @return A matrix.
#' @author Sebastian Sosa, Ivan Puga-Gonzalez.
#' @keywords internal
mat.binaryzation <- function(M) {
  m <- mat_binaryzation(M)
  attributes(m) <- attributes(M)
  return(m)
}
