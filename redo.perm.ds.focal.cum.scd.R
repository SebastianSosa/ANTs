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

redo.perm.ds.focal.cum.scd <- function(GBI, GBI2, method, nperm) {
  r <- redo_perm_dataStream1_focal(GBI, GBI2, nperm = nperm, method = method) ### I PUT LIST_GBI, IT WAS RETURNING TO NO OBJECT!!!
  colnames(r[[1]]) <- colnames(GBI)
  rownames(r[[1]]) <- rownames(GBI)

  colnames(r[[2]]) <- colnames(GBI)
  rownames(r[[2]]) <- rownames(GBI)

  colnames(r[[3]]) <- colnames(GBI)
  rownames(r[[3]]) <- colnames(GBI)

  names(r) <- c("GBI", "GBI2", "MAT")

  return(r)
}
