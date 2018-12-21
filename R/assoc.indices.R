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

#' @title Association indices
#' @description Compute several association indices.
#' @param gbi  a group by individual matrix.
#' @param index a string indicating the association index to compute:
#' \itemize{
#' \item 'sri' for Simple ratio index: \eqn{x/x+yAB+yA+yB}
#' \item 'hw' for Half-weight index: \eqn{x/x+yAB+1/2(yA+yB)}
#' \item 'sr' for Square root index:\eqn{x/sqr((x+yAB+yA)(x+yAB+yB))}
#' }
#' @return A square matrix of individual association indices.
#' @author Sebastian Sosa, Ivan Puga-Gonzalez
#' @references Whitehead, H. A. L. (1997). Analysing animal social structure. Animal behaviour, 53(5), 1053-1067.
#' @examples
#' sim.gbi
#' assoc.indices(gbi=sim.gbi,index='sri')

assoc.indices <- function(gbi, index = "sri") {
  result <- assoc_mat(Mgbi = gbi, method = index)
  colnames(result)=colnames(gbi)
  rownames(result)=colnames(gbi)
  return(result)
}
